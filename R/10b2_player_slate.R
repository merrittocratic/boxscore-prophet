# R/10b2_player_slate.R
# Step 10b (part 2): Player slate -- ex-ante feature rows for a target week.
# RB implementation; WR/QB clones follow once the validation gate passes.
#
# DESIGN: every feature in the frozen layer is strictly backward-looking,
# so a week-W row is the "next value" of each rolling series computed from
# games through W-1. This script rebuilds those next values STANDALONE
# (frozen feature scripts untouched), using:
#   - the frozen rolling helpers copied VERBATIM (roll_wt_mean_prior etc.)
#   - season-constant features (baseline/prior/draft_tier/is_cold_start)
#     LIFTED from the player's most recent frozen-table row -- constant per
#     player-season, so lifting is drift-free by construction
#   - the defense chain recomputed from saved raw inputs (data/rb_plays.rds)
#     with the exact step-9 logic of build_rb_feature_layer.R
#
# VALIDATION GATE (the whole point): the frozen table's week-W rows were
# themselves computed ex-ante, so hindcasting a played week must reproduce
# them EXACTLY. Gate: max |diff| < 1e-9 across all feature columns for all
# shared player rows. A mismatch is a STOP (clone drifted from frozen
# logic), not a tolerance to widen.
#
# MODES:
#   hindcast (target week exists in feature table): build from priors,
#     run the gate, emit slate + comparison report
#   future (target week beyond table): same machinery, no gate available;
#     roster = ex-ante rule (played this season, team plays this week)
#     + cold-start additions from current rosters -- future mode gains
#     trust from the hindcast gate passing
#
# Slate extras: injury report join (load_injuries: report + practice
# status -- context columns; the DNP-progression FEATURE family is ladder
# rung 1, separate build), game-slate weather join (10b part 1 output),
# and the depth-chart override hook (data/overrides/depth_overrides.csv:
# player_id, action add/drop, note -- Earnest's X-monitoring feeds this).
#
# Usage: Rscript R/10b2_player_slate.R [season] [week]
#   Default 2025 15 (hindcast validation run).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2025L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L

# Frozen constants -- copied from build_rb_feature_layer.R v1.5
ROLLING_WINDOW     <- 5L
DECAY_RATE         <- 0.85
DEF_WINDOW         <- 6L
FALLBACK_MIN_GAMES <- 3L
ANCHOR_SEASONS     <- 2013L:2024L
PREDICTION_SEASONS <- 2014L:2025L

# Frozen helpers -- copied VERBATIM from build_rb_feature_layer.R
exp_weights <- function(n, decay = DECAY_RATE) {
  if (n == 0L) return(numeric(0))
  w <- decay ^ seq(n - 1L, 0L)
  w / sum(w)
}
wt_mean <- function(x, w) {
  if (length(x) == 0L || all(is.na(x))) return(NA_real_)
  valid <- !is.na(x)
  if (!any(valid)) return(NA_real_)
  sum(x[valid] * w[valid]) / sum(w[valid])
}
roll_wt_mean_prior <- function(x, window = ROLLING_WINDOW, decay = DECAY_RATE) {
  n   <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i == 1L) next
    vals   <- x[max(1L, i - window):(i - 1L)]
    out[i] <- wt_mean(vals, exp_weights(length(vals), decay))
  }
  out
}

# Next value of a rolling series: what roll_wt_mean_prior would emit at
# position n+1. Appending a placeholder row reuses the frozen function
# unchanged rather than reimplementing its window arithmetic.
next_roll <- function(x, window, decay) {
  roll_wt_mean_prior(c(x, NA_real_), window, decay)[length(x) + 1L]
}

cli_h1("Step 10b (part 2): RB player slate -- {TARGET_SEASON} week {TARGET_WEEK}")

# ===========================================================================
# 1. LOAD FROZEN ARTIFACTS + TARGET-WEEK GAMES
# ===========================================================================

cli_h1("Step 1: Load artifacts and target-week games")

rb_outcomes <- readRDS("data/rb_outcomes.rds") |> filter(!is.na(player_id))
rb_plays    <- readRDS("data/rb_plays.rds")
ft          <- readRDS("data/rb_feature_table.rds")
def_saved   <- readRDS("data/def_rolling_final.rds")

games <- nflreadr::load_schedules(seasons = TARGET_SEASON) |>
  filter(game_type == "REG", week == TARGET_WEEK) |>
  select(game_id, season, week, home_team, away_team)
games_long <- bind_rows(
  games |> transmute(game_id, season, week, posteam = home_team, defteam = away_team),
  games |> transmute(game_id, season, week, posteam = away_team, defteam = home_team)
)

hindcast <- any(ft$season == TARGET_SEASON & ft$week == TARGET_WEEK)
cli_alert_info("{nrow(games)} games | mode: {if (hindcast) 'HINDCAST (gate available)' else 'FUTURE'}")

# History = strictly before the target week
hist_outcomes <- rb_outcomes |>
  filter(season < TARGET_SEASON |
         (season == TARGET_SEASON & week < TARGET_WEEK))
season_hist <- hist_outcomes |> filter(season == TARGET_SEASON)

# ===========================================================================
# 2. ROSTER
# ===========================================================================

cli_h1("Step 2: Slate roster")

# Ex-ante roster: every RB with a played game this season whose most recent
# team plays this week. Team = most recent posteam (mid-season trades track
# through outcomes; the override hook below catches breaking moves).
roster_exante <- season_hist |>
  arrange(player_id, week) |>
  group_by(player_id) |>
  summarise(posteam = last(posteam), .groups = "drop") |>
  inner_join(games_long, by = "posteam")

# Depth-chart override hook (Earnest): add players (e.g. practice-squad
# elevation, cold-start rookie) or drop players (e.g. announced inactive)
ov_file <- "data/overrides/depth_overrides.csv"
overrides <- if (file.exists(ov_file)) {
  readr::read_csv(ov_file, show_col_types = FALSE)
} else tibble(player_id = character(), action = character(), note = character())
if (nrow(overrides)) {
  cli_alert_info("Applying {nrow(overrides)} depth-chart override(s)")
  roster_exante <- roster_exante |> filter(!player_id %in% overrides$player_id[overrides$action == "drop"])
  # 'add' overrides must supply posteam in the CSV; joined to games_long
  adds <- overrides |> filter(action == "add") |>
    inner_join(games_long, by = "posteam")
  roster_exante <- bind_rows(roster_exante, adds |> select(any_of(names(roster_exante)))) |>
    distinct(player_id, .keep_all = TRUE)
}

# Hindcast gate roster: the players actually in the frozen table that week
roster <- if (hindcast) {
  ft |> filter(season == TARGET_SEASON, week == TARGET_WEEK) |>
    select(player_id, posteam, defteam, game_id)
} else {
  roster_exante |> select(player_id, posteam, defteam, game_id)
}
cli_alert_success("Slate roster: {nrow(roster)} players (ex-ante roster would be {nrow(roster_exante)})")

# ===========================================================================
# 3. PLAYER ROLLING FEATURES (next value from season history)
# ===========================================================================

cli_h1("Step 3: Player rolling features")

player_roll <- season_hist |>
  semi_join(roster, by = "player_id") |>
  arrange(player_id, week) |>
  group_by(player_id) |>
  summarise(
    rolling_epa_per_opp = next_roll(epa_per_opp_obs,  ROLLING_WINDOW, DECAY_RATE),
    wt_carry_share      = next_roll(carry_share_obs,  ROLLING_WINDOW, DECAY_RATE),
    wt_target_share     = next_roll(target_share_obs, ROLLING_WINDOW, DECAY_RATE),
    .groups = "drop"
  )

# Season-constant features RECOMPUTED exactly from saved raw plays (the
# table-lift shortcut left NAs for players whose games all fell under the
# 5-opp output floor -- the frozen script computes priors from unfiltered
# plays, so we do the same; step-6 logic of build_rb_feature_layer.R)
MIN_PRIOR_OPP <- 10L

rosters_all <- nflreadr::load_rosters(seasons = PREDICTION_SEASONS)
draft_raw   <- nflreadr::load_draft_picks()

draft_meta <- draft_raw |>
  filter(position %in% c("RB", "FB"), !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, draft_round = round)

rb_draft <- rosters_all |>
  filter(position == "RB") |>
  distinct(gsis_id) |>
  left_join(draft_meta, by = "gsis_id") |>
  mutate(draft_tier = case_when(
    is.na(draft_round) ~ "udfa",
    draft_round == 1   ~ "r1",
    draft_round <= 3   ~ "r2_3",
    draft_round <= 5   ~ "r4_5",
    TRUE               ~ "r6_udfa"
  )) |>
  select(player_id = gsis_id, draft_tier)

prior_stats <- rb_plays |>
  filter(season == TARGET_SEASON - 1L) |>
  group_by(player_id) |>
  summarise(prior_opp = n(), prior_epa = sum(epa, na.rm = TRUE), .groups = "drop") |>
  mutate(prior_epa_per_opp = if_else(prior_opp >= MIN_PRIOR_OPP,
                                     prior_epa / prior_opp, NA_real_))

tier_prior <- prior_stats |>
  left_join(rb_draft, by = "player_id") |>
  filter(!is.na(prior_epa_per_opp), !is.na(draft_tier)) |>
  group_by(draft_tier) |>
  summarise(tier_epa_per_opp = median(prior_epa_per_opp, na.rm = TRUE), .groups = "drop")

pos_prior <- median(prior_stats$prior_epa_per_opp[!is.na(prior_stats$prior_epa_per_opp)],
                    na.rm = TRUE)

season_const <- rb_draft |>
  left_join(prior_stats |> select(player_id, prior_epa_per_opp), by = "player_id") |>
  left_join(tier_prior, by = "draft_tier") |>
  mutate(
    is_cold_start        = is.na(prior_epa_per_opp),
    baseline_epa_per_opp = if_else(is_cold_start,
                                   coalesce(tier_epa_per_opp, pos_prior),
                                   prior_epa_per_opp)
  ) |>
  select(player_id, prior_epa_per_opp, baseline_epa_per_opp,
         is_cold_start, draft_tier)

player_names <- rosters_all |>
  filter(season == TARGET_SEASON, !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(player_id = gsis_id, player_name = full_name)

# ===========================================================================
# 4. TEAM + SNAP ROLLING
# ===========================================================================

cli_h1("Step 4: Team plays and snap-share rolling")

team_roll <- hist_outcomes |>
  filter(season == TARGET_SEASON) |>
  distinct(game_id, week, posteam, team_total_plays_obs) |>
  arrange(posteam, week) |>
  group_by(posteam) |>
  summarise(wt_team_total_plays = next_roll(team_total_plays_obs, ROLLING_WINDOW, DECAY_RATE),
            .groups = "drop")

id_xwalk <- rosters_all |>
  filter(!is.na(gsis_id), !is.na(pfr_id)) |>
  arrange(desc(season)) |>
  distinct(pfr_id, .keep_all = TRUE) |>
  select(gsis_id, pfr_id)

snaps_raw <- nflreadr::load_snap_counts(seasons = TARGET_SEASON)
snap_pct_divisor <- if (max(snaps_raw$offense_pct, na.rm = TRUE) > 1.5) 100 else 1
snap_roll <- snaps_raw |>
  filter(game_type == "REG", week < TARGET_WEEK,
         !is.na(pfr_player_id), !is.na(offense_pct)) |>
  mutate(snap_pct = offense_pct / snap_pct_divisor) |>
  left_join(id_xwalk, by = c("pfr_player_id" = "pfr_id")) |>
  filter(!is.na(gsis_id)) |>
  arrange(gsis_id, week) |>
  group_by(gsis_id) |>
  summarise(wt_snap_share = next_roll(snap_pct, ROLLING_WINDOW, DECAY_RATE),
            .groups = "drop")

# ===========================================================================
# 5. DEFENSE CHAIN (frozen step-9 logic, recomputed from saved raw plays)
# ===========================================================================

cli_h1("Step 5: Defense features (recomputed from data/rb_plays.rds)")

game_teams <- rb_plays |> distinct(game_id, season, week, posteam, defteam)

def_per_game <- rb_plays |>
  group_by(game_id, season, week, defteam, play_cat) |>
  summarise(epa_sum = sum(epa, na.rm = TRUE), n_plays = n(), .groups = "drop") |>
  mutate(epa_per_play = epa_sum / n_plays)

off_per_game <- rb_plays |>
  group_by(game_id, season, week, posteam, play_cat) |>
  summarise(off_epa = sum(epa, na.rm = TRUE), off_n = n(), .groups = "drop") |>
  mutate(off_epa_per_play = off_epa / off_n)

off_rolling_strength <- off_per_game |>
  arrange(posteam, play_cat, season, week) |>
  group_by(posteam, play_cat) |>
  mutate(rolling_off_strength = roll_wt_mean_prior(off_epa_per_play, DEF_WINDOW, DECAY_RATE)) |>
  ungroup() |>
  select(game_id, posteam, play_cat, rolling_off_strength)

lg_avg_off <- off_per_game |>
  group_by(play_cat) |>
  summarise(lg_avg = mean(off_epa_per_play, na.rm = TRUE), .groups = "drop")

def_adj <- def_per_game |>
  left_join(game_teams |> select(game_id, defteam, opponent = posteam) |> distinct(),
            by = c("game_id", "defteam")) |>
  left_join(off_rolling_strength, by = c("game_id", "opponent" = "posteam", "play_cat")) |>
  left_join(lg_avg_off, by = "play_cat") |>
  mutate(
    opp_strength     = coalesce(rolling_off_strength, lg_avg),
    adj_factor       = opp_strength - lg_avg,
    epa_per_play_adj = epa_per_play - adj_factor
  ) |>
  select(game_id, season, week, defteam, play_cat, epa_per_play_adj)

def_prior <- def_adj |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(defteam, season, play_cat) |>
  summarise(ps_epa = mean(epa_per_play_adj, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = play_cat, values_from = ps_epa, names_prefix = "ps_") |>
  mutate(prediction_season = season + 1L) |>
  select(defteam, prediction_season, starts_with("ps_"))

lg_scalar <- function(cat) {
  mean(def_adj$epa_per_play_adj[def_adj$play_cat == cat &
                                def_adj$season %in% ANCHOR_SEASONS], na.rm = TRUE)
}

def_wide <- def_adj |>
  filter(season == TARGET_SEASON, week < TARGET_WEEK) |>
  pivot_wider(names_from = play_cat, values_from = epa_per_play_adj,
              values_fill = list(epa_per_play_adj = 0))
for (cat in c("rush", "short_pass", "deep_pass")) {
  if (!cat %in% names(def_wide)) def_wide[[cat]] <- 0
}

def_next <- def_wide |>
  arrange(defteam, week) |>
  group_by(defteam) |>
  summarise(
    games_played_so_far    = n(),
    def_rush_epa_adj       = next_roll(rush,       DEF_WINDOW, DECAY_RATE),
    def_short_pass_epa_adj = next_roll(short_pass, DEF_WINDOW, DECAY_RATE),
    def_deep_pass_epa_adj  = next_roll(deep_pass,  DEF_WINDOW, DECAY_RATE),
    .groups = "drop"
  ) |>
  left_join(def_prior |> filter(prediction_season == TARGET_SEASON), by = "defteam") |>
  mutate(
    def_used_fallback = games_played_so_far < FALLBACK_MIN_GAMES,
    def_rush_epa_adj       = if_else(def_used_fallback, coalesce(ps_rush, lg_scalar("rush")), def_rush_epa_adj),
    def_short_pass_epa_adj = if_else(def_used_fallback, coalesce(ps_short_pass, lg_scalar("short_pass")), def_short_pass_epa_adj),
    def_deep_pass_epa_adj  = if_else(def_used_fallback, coalesce(ps_deep_pass, lg_scalar("deep_pass")), def_deep_pass_epa_adj)
  ) |>
  select(defteam, games_played_so_far, def_used_fallback,
         def_rush_epa_adj, def_short_pass_epa_adj, def_deep_pass_epa_adj)

# ===========================================================================
# 6. ASSEMBLE SLATE
# ===========================================================================

cli_h1("Step 6: Assemble slate rows")

slate <- roster |>
  left_join(player_roll,  by = "player_id") |>
  left_join(season_const, by = "player_id") |>
  left_join(player_names, by = "player_id") |>
  mutate(form_residual = rolling_epa_per_opp - baseline_epa_per_opp) |>
  left_join(team_roll, by = "posteam") |>
  left_join(snap_roll, by = c("player_id" = "gsis_id")) |>
  left_join(def_next,  by = "defteam") |>
  mutate(season = TARGET_SEASON, week = TARGET_WEEK)

# Injury context (report + practice status; the DNP-progression FEATURE
# family is ablation ladder rung 1, built separately)
inj <- tryCatch(
  nflreadr::load_injuries(seasons = TARGET_SEASON) |>
    filter(week == TARGET_WEEK) |>
    select(player_id = gsis_id, report_status, practice_status) |>
    distinct(player_id, .keep_all = TRUE),
  error = function(e) {
    cli_alert_warning("Injury data unavailable: {conditionMessage(e)}")
    tibble(player_id = character(), report_status = character(), practice_status = character())
  }
)
slate <- slate |> left_join(inj, by = "player_id")
cli_alert_success("Slate: {nrow(slate)} rows | injuries matched: {sum(!is.na(slate$report_status))}")

# ===========================================================================
# 7. VALIDATION GATE (hindcast only): exact match vs frozen table
# ===========================================================================

if (hindcast) {
  cli_h1("Step 7: VALIDATION GATE -- slate vs frozen table, exact match")

  truth <- ft |> filter(season == TARGET_SEASON, week == TARGET_WEEK)
  feat_cols <- c("prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp",
                 "form_residual", "wt_carry_share", "wt_target_share", "wt_snap_share",
                 "wt_team_total_plays", "def_rush_epa_adj", "def_short_pass_epa_adj",
                 "def_deep_pass_epa_adj", "games_played_so_far")

  cmp <- truth |>
    select(player_id, all_of(feat_cols), def_used_fallback, is_cold_start) |>
    inner_join(slate |> select(player_id, all_of(feat_cols),
                               def_used_fallback, is_cold_start),
               by = "player_id", suffix = c("_truth", "_slate"))

  gate <- map_dfr(feat_cols, function(cl) {
    a <- cmp[[paste0(cl, "_truth")]]; b <- cmp[[paste0(cl, "_slate")]]
    both_na <- is.na(a) & is.na(b)
    d <- abs(a - b)
    tibble(feature = cl,
           n = nrow(cmp),
           na_mismatch = sum(is.na(a) != is.na(b)),
           max_abs_diff = if (all(both_na)) 0 else max(d, na.rm = TRUE))
  })
  gate$flag_mismatch <- c(
    sum(cmp$def_used_fallback_truth != cmp$def_used_fallback_slate),
    rep(NA, nrow(gate) - 1)
  )
  print(as.data.frame(gate), digits = 4)

  worst  <- max(gate$max_abs_diff, na.rm = TRUE)
  na_bad <- sum(gate$na_mismatch)
  if (worst < 1e-9 && na_bad == 0) {
    cli_alert_success("GATE PASSED: max |diff| = {format(worst, scientific = TRUE)} across {nrow(cmp)} players x {length(feat_cols)} features")
  } else {
    cli_abort("GATE FAILED: max |diff| = {worst}, NA mismatches = {na_bad} -- carry-forward drifted from frozen logic, DO NOT PROCEED")
  }
}

# ===========================================================================
# 8. SAVE
# ===========================================================================

cli_h1("Step 8: Save")

out_file <- sprintf("output/10b2_rb_slate_%d_w%02d.csv", TARGET_SEASON, TARGET_WEEK)
readr::write_csv(slate, out_file)
cli_alert_success("{out_file} ({nrow(slate)} rows)")

cli_h1("Step 10b2 complete (RB) -- WR/QB clones next")
