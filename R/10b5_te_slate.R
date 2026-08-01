# R/10b5_te_slate.R
# Step 10b (part 4): TE player slate -- clone of 10b3 for the TE layer.
# Same design and validation gate as 10b2/10b3 (see 10b2 header):
# next-value carry-forward of the frozen 12a rolling logic, priors
# recomputed from saved raw plays, exact-match gate against the frozen
# table on hindcast weeks (max |diff| < 1e-9 or STOP).
# TE-specific vs WR: defense split at 7 air yards (already baked into
# data/te_plays.rds play_cat); adds wt_tgt_per_snap role feature built on
# the snaps table with zero-target blocking games filled as 0 (12a step 8);
# draft meta filtered to position == "TE".
#
# Usage: Rscript R/10b5_te_slate.R [season] [week]   (default 2025 15)

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10b_roster_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L

ROLLING_WINDOW     <- 5L
DECAY_RATE         <- 0.85
DEF_WINDOW         <- 6L
FALLBACK_MIN_GAMES <- 3L
ANCHOR_SEASONS     <- 2013L:2025L
PREDICTION_SEASONS <- 2014L:2026L
MIN_PRIOR_OPP      <- 10L

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
next_roll <- function(x, window, decay) {
  roll_wt_mean_prior(c(x, NA_real_), window, decay)[length(x) + 1L]
}

cli_h1("Step 10b5: WR player slate -- {TARGET_SEASON} week {TARGET_WEEK}")

# ===========================================================================
# 1. LOAD + TARGET GAMES
# ===========================================================================

cli_h1("Step 1: Load artifacts and target-week games")

te_outcomes <- readRDS("data/te_outcomes.rds") |> filter(!is.na(player_id))
te_plays    <- readRDS("data/te_plays.rds")
ft          <- readRDS("data/te_feature_table.rds")

games <- nflreadr::load_schedules(seasons = TARGET_SEASON) |>
  filter(game_type == "REG", week == TARGET_WEEK) |>
  select(game_id, season, week, home_team, away_team)
games_long <- bind_rows(
  games |> transmute(game_id, season, week, posteam = home_team, defteam = away_team),
  games |> transmute(game_id, season, week, posteam = away_team, defteam = home_team)
)

hindcast <- any(ft$season == TARGET_SEASON & ft$week == TARGET_WEEK)
cli_alert_info("{nrow(games)} games | mode: {if (hindcast) 'HINDCAST (gate available)' else 'FUTURE'}")

hist_outcomes <- te_outcomes |>
  filter(season < TARGET_SEASON |
         (season == TARGET_SEASON & week < TARGET_WEEK))
season_hist <- hist_outcomes |> filter(season == TARGET_SEASON)

# ===========================================================================
# 2. ROSTER (+ override hook)
# ===========================================================================

cli_h1("Step 2: Slate roster")

roster_exante <- build_exante_roster("TE", TARGET_SEASON, TARGET_WEEK,
                                     games_long, season_hist,
                                     known_ids = unique(te_outcomes$player_id))

# NA player_id guard kept for symmetry with 10b3; the TE table is built
# with the NA-receiver filter (12a) so no pseudo-rows exist to exclude.
force_future <- Sys.getenv("FORCE_FUTURE", "0") == "1"
roster <- if (hindcast && !force_future) {
  ft |> filter(season == TARGET_SEASON, week == TARGET_WEEK, !is.na(player_id)) |>
    select(player_id, posteam, defteam, game_id)
} else {
  roster_exante |> select(player_id, posteam, defteam, game_id, source)
}
cli_alert_success("Slate roster: {nrow(roster)} players (ex-ante: {nrow(roster_exante)}; cold adds: {sum(roster_exante$source == 'roster_cold')})")

# ===========================================================================
# 3. PLAYER ROLLING + SEASON CONSTANTS
# ===========================================================================

cli_h1("Step 3: Player rolling features + priors")

player_roll <- season_hist |>
  semi_join(roster, by = "player_id") |>
  arrange(player_id, week) |>
  group_by(player_id) |>
  summarise(
    rolling_epa_per_opp     = next_roll(epa_per_opp_obs,          ROLLING_WINDOW, DECAY_RATE),
    wt_target_share         = next_roll(target_share_obs,         ROLLING_WINDOW, DECAY_RATE),
    wt_air_yards_share      = next_roll(air_yards_share_obs,      ROLLING_WINDOW, DECAY_RATE),
    wt_air_yards_per_target = next_roll(air_yards_per_target_obs, ROLLING_WINDOW, DECAY_RATE),
    .groups = "drop"
  )

rosters_all <- nflreadr::load_rosters(
  seasons = sort(unique(c(PREDICTION_SEASONS, TARGET_SEASON))))
draft_raw   <- nflreadr::load_draft_picks()

draft_meta <- draft_raw |>
  filter(position == "TE", !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, draft_round = round)

te_draft <- rosters_all |>
  filter(position == "TE") |>
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

prior_stats <- te_plays |>
  filter(season == TARGET_SEASON - 1L) |>
  group_by(player_id) |>
  summarise(prior_opp = n(), prior_epa = sum(epa, na.rm = TRUE), .groups = "drop") |>
  mutate(prior_epa_per_opp = if_else(prior_opp >= MIN_PRIOR_OPP,
                                     prior_epa / prior_opp, NA_real_))

tier_prior <- prior_stats |>
  left_join(te_draft, by = "player_id") |>
  filter(!is.na(prior_epa_per_opp), !is.na(draft_tier)) |>
  group_by(draft_tier) |>
  summarise(tier_epa_per_opp = median(prior_epa_per_opp, na.rm = TRUE), .groups = "drop")

pos_prior <- median(prior_stats$prior_epa_per_opp[!is.na(prior_stats$prior_epa_per_opp)],
                    na.rm = TRUE)

season_const <- te_draft |>
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

team_roll <- season_hist |>
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

snaps_raw <- load_season_or_empty(nflreadr::load_snap_counts, TARGET_SEASON)
snap_pct_divisor <- if (max(snaps_raw$offense_pct, na.rm = TRUE) > 1.5) 100 else 1

# TE role feature (mirrors 12a step 8): targets-per-snap built on the SNAPS
# table with zero-target (blocking) games filled as 0. Targets come from the
# outcome history; a snap game with no outcome row is a 0-target game.
game_targets_hist <- season_hist |> select(player_id, week, targets)

snap_roll <- snaps_raw |>
  filter(game_type == "REG", week < TARGET_WEEK,
         !is.na(pfr_player_id), !is.na(offense_pct)) |>
  mutate(snap_pct = offense_pct / snap_pct_divisor) |>
  left_join(id_xwalk, by = c("pfr_player_id" = "pfr_id")) |>
  filter(!is.na(gsis_id)) |>
  left_join(game_targets_hist, by = c("gsis_id" = "player_id", "week")) |>
  mutate(
    targets_filled   = coalesce(targets, 0L),
    tgt_per_snap_obs = if_else(offense_snaps > 0L,
                               targets_filled / offense_snaps,
                               NA_real_)
  ) |>
  arrange(gsis_id, week) |>
  group_by(gsis_id) |>
  summarise(wt_snap_share   = next_roll(snap_pct,         ROLLING_WINDOW, DECAY_RATE),
            wt_tgt_per_snap = next_roll(tgt_per_snap_obs, ROLLING_WINDOW, DECAY_RATE),
            .groups = "drop")

# ===========================================================================
# 5. DEFENSE CHAIN (frozen 04a step-9 logic from saved raw plays)
# ===========================================================================

cli_h1("Step 5: Defense features (recomputed from data/te_plays.rds)")

game_teams <- te_plays |> distinct(game_id, season, week, posteam, defteam)

def_per_game <- te_plays |>
  group_by(game_id, season, week, defteam, play_cat) |>
  summarise(epa_sum = sum(epa, na.rm = TRUE), n_plays = n(), .groups = "drop") |>
  mutate(epa_per_play = epa_sum / n_plays)

off_per_game <- te_plays |>
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
for (cat in c("short_pass", "deep_pass")) {
  if (!cat %in% names(def_wide)) def_wide[[cat]] <- 0
}

# All slated defenses get a row (week-1/no-history teams fall through to
# the prior-season fallback instead of NA features)
def_next <- games_long |>
  distinct(defteam) |>
  left_join(
    def_wide |>
      arrange(defteam, week) |>
      group_by(defteam) |>
      summarise(
        games_played_so_far    = n(),
        def_short_pass_epa_adj = next_roll(short_pass, DEF_WINDOW, DECAY_RATE),
        def_deep_pass_epa_adj  = next_roll(deep_pass,  DEF_WINDOW, DECAY_RATE),
        .groups = "drop"
      ),
    by = "defteam"
  ) |>
  mutate(games_played_so_far = coalesce(games_played_so_far, 0L)) |>
  left_join(def_prior |> filter(prediction_season == TARGET_SEASON), by = "defteam") |>
  mutate(
    def_used_fallback = games_played_so_far < FALLBACK_MIN_GAMES,
    def_short_pass_epa_adj = if_else(def_used_fallback, coalesce(ps_short_pass, lg_scalar("short_pass")), def_short_pass_epa_adj),
    def_deep_pass_epa_adj  = if_else(def_used_fallback, coalesce(ps_deep_pass,  lg_scalar("deep_pass")),  def_deep_pass_epa_adj)
  ) |>
  select(defteam, games_played_so_far, def_used_fallback,
         def_short_pass_epa_adj, def_deep_pass_epa_adj)

# ===========================================================================
# 6. ASSEMBLE + INJURIES
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
  mutate(season = TARGET_SEASON, week = TARGET_WEEK) |>
  left_join(vegas_slate_lines(games_long, TARGET_SEASON, hindcast && !force_future),
            by = c("game_id", "posteam"))

inj <- tryCatch(
  load_season_or_empty(nflreadr::load_injuries, TARGET_SEASON) |>
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
# 7. VALIDATION GATE (hindcast only)
# ===========================================================================

if (hindcast) {
  cli_h1("Step 7: VALIDATION GATE -- slate vs frozen table, exact match")

  truth <- ft |> filter(season == TARGET_SEASON, week == TARGET_WEEK, !is.na(player_id))
  feat_cols <- c("prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp",
                 "form_residual", "wt_target_share", "wt_air_yards_share",
                 "wt_air_yards_per_target", "wt_snap_share", "wt_tgt_per_snap",
                 "wt_team_total_plays",
                 "def_short_pass_epa_adj", "def_deep_pass_epa_adj", "games_played_so_far")

  cmp <- truth |>
    select(player_id, all_of(feat_cols), def_used_fallback, is_cold_start) |>
    inner_join(slate |> select(player_id, all_of(feat_cols),
                               def_used_fallback, is_cold_start),
               by = "player_id", suffix = c("_truth", "_slate"))

  gate <- map_dfr(feat_cols, function(cl) {
    a <- cmp[[paste0(cl, "_truth")]]; b <- cmp[[paste0(cl, "_slate")]]
    both_na <- is.na(a) & is.na(b)
    d <- abs(a - b)
    tibble(feature = cl, n = nrow(cmp),
           na_mismatch = sum(is.na(a) != is.na(b)),
           max_abs_diff = if (all(both_na)) 0 else max(d, na.rm = TRUE))
  })
  print(as.data.frame(gate), digits = 4)

  worst  <- max(gate$max_abs_diff, na.rm = TRUE)
  na_bad <- sum(gate$na_mismatch)
  flag_bad <- sum(cmp$def_used_fallback_truth != cmp$def_used_fallback_slate) +
              sum(cmp$is_cold_start_truth != cmp$is_cold_start_slate)
  if (worst < 1e-9 && na_bad == 0 && flag_bad == 0) {
    cli_alert_success("GATE PASSED: max |diff| = {format(worst, scientific = TRUE)} across {nrow(cmp)} players x {length(feat_cols)} features")
  } else {
    cli_abort("GATE FAILED: max |diff| = {worst}, NA mismatches = {na_bad}, flag mismatches = {flag_bad} -- DO NOT PROCEED")
  }
}

# ===========================================================================
# 8. SAVE
# ===========================================================================

cli_h1("Step 8: Save")

out_file <- sprintf("output/10b5_te_slate_%d_w%02d.csv", TARGET_SEASON, TARGET_WEEK)
readr::write_csv(slate, out_file)
cli_alert_success("{out_file} ({nrow(slate)} rows)")

cli_h1("Step 10b5 complete (WR)")
