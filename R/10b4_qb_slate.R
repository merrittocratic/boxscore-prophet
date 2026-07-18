# R/10b4_qb_slate.R
# Step 10b (part 2): QB player slate -- clone of 10b2/10b3 for the QB layer.
# Same design and validation gate (see 10b2 header): next-value carry-forward
# of the frozen 08a rolling logic, priors recomputed from saved raw plays,
# exact-match gate against the frozen table on hindcast weeks.
# QB-specific: pass + rush sides (kneels excluded upstream in the saved
# plays); pass priors gated at 100 anchor dropbacks; rush priors keyed to
# anchor GAMES (>= 6) and CARRIES (>= 20); team pass rate rolling; defense
# vector short/deep/rush where rush is measured vs ALL rushers -- raw
# inputs from data/qb_def_adj.rds (persisted by 08a for exactly this).
#
# Usage: Rscript R/10b4_qb_slate.R [season] [week]   (default 2025 15)

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2025L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L

ROLLING_WINDOW     <- 5L
DECAY_RATE         <- 0.85
DEF_WINDOW         <- 6L
FALLBACK_MIN_GAMES <- 3L
ANCHOR_SEASONS     <- 2013L:2024L
PREDICTION_SEASONS <- 2014L:2025L
MIN_PRIOR_DB       <- 100L
MIN_PRIOR_GAMES    <- 6L
MIN_PRIOR_CARRIES  <- 20L
DEF_CATS           <- c("short_pass", "deep_pass", "rush")

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

cli_h1("Step 10b4: QB player slate -- {TARGET_SEASON} week {TARGET_WEEK}")

# ===========================================================================
# 1. LOAD + TARGET GAMES
# ===========================================================================

cli_h1("Step 1: Load artifacts and target-week games")

qb_outcomes   <- readRDS("data/qb_outcomes.rds") |> filter(!is.na(player_id))
qb_pass_plays <- readRDS("data/qb_pass_plays.rds")
qb_rush_plays <- readRDS("data/qb_rush_plays.rds")
def_adj       <- readRDS("data/qb_def_adj.rds")
ft            <- readRDS("data/qb_feature_table.rds")

games <- nflreadr::load_schedules(seasons = TARGET_SEASON) |>
  filter(game_type == "REG", week == TARGET_WEEK) |>
  select(game_id, season, week, home_team, away_team)
games_long <- bind_rows(
  games |> transmute(game_id, season, week, posteam = home_team, defteam = away_team),
  games |> transmute(game_id, season, week, posteam = away_team, defteam = home_team)
)

hindcast <- any(ft$season == TARGET_SEASON & ft$week == TARGET_WEEK)
cli_alert_info("{nrow(games)} games | mode: {if (hindcast) 'HINDCAST (gate available)' else 'FUTURE'}")

season_hist <- qb_outcomes |>
  filter(season == TARGET_SEASON, week < TARGET_WEEK)

# ===========================================================================
# 2. ROSTER (+ override hook)
# ===========================================================================

cli_h1("Step 2: Slate roster")

roster_exante <- season_hist |>
  arrange(player_id, week) |>
  group_by(player_id) |>
  summarise(posteam = last(posteam), .groups = "drop") |>
  inner_join(games_long, by = "posteam")

ov_file <- "data/overrides/depth_overrides.csv"
overrides <- if (file.exists(ov_file)) {
  readr::read_csv(ov_file, show_col_types = FALSE)
} else tibble(player_id = character(), action = character(), note = character())
if (nrow(overrides)) {
  cli_alert_info("Applying {nrow(overrides)} depth-chart override(s)")
  roster_exante <- roster_exante |> filter(!player_id %in% overrides$player_id[overrides$action == "drop"])
  adds <- overrides |> filter(action == "add") |> inner_join(games_long, by = "posteam")
  roster_exante <- bind_rows(roster_exante, adds |> select(any_of(names(roster_exante)))) |>
    distinct(player_id, .keep_all = TRUE)
}

roster <- if (hindcast) {
  ft |> filter(season == TARGET_SEASON, week == TARGET_WEEK, !is.na(player_id)) |>
    select(player_id, posteam, defteam, game_id)
} else {
  roster_exante |> filter(!is.na(player_id)) |>
    select(player_id, posteam, defteam, game_id)
}
cli_alert_success("Slate roster: {nrow(roster)} QBs (ex-ante roster would be {nrow(roster_exante)})")

# ===========================================================================
# 3. PLAYER ROLLING + PRIORS
# ===========================================================================

cli_h1("Step 3: Player rolling features + priors")

player_roll <- season_hist |>
  semi_join(roster, by = "player_id") |>
  arrange(player_id, week) |>
  group_by(player_id) |>
  summarise(
    rolling_pass_epa_per_db = next_roll(pass_epa_per_db_obs,    ROLLING_WINDOW, DECAY_RATE),
    wt_dropbacks            = next_roll(dropbacks,              ROLLING_WINDOW, DECAY_RATE),
    wt_carries              = next_roll(carries,                ROLLING_WINDOW, DECAY_RATE),
    wt_carry_share          = next_roll(carry_share_obs,        ROLLING_WINDOW, DECAY_RATE),
    wt_rush_epa_pg          = next_roll(rush_epa,               ROLLING_WINDOW, DECAY_RATE),
    wt_rush_epa_per_carry   = next_roll(rush_epa_per_carry_obs, ROLLING_WINDOW, DECAY_RATE),
    .groups = "drop"
  )

rosters_all <- nflreadr::load_rosters(seasons = PREDICTION_SEASONS)
draft_raw   <- nflreadr::load_draft_picks()

draft_meta <- draft_raw |>
  filter(position == "QB", !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, draft_round = round)

qb_draft <- rosters_all |>
  filter(position == "QB") |>
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

# Pass priors (anchor season = TARGET_SEASON - 1; MIN_PRIOR_DB gate)
prior_pass <- qb_pass_plays |>
  filter(season == TARGET_SEASON - 1L) |>
  group_by(player_id) |>
  summarise(prior_db = n(), prior_pass_epa = sum(epa, na.rm = TRUE),
            prior_games = n_distinct(game_id), .groups = "drop") |>
  mutate(prior_pass_epa_per_db = if_else(prior_db >= MIN_PRIOR_DB,
                                         prior_pass_epa / prior_db, NA_real_))

tier_prior <- prior_pass |>
  left_join(qb_draft, by = "player_id") |>
  filter(!is.na(prior_pass_epa_per_db), !is.na(draft_tier)) |>
  group_by(draft_tier) |>
  summarise(tier_pass_epa_per_db = median(prior_pass_epa_per_db, na.rm = TRUE),
            .groups = "drop")

pos_prior <- median(prior_pass$prior_pass_epa_per_db[!is.na(prior_pass$prior_pass_epa_per_db)],
                    na.rm = TRUE)

# Rush priors: keyed to prior-season PASS games (a statue's zero-rush
# season still yields a valid 0-ish per-game prior), gated at 6 games /
# 20 carries -- frozen 08a step-7 logic
prior_rush_raw <- qb_rush_plays |>
  filter(season == TARGET_SEASON - 1L) |>
  group_by(player_id) |>
  summarise(prior_carries = n(), prior_rush_epa_total = sum(epa, na.rm = TRUE),
            .groups = "drop")

prior_rush <- prior_pass |>
  select(player_id, prior_games) |>
  left_join(prior_rush_raw, by = "player_id") |>
  mutate(
    prior_carries        = coalesce(prior_carries, 0L),
    prior_rush_epa_total = coalesce(prior_rush_epa_total, 0),
    prior_rush_epa_pg = if_else(prior_games >= MIN_PRIOR_GAMES,
                                prior_rush_epa_total / prior_games, NA_real_),
    prior_carries_pg  = if_else(prior_games >= MIN_PRIOR_GAMES,
                                prior_carries / prior_games, NA_real_),
    prior_rush_epa_per_carry = if_else(prior_carries >= MIN_PRIOR_CARRIES,
                                       prior_rush_epa_total / prior_carries, NA_real_)
  ) |>
  select(player_id, prior_rush_epa_pg, prior_carries_pg, prior_rush_epa_per_carry)

season_const <- qb_draft |>
  left_join(prior_pass |> select(player_id, prior_pass_epa_per_db), by = "player_id") |>
  left_join(tier_prior, by = "draft_tier") |>
  mutate(
    is_cold_start            = is.na(prior_pass_epa_per_db),
    baseline_pass_epa_per_db = if_else(is_cold_start,
                                       coalesce(tier_pass_epa_per_db, pos_prior),
                                       prior_pass_epa_per_db)
  ) |>
  select(player_id, prior_pass_epa_per_db, baseline_pass_epa_per_db,
         is_cold_start, draft_tier) |>
  left_join(prior_rush, by = "player_id")

player_names <- rosters_all |>
  filter(season == TARGET_SEASON, !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(player_id = gsis_id, player_name = full_name)

# ===========================================================================
# 4. TEAM + SNAP ROLLING
# ===========================================================================

cli_h1("Step 4: Team totals and snap-share rolling")

team_roll <- season_hist |>
  distinct(game_id, week, posteam, team_total_plays_obs, team_pass_rate_obs) |>
  arrange(posteam, week) |>
  group_by(posteam) |>
  summarise(
    wt_team_total_plays = next_roll(team_total_plays_obs, ROLLING_WINDOW, DECAY_RATE),
    wt_team_pass_rate   = next_roll(team_pass_rate_obs,   ROLLING_WINDOW, DECAY_RATE),
    .groups = "drop"
  )

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
# 5. DEFENSE (frozen 08a step-10 logic from persisted def_adj)
# ===========================================================================

cli_h1("Step 5: Defense features (from data/qb_def_adj.rds)")

def_prior <- def_adj |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(defteam, season, play_cat) |>
  summarise(ps_epa = mean(epa_per_play_adj, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = play_cat, values_from = ps_epa, names_prefix = "ps_") |>
  mutate(prediction_season = season + 1L) |>
  select(defteam, prediction_season, starts_with("ps_"))

lg_scalar <- map_dbl(set_names(DEF_CATS), function(cat) {
  mean(def_adj$epa_per_play_adj[def_adj$play_cat == cat &
                                def_adj$season %in% ANCHOR_SEASONS], na.rm = TRUE)
})

def_wide <- def_adj |>
  filter(season == TARGET_SEASON, week < TARGET_WEEK) |>
  select(game_id, week, defteam, play_cat, epa_per_play_adj) |>
  pivot_wider(names_from = play_cat, values_from = epa_per_play_adj,
              values_fill = list(epa_per_play_adj = 0))
for (cat in DEF_CATS) {
  if (!cat %in% names(def_wide)) def_wide[[cat]] <- 0
}

def_next <- def_wide |>
  arrange(defteam, week) |>
  group_by(defteam) |>
  summarise(
    games_played_so_far    = n(),
    def_short_pass_epa_adj = next_roll(short_pass, DEF_WINDOW, DECAY_RATE),
    def_deep_pass_epa_adj  = next_roll(deep_pass,  DEF_WINDOW, DECAY_RATE),
    def_rush_epa_adj       = next_roll(rush,       DEF_WINDOW, DECAY_RATE),
    .groups = "drop"
  ) |>
  left_join(def_prior |> filter(prediction_season == TARGET_SEASON), by = "defteam") |>
  mutate(
    def_used_fallback = games_played_so_far < FALLBACK_MIN_GAMES,
    def_short_pass_epa_adj = if_else(def_used_fallback, coalesce(ps_short_pass, lg_scalar[["short_pass"]]), def_short_pass_epa_adj),
    def_deep_pass_epa_adj  = if_else(def_used_fallback, coalesce(ps_deep_pass,  lg_scalar[["deep_pass"]]),  def_deep_pass_epa_adj),
    def_rush_epa_adj       = if_else(def_used_fallback, coalesce(ps_rush,       lg_scalar[["rush"]]),       def_rush_epa_adj)
  ) |>
  select(defteam, games_played_so_far, def_used_fallback,
         def_short_pass_epa_adj, def_deep_pass_epa_adj, def_rush_epa_adj)

# ===========================================================================
# 6. ASSEMBLE + INJURIES
# ===========================================================================

cli_h1("Step 6: Assemble slate rows")

slate <- roster |>
  left_join(player_roll,  by = "player_id") |>
  left_join(season_const, by = "player_id") |>
  left_join(player_names, by = "player_id") |>
  mutate(form_residual = rolling_pass_epa_per_db - baseline_pass_epa_per_db) |>
  left_join(team_roll, by = "posteam") |>
  left_join(snap_roll, by = c("player_id" = "gsis_id")) |>
  left_join(def_next,  by = "defteam") |>
  mutate(season = TARGET_SEASON, week = TARGET_WEEK)

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
# 7. VALIDATION GATE (hindcast only)
# ===========================================================================

if (hindcast) {
  cli_h1("Step 7: VALIDATION GATE -- slate vs frozen table, exact match")

  truth <- ft |> filter(season == TARGET_SEASON, week == TARGET_WEEK, !is.na(player_id))
  feat_cols <- c("prior_pass_epa_per_db", "baseline_pass_epa_per_db",
                 "rolling_pass_epa_per_db", "form_residual",
                 "prior_rush_epa_pg", "prior_carries_pg", "prior_rush_epa_per_carry",
                 "wt_dropbacks", "wt_carries", "wt_carry_share",
                 "wt_rush_epa_pg", "wt_rush_epa_per_carry",
                 "wt_snap_share", "wt_team_total_plays", "wt_team_pass_rate",
                 "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
                 "def_rush_epa_adj", "games_played_so_far")

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
    cli_alert_success("GATE PASSED: max |diff| = {format(worst, scientific = TRUE)} across {nrow(cmp)} QBs x {length(feat_cols)} features")
  } else {
    cli_abort("GATE FAILED: max |diff| = {worst}, NA mismatches = {na_bad}, flag mismatches = {flag_bad} -- DO NOT PROCEED")
  }
}

# ===========================================================================
# 8. SAVE
# ===========================================================================

cli_h1("Step 8: Save")

out_file <- sprintf("output/10b4_qb_slate_%d_w%02d.csv", TARGET_SEASON, TARGET_WEEK)
readr::write_csv(slate, out_file)
cli_alert_success("{out_file} ({nrow(slate)} rows)")

cli_h1("Step 10b4 complete (QB)")
