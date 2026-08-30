# R/12a_te_feature_layer.R  (v1.1)
# Feature layer for in-season TE EPA model -- step 12a (TE clone of step 4a).
# Clones 04a_wr_feature_layer.R structure with TE-specific adjustments backed
# by the 12_te_feasibility.R receipts (output/12_te_feasibility_summary.csv):
#   - SHORT_PASS_THRESH = 7 (TE median aDOT 6.9 vs WR 10.4; the WR cut at 10
#     would leave only ~22% of TE work classified deep -- vector starves)
#   - NEW role feature wt_tgt_per_snap: rolling targets-per-snap built from the
#     SNAPS table with zero-target games filled in as 0. Feasibility showed
#     26.5% of high-snap TE weeks get <3 targets (blocking state) -- snap share
#     alone does not imply target volume for TEs, and the outcome table never
#     sees 0-target weeks, so the role signal must come from the snaps side.
#   - Everything else identical to WR: targets only, opportunities = targets,
#     MIN_OPPORTUNITIES = 3, same rolling windows/decay, same fold framework.
# Downstream column names (def_short_pass_epa_adj etc.) intentionally match
# the WR layer so 12b/12c clone from 04b/04c with minimal diffs.
#
# v1.1: Confirmed the same volume-side gap as WR (04a v1.1) and RB (v1.6):
# wt_target_share/wt_air_yards_share/wt_snap_share/wt_team_total_plays/
# wt_tgt_per_snap are all backward-looking within a season, NA at every
# player's Week 1 by construction. Adds the same baseline_* carryforward
# columns (real prior-season value -> draft-tier median -> league median),
# INCLUDING baseline_tgt_per_snap for the TE-specific role signal, which has
# the identical gap. Root-caused 2026-08-30; see project memory
# project_wr_coldstart_volume_gap ("very likely inherited by TE... unconfirmed
# at the code level" -- now confirmed). NOT yet wired into the deployed
# model's feature list or revalidated downstream -- feature table only.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

# ===========================================================================
# PARAMETERS
# ===========================================================================
ALL_SEASONS        <- 2013L:2026L
PREDICTION_SEASONS <- 2014L:2026L
ANCHOR_SEASONS     <- 2013L:2025L

ROLLING_WINDOW     <- 5L
DECAY_RATE         <- 0.85
DEF_WINDOW         <- 6L
FALLBACK_MIN_GAMES <- 3L
SHORT_PASS_THRESH  <- 7L     # TE-specific: median TE aDOT 6.9 (12_te receipts)
MIN_PRIOR_OPP      <- 10L    # prior-season targets needed for a non-NA baseline
MIN_OPPORTUNITIES  <- 3L     # per-game target floor; rows below this are dropped

# ===========================================================================
# HELPERS  (identical to RB/WR layers)
# ===========================================================================
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

# Strictly backward-looking: position i gets the weighted mean of positions 1..(i-1).
# Returns NA for i=1. Window resets between seasons when data is grouped by season.
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

cli_h1("TE Feature Layer v1.0 | Seasons {paste(PREDICTION_SEASONS, collapse=', ')}")

# ===========================================================================
# 1. PULL RAW DATA
# ===========================================================================
cli_h1("Step 1: Pull raw data")

cli_alert_info("PBP seasons {paste(ALL_SEASONS, collapse='-')}")
# pbp only exists for seasons nflverse serves (load_pbp(2026) hard-errors
# pre-season); rosters/draft DO have 2026 rows now. Clamp pbp only -- 2026
# feature rows appear as games are played, which is the intended behavior.
PBP_SEASONS <- ALL_SEASONS[ALL_SEASONS <= nflreadr::most_recent_season()]
pbp_raw <- nflreadr::load_pbp(PBP_SEASONS)

cli_alert_info("Rosters seasons {paste(ALL_SEASONS, collapse='-')}")
rosters_raw <- nflreadr::load_rosters(ALL_SEASONS)

cli_alert_info("Draft picks (all available seasons)")
draft_raw <- nflreadr::load_draft_picks()

cli_alert_info("Snap counts seasons {paste(PBP_SEASONS, collapse='-')}")
# Widened to PBP_SEASONS (was intersect(PREDICTION_SEASONS, PBP_SEASONS)) so the
# earliest ANCHOR_SEASONS year (2013) has real snap data to carry forward into
# prediction_season 2014's baseline_snap_share/baseline_tgt_per_snap -- see
# v1.1 note above.
snaps_raw <- nflreadr::load_snap_counts(PBP_SEASONS)

# ===========================================================================
# 2. COLUMN INVENTORY
# ===========================================================================
cli_h1("Step 2: Column inventory")

req_pbp <- c("season","week","season_type","game_id","posteam","defteam",
             "pass_attempt","epa","receiver_player_id","air_yards","play","play_type")
missing <- setdiff(req_pbp, names(pbp_raw))
if (length(missing) > 0) cli::cli_abort("Missing PBP columns: {paste(missing, collapse=', ')}")
req_snap <- c("game_type","pfr_player_id","offense_pct","offense_snaps")
missing_s <- setdiff(req_snap, names(snaps_raw))
if (length(missing_s) > 0) cli::cli_abort("Missing snap columns: {paste(missing_s, collapse=', ')}")
cli_alert_success("All required PBP and snap columns present")

# ===========================================================================
# 3. LOOKUP TABLES
# ===========================================================================
cli_h1("Step 3: Build lookup tables")

# !is.na guard: an NA gsis_id in this vector makes `receiver_player_id %in%
# te_ids` TRUE for every receiver-less pass play (NA %in% c(NA,...) is TRUE),
# creating phantom NA-player rows from throwaways/spikes. Found during 12a
# build; the WR layer (04a) has the same latent bug -- flagged separately.
te_ids <- rosters_raw |>
  filter(position == "TE", !is.na(gsis_id)) |>
  pull(gsis_id) |>
  unique()
cli_alert_info("{length(te_ids)} unique TE gsis_ids across all seasons")

# pfr_id <-> gsis_id crosswalk for snap count join.
# Keep most-recent season's mapping when a pfr_id maps to multiple gsis_ids.
id_xwalk <- rosters_raw |>
  filter(!is.na(gsis_id), !is.na(pfr_id)) |>
  arrange(desc(season)) |>
  distinct(pfr_id, .keep_all = TRUE) |>
  select(gsis_id, pfr_id)

# Player name + team keyed by (gsis_id, season) -- players change teams year-to-year
player_meta <- rosters_raw |>
  filter(season %in% PREDICTION_SEASONS, position == "TE", !is.na(gsis_id)) |>
  distinct(gsis_id, season, .keep_all = TRUE) |>
  select(gsis_id, season, player_name = full_name, team)

draft_meta <- draft_raw |>
  filter(position == "TE", !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, draft_round = round, draft_pick = pick)

te_draft <- rosters_raw |>
  filter(position == "TE") |>
  distinct(gsis_id) |>
  left_join(draft_meta, by = "gsis_id") |>
  mutate(
    draft_tier = case_when(
      is.na(draft_round)  ~ "udfa",
      draft_round == 1    ~ "r1",
      draft_round <= 3    ~ "r2_3",
      draft_round <= 5    ~ "r4_5",
      TRUE                ~ "r6_udfa"
    )
  )

# Canonical game-level team mapping (used for schedule adjustment join)
game_teams <- pbp_raw |>
  filter(season_type == "REG") |>
  distinct(game_id, season, week, posteam, defteam)

# ===========================================================================
# 4. FILTER PBP TO REGULAR-SEASON TE PLAYS (targets only -- no rush)
# ===========================================================================
cli_h1("Step 4: Filter PBP to TE plays")

pbp_reg <- pbp_raw |>
  filter(season_type == "REG", !is.na(epa))

te_plays <- pbp_reg |>
  filter(pass_attempt == 1, !is.na(receiver_player_id),
         receiver_player_id %in% te_ids) |>
  transmute(
    game_id, season, week, posteam, defteam,
    player_id = receiver_player_id,
    epa,
    air_yards,
    # NA air_yards coded as short pass (consistent with nflreadr behavior)
    play_cat = if_else(!is.na(air_yards) & air_yards >= SHORT_PASS_THRESH,
                       "deep_pass", "short_pass")
  ) |>
  filter(!is.na(posteam), !is.na(defteam))

cli_alert_success(
  "TE plays: {nrow(te_plays)} total ({sum(te_plays$play_cat=='deep_pass')} deep, {sum(te_plays$play_cat=='short_pass')} short at air_yards >= {SHORT_PASS_THRESH}), all seasons"
)

# ===========================================================================
# 5. OBSERVED OUTCOMES (PREDICTION_SEASONS only)
# ===========================================================================
cli_h1("Step 5: Build observed outcome table")

# All team pass attempts for target-share denominator (all eligible receivers)
team_pass_obs <- pbp_reg |>
  filter(pass_attempt == 1, !is.na(posteam)) |>
  group_by(game_id, season, week, posteam) |>
  summarise(
    team_total_targets   = n(),
    team_total_air_yards = sum(air_yards, na.rm = TRUE),
    .groups = "drop"
  )

# All scrimmage plays for team_total_plays denominator (play==1 covers runs + passes)
team_plays_obs <- pbp_reg |>
  filter(play == 1, !is.na(posteam)) |>
  group_by(game_id, season, week, posteam) |>
  summarise(team_total_plays_obs = n(), .groups = "drop")

te_game <- te_plays |>
  filter(season %in% PREDICTION_SEASONS) |>
  group_by(game_id, season, week, posteam, defteam, player_id) |>
  summarise(
    total_epa     = sum(epa, na.rm = TRUE),
    targets       = n(),
    air_yards_obs = sum(air_yards, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  mutate(opportunities = targets)

te_outcomes <- te_game |>
  left_join(team_pass_obs,  by = c("game_id","season","week","posteam")) |>
  left_join(team_plays_obs, by = c("game_id","season","week","posteam")) |>
  mutate(
    epa_per_opp_obs          = if_else(opportunities > 0L, total_epa / opportunities, NA_real_),
    target_share_obs         = if_else(team_total_targets > 0L,   targets / team_total_targets,     0),
    air_yards_share_obs      = if_else(team_total_air_yards > 0,  air_yards_obs / team_total_air_yards, 0),
    air_yards_per_target_obs = if_else(targets > 0L, air_yards_obs / targets, NA_real_)
  )

cli_alert_success(
  "Outcome table: {nrow(te_outcomes)} player-game rows (seasons {paste(PREDICTION_SEASONS, collapse='+')})"
)

# ===========================================================================
# 6. EFFICIENCY PRIOR FEATURES
# ===========================================================================
cli_h1("Step 6: Efficiency prior features")

# Prior stats keyed on prediction_season = anchor_season + 1
prior_stats <- te_plays |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(player_id, season) |>
  summarise(
    prior_opp = n(),
    prior_epa = sum(epa, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    prior_epa_per_opp = if_else(prior_opp >= MIN_PRIOR_OPP,
                                prior_epa / prior_opp,
                                NA_real_),
    prediction_season = season + 1L
  ) |>
  select(player_id, prediction_season, prior_epa_per_opp)

tier_prior <- prior_stats |>
  left_join(te_draft |> select(player_id = gsis_id, draft_tier), by = "player_id") |>
  filter(!is.na(prior_epa_per_opp), !is.na(draft_tier)) |>
  group_by(draft_tier, prediction_season) |>
  summarise(tier_epa_per_opp = median(prior_epa_per_opp, na.rm = TRUE), .groups = "drop")

position_prior <- prior_stats |>
  filter(!is.na(prior_epa_per_opp)) |>
  group_by(prediction_season) |>
  summarise(pos_prior = median(prior_epa_per_opp, na.rm = TRUE), .groups = "drop")

cli_alert_info(
  "Position prior EPA/target: {paste(position_prior$prediction_season, round(position_prior$pos_prior,3), sep='=', collapse=' | ')}"
)

# Rolling form -- group by (player_id, season) so the window resets each season
te_form <- te_outcomes |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  mutate(
    rolling_epa_per_opp = roll_wt_mean_prior(epa_per_opp_obs, ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup() |>
  left_join(prior_stats,    by = c("player_id", "season" = "prediction_season")) |>
  left_join(te_draft |> select(gsis_id, draft_tier), by = c("player_id" = "gsis_id")) |>
  left_join(tier_prior,     by = c("draft_tier", "season" = "prediction_season")) |>
  left_join(position_prior, by = c("season" = "prediction_season")) |>
  mutate(
    is_cold_start        = is.na(prior_epa_per_opp),
    baseline_epa_per_opp = if_else(
      is_cold_start,
      coalesce(tier_epa_per_opp, pos_prior),
      prior_epa_per_opp
    ),
    form_residual = rolling_epa_per_opp - baseline_epa_per_opp
  ) |>
  select(-tier_epa_per_opp, -pos_prior)

pct_cold <- mean(te_form$is_cold_start, na.rm = TRUE) * 100
cli_alert_success(
  "Efficiency prior: {sum(te_form$is_cold_start, na.rm=TRUE)} cold-start rows ({round(pct_cold,1)}%)"
)

# ===========================================================================
# 7. VOLUME FEATURES
# ===========================================================================
cli_h1("Step 7: Volume features")

# Group by (player_id, season) -- window resets each season
te_volume <- te_form |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  mutate(
    wt_target_share          = roll_wt_mean_prior(target_share_obs,          ROLLING_WINDOW, DECAY_RATE),
    wt_air_yards_share       = roll_wt_mean_prior(air_yards_share_obs,       ROLLING_WINDOW, DECAY_RATE),
    wt_air_yards_per_target  = roll_wt_mean_prior(air_yards_per_target_obs,  ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup()

# Team total plays rolling -- group by (posteam, season)
team_plays_rolling <- te_outcomes |>
  distinct(game_id, season, week, posteam, team_total_plays_obs) |>
  arrange(posteam, season, week) |>
  group_by(posteam, season) |>
  mutate(wt_team_total_plays = roll_wt_mean_prior(team_total_plays_obs, ROLLING_WINDOW, DECAY_RATE)) |>
  ungroup() |>
  select(game_id, season, posteam, wt_team_total_plays)

te_volume <- te_volume |>
  left_join(team_plays_rolling, by = c("game_id","season","posteam"))

cli_alert_success("Volume features built")

# ===========================================================================
# 7b. VOLUME BASELINE FEATURES (v1.1 fix -- prior-season carryforward)
#     Exact structural mirror of Step 6's efficiency baseline and of the WR
#     layer's Step 7b (04a v1.1). wt_target_share/wt_air_yards_share above are
#     NA at every player's Week 1 by construction; this baseline gives Week 1
#     rows a real signal instead of falling through to draft_tier alone.
# ===========================================================================
cli_h1("Step 7b: Volume baseline features (prior-season carryforward)")

# te_plays (Step 4) and team_pass_obs/team_plays_obs (Step 5) are all built
# from pbp_reg over ALL_SEASONS (2013-2026), NOT filtered to PREDICTION_SEASONS
# -- so ANCHOR_SEASONS (2013-2025) already has real target/team-total data
# here, no extra PBP pull needed.
te_game_anchor <- te_plays |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(game_id, season, week, posteam, player_id) |>
  summarise(
    targets       = n(),
    air_yards_obs = sum(air_yards, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  left_join(team_pass_obs, by = c("game_id","season","week","posteam"))

prior_vol_stats <- te_game_anchor |>
  group_by(player_id, season) |>
  summarise(
    prior_targets        = sum(targets, na.rm = TRUE),
    prior_team_targets   = sum(team_total_targets, na.rm = TRUE),
    prior_air_yards      = sum(air_yards_obs, na.rm = TRUE),
    prior_team_air_yards = sum(team_total_air_yards, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    prior_target_share    = if_else(prior_targets >= MIN_PRIOR_OPP,
                                    prior_targets / prior_team_targets, NA_real_),
    prior_air_yards_share = if_else(prior_targets >= MIN_PRIOR_OPP & prior_team_air_yards > 0,
                                    prior_air_yards / prior_team_air_yards, NA_real_),
    prediction_season = season + 1L
  ) |>
  select(player_id, prediction_season, prior_target_share, prior_air_yards_share)

tier_vol_prior <- prior_vol_stats |>
  left_join(te_draft |> select(player_id = gsis_id, draft_tier), by = "player_id") |>
  filter(!is.na(draft_tier)) |>
  group_by(draft_tier, prediction_season) |>
  summarise(
    tier_target_share    = median(prior_target_share,    na.rm = TRUE),
    tier_air_yards_share = median(prior_air_yards_share, na.rm = TRUE),
    .groups = "drop"
  )

position_vol_prior <- prior_vol_stats |>
  group_by(prediction_season) |>
  summarise(
    pos_target_share    = median(prior_target_share,    na.rm = TRUE),
    pos_air_yards_share = median(prior_air_yards_share, na.rm = TRUE),
    .groups = "drop"
  )

te_volume <- te_volume |>
  left_join(prior_vol_stats,    by = c("player_id", "season" = "prediction_season")) |>
  left_join(tier_vol_prior,     by = c("draft_tier", "season" = "prediction_season")) |>
  left_join(position_vol_prior, by = c("season" = "prediction_season")) |>
  mutate(
    baseline_target_share    = coalesce(prior_target_share,    tier_target_share,    pos_target_share),
    baseline_air_yards_share = coalesce(prior_air_yards_share, tier_air_yards_share, pos_air_yards_share)
  ) |>
  select(-prior_target_share, -prior_air_yards_share,
         -tier_target_share, -tier_air_yards_share,
         -pos_target_share, -pos_air_yards_share)

n_na_bts <- sum(is.na(te_volume$baseline_target_share))
cli_alert_success(
  "baseline_target_share/baseline_air_yards_share built | NA baseline_target_share: {n_na_bts} of {nrow(te_volume)}"
)

# Team-level baseline_team_total_plays -- exact structural mirror of Step 9's
# def_prior/lg_scalar pattern. team_plays_obs (Step 5) already covers
# ANCHOR_SEASONS (unfiltered pbp_reg), no extra pull needed.
prior_team_plays_vol <- team_plays_obs |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(posteam, season) |>
  summarise(prior_team_total_plays = mean(team_total_plays_obs, na.rm = TRUE), .groups = "drop") |>
  mutate(prediction_season = season + 1L) |>
  select(posteam, prediction_season, prior_team_total_plays)

lg_team_plays_scalar <- mean(
  team_plays_obs$team_total_plays_obs[team_plays_obs$season %in% ANCHOR_SEASONS],
  na.rm = TRUE
)

te_volume <- te_volume |>
  left_join(prior_team_plays_vol, by = c("posteam", "season" = "prediction_season")) |>
  mutate(
    baseline_team_total_plays = coalesce(prior_team_total_plays, lg_team_plays_scalar)
  ) |>
  select(-prior_team_total_plays)

cli_alert_success(
  "baseline_team_total_plays built | league scalar fallback: {round(lg_team_plays_scalar,1)}"
)

# ===========================================================================
# 8. SNAP SHARE + ROLE FEATURES (TE-specific block)
# ===========================================================================
cli_h1("Step 8: Snap share + targets-per-snap role features")

snap_pct_divisor <- if (max(snaps_raw$offense_pct, na.rm = TRUE) > 1.5) 100 else 1

# Filter by gsis_id in te_ids (not by position field in snap counts)
snaps_clean <- snaps_raw |>
  filter(game_type == "REG", !is.na(pfr_player_id), !is.na(offense_pct)) |>
  mutate(snap_pct = offense_pct / snap_pct_divisor) |>
  left_join(id_xwalk, by = c("pfr_player_id" = "pfr_id")) |>
  filter(!is.na(gsis_id), gsis_id %in% te_ids) |>
  select(gsis_id, season, week, snap_pct, offense_snaps)

cli_alert_info(
  "Snap counts: {nrow(snaps_clean)} player-game records, {n_distinct(snaps_clean$gsis_id)} unique players"
)

# TE ROLE SIGNAL: targets per offensive snap, built on the SNAPS table so that
# zero-target (blocking) weeks enter the rolling window as 0 -- the outcome
# table never sees those weeks. Feasibility receipt: 26.5% of high-snap TE
# weeks fall under the 3-target floor vs 10.7% for WR.
game_targets <- te_game |>
  select(player_id, season, week, targets)

snap_role <- snaps_clean |>
  left_join(game_targets, by = c("gsis_id" = "player_id", "season", "week")) |>
  mutate(
    targets_filled  = coalesce(targets, 0L),
    tgt_per_snap_obs = if_else(offense_snaps > 0L,
                               targets_filled / offense_snaps,
                               NA_real_)
  )

n_zero_tgt <- sum(snap_role$targets_filled == 0L & snap_role$snap_pct >= 0.25)
cli_alert_info(
  "Zero-target games with 25%+ snaps entering role windows: {n_zero_tgt} ({round(100*n_zero_tgt/nrow(snap_role),1)}% of snap games)"
)

# Rolling snap share + role -- group by (gsis_id, season)
snap_rolling <- snap_role |>
  arrange(gsis_id, season, week) |>
  group_by(gsis_id, season) |>
  mutate(
    wt_snap_share   = roll_wt_mean_prior(snap_pct,         ROLLING_WINDOW, DECAY_RATE),
    wt_tgt_per_snap = roll_wt_mean_prior(tgt_per_snap_obs, ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup() |>
  select(gsis_id, season, week, wt_snap_share, wt_tgt_per_snap)

te_volume <- te_volume |>
  left_join(snap_rolling, by = c("player_id" = "gsis_id", "season", "week"))

n_snap_matched <- sum(!is.na(te_volume$wt_snap_share))
n_role_matched <- sum(!is.na(te_volume$wt_tgt_per_snap))
cli_alert_info(
  "wt_snap_share: {n_snap_matched} non-NA of {nrow(te_volume)} rows ({round(100*n_snap_matched/nrow(te_volume),1)}%)"
)
cli_alert_info(
  "wt_tgt_per_snap: {n_role_matched} non-NA of {nrow(te_volume)} rows ({round(100*n_role_matched/nrow(te_volume),1)}%)"
)

# Prior-season snap-share AND targets-per-snap carryforward (v1.1 fix) -- same
# fallback ladder as baseline_target_share above. tgt_per_snap uses a pooled
# season-level ratio (sum targets / sum snaps), matching how target_share was
# computed on the WR side, rather than a mean of noisy per-game ratios.
prior_snap_stats <- snap_role |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(gsis_id, season) |>
  summarise(
    prior_snap_share    = mean(snap_pct, na.rm = TRUE),
    prior_targets_sum    = sum(targets_filled, na.rm = TRUE),
    prior_snaps_sum       = sum(offense_snaps, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    prior_tgt_per_snap = if_else(prior_snaps_sum > 0L, prior_targets_sum / prior_snaps_sum, NA_real_),
    prediction_season  = season + 1L
  ) |>
  select(player_id = gsis_id, prediction_season, prior_snap_share, prior_tgt_per_snap)

tier_snap_prior <- prior_snap_stats |>
  left_join(te_draft |> select(player_id = gsis_id, draft_tier), by = "player_id") |>
  filter(!is.na(draft_tier)) |>
  group_by(draft_tier, prediction_season) |>
  summarise(
    tier_snap_share    = median(prior_snap_share,    na.rm = TRUE),
    tier_tgt_per_snap  = median(prior_tgt_per_snap,  na.rm = TRUE),
    .groups = "drop"
  )

position_snap_prior <- prior_snap_stats |>
  group_by(prediction_season) |>
  summarise(
    pos_snap_share   = median(prior_snap_share,   na.rm = TRUE),
    pos_tgt_per_snap = median(prior_tgt_per_snap, na.rm = TRUE),
    .groups = "drop"
  )

te_volume <- te_volume |>
  left_join(prior_snap_stats,    by = c("player_id", "season" = "prediction_season")) |>
  left_join(tier_snap_prior,     by = c("draft_tier", "season" = "prediction_season")) |>
  left_join(position_snap_prior, by = c("season" = "prediction_season")) |>
  mutate(
    baseline_snap_share   = coalesce(prior_snap_share,   tier_snap_share,   pos_snap_share),
    baseline_tgt_per_snap = coalesce(prior_tgt_per_snap, tier_tgt_per_snap, pos_tgt_per_snap)
  ) |>
  select(-prior_snap_share, -prior_tgt_per_snap,
         -tier_snap_share, -tier_tgt_per_snap,
         -pos_snap_share, -pos_tgt_per_snap)

n_na_bss <- sum(is.na(te_volume$baseline_snap_share))
n_na_btps <- sum(is.na(te_volume$baseline_tgt_per_snap))
cli_alert_success(
  "baseline_snap_share built | NA: {n_na_bss} of {nrow(te_volume)}"
)
cli_alert_success(
  "baseline_tgt_per_snap built | NA: {n_na_btps} of {nrow(te_volume)}"
)

# ===========================================================================
# 9. DEFENSIVE COMPONENT VECTOR (short_pass + deep_pass vs TE routes only)
# ===========================================================================
cli_h1("Step 9: Defensive component vector")

# Per-game adjusted defensive EPA per TE target (all seasons for prior-season baselines)
def_per_game <- te_plays |>
  group_by(game_id, season, week, defteam, play_cat) |>
  summarise(epa_sum = sum(epa, na.rm = TRUE), n_plays = n(), .groups = "drop") |>
  mutate(epa_per_play = epa_sum / n_plays)

off_per_game <- te_plays |>
  group_by(game_id, season, week, posteam, play_cat) |>
  summarise(off_epa = sum(epa, na.rm = TRUE), off_n = n(), .groups = "drop") |>
  mutate(off_epa_per_play = off_epa / off_n)

# Cross-season offensive rolling strength (window does NOT reset per season;
# week-1 opponents have a meaningful prior from the previous year)
off_rolling_strength <- off_per_game |>
  arrange(posteam, play_cat, season, week) |>
  group_by(posteam, play_cat) |>
  mutate(rolling_off_strength = roll_wt_mean_prior(off_epa_per_play, DEF_WINDOW, DECAY_RATE)) |>
  ungroup() |>
  select(game_id, posteam, play_cat, rolling_off_strength)

lg_avg_off <- off_per_game |>
  group_by(play_cat) |>
  summarise(lg_avg = mean(off_epa_per_play, na.rm = TRUE), .groups = "drop")

cli_alert_info(
  "League avg off EPA/TE target: {paste(lg_avg_off$play_cat, round(lg_avg_off$lg_avg,3), sep='=', collapse=' | ')}"
)

# Schedule adjustment: adj = raw - (opponent_rolling_strength - league_avg)
def_adj <- def_per_game |>
  left_join(
    game_teams |> select(game_id, defteam, opponent = posteam) |> distinct(),
    by = c("game_id","defteam")
  ) |>
  left_join(off_rolling_strength, by = c("game_id","opponent" = "posteam","play_cat")) |>
  left_join(lg_avg_off, by = "play_cat") |>
  mutate(
    opp_strength     = coalesce(rolling_off_strength, lg_avg),
    adj_factor       = opp_strength - lg_avg,
    epa_per_play_adj = epa_per_play - adj_factor
  ) |>
  select(game_id, season, week, defteam, play_cat, epa_per_play_adj, n_plays)

# Prior-season defensive baselines keyed by prediction_season = anchor_season + 1
def_prior <- def_adj |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(defteam, season, play_cat) |>
  summarise(ps_epa = mean(epa_per_play_adj, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = play_cat, values_from = ps_epa, names_prefix = "ps_") |>
  mutate(prediction_season = season + 1L) |>
  select(defteam, prediction_season, starts_with("ps_"))

for (cat in c("short_pass","deep_pass")) {
  col <- paste0("ps_", cat)
  if (!col %in% names(def_prior)) def_prior[[col]] <- NA_real_
}

# League-average fallback (pooled anchor seasons)
lg_short_pass_scalar <- mean(def_adj$epa_per_play_adj[def_adj$play_cat == "short_pass" & def_adj$season %in% ANCHOR_SEASONS], na.rm = TRUE)
lg_deep_pass_scalar  <- mean(def_adj$epa_per_play_adj[def_adj$play_cat == "deep_pass"  & def_adj$season %in% ANCHOR_SEASONS], na.rm = TRUE)

cli_alert_info(
  "League avg def EPA/TE target (anchor seasons): short={round(lg_short_pass_scalar,3)}, deep={round(lg_deep_pass_scalar,3)}"
)

# Wide format; rolling per (defteam, season) -- window resets each season
def_wide <- def_adj |>
  filter(season %in% PREDICTION_SEASONS) |>
  pivot_wider(
    names_from  = play_cat,
    values_from = c(epa_per_play_adj, n_plays),
    values_fill = list(epa_per_play_adj = 0, n_plays = 0)
  )

for (cat in c("short_pass","deep_pass")) {
  if (!paste0("epa_per_play_adj_", cat) %in% names(def_wide)) def_wide[[paste0("epa_per_play_adj_", cat)]] <- 0
  if (!paste0("n_plays_",          cat) %in% names(def_wide)) def_wide[[paste0("n_plays_",          cat)]] <- 0L
}

def_rolling <- def_wide |>
  arrange(defteam, season, week) |>
  group_by(defteam, season) |>
  mutate(
    games_played_so_far    = row_number() - 1L,
    def_short_pass_epa_adj = roll_wt_mean_prior(epa_per_play_adj_short_pass, DEF_WINDOW, DECAY_RATE),
    def_deep_pass_epa_adj  = roll_wt_mean_prior(epa_per_play_adj_deep_pass,  DEF_WINDOW, DECAY_RATE)
  ) |>
  ungroup()

# Fallback: join prior-season grade on (defteam, season = prediction_season)
def_final <- def_rolling |>
  left_join(def_prior, by = c("defteam", "season" = "prediction_season")) |>
  mutate(
    def_used_fallback = games_played_so_far < FALLBACK_MIN_GAMES,
    def_short_pass_epa_adj = if_else(
      def_used_fallback, coalesce(ps_short_pass, lg_short_pass_scalar), def_short_pass_epa_adj
    ),
    def_deep_pass_epa_adj = if_else(
      def_used_fallback, coalesce(ps_deep_pass, lg_deep_pass_scalar), def_deep_pass_epa_adj
    )
  ) |>
  select(game_id, season, week, defteam,
         def_short_pass_epa_adj, def_deep_pass_epa_adj,
         games_played_so_far, def_used_fallback)

cli_alert_success(
  "Defensive vector built; {sum(def_final$def_used_fallback)} fallback rows"
)

# ===========================================================================
# 10. ASSEMBLE, FILTER, OUTPUT
# ===========================================================================
cli_h1("Step 10: Assemble feature table")

feature_table_raw <- te_volume |>
  left_join(def_final,   by = c("game_id","season","week","defteam")) |>
  left_join(player_meta, by = c("player_id" = "gsis_id", "season")) |>
  select(
    # GROUPING KEYS (preserved; non-negotiable for hierarchical model downstream)
    player_id, player_name, posteam, defteam, season, week, game_id,
    # OBSERVED OUTCOMES: efficiency x volume = total_epa (kept separate)
    epa_per_opp_obs, opportunities, targets,
    target_share_obs, air_yards_obs, air_yards_share_obs, air_yards_per_target_obs,
    team_total_plays_obs, total_epa,
    # EFFICIENCY PRIOR FEATURES
    prior_epa_per_opp, baseline_epa_per_opp, rolling_epa_per_opp, form_residual,
    is_cold_start, draft_tier,
    # VOLUME FEATURES (recency-weighted, backward-looking)
    wt_target_share, wt_air_yards_share, wt_air_yards_per_target,
    wt_snap_share, wt_tgt_per_snap, wt_team_total_plays,
    # VOLUME BASELINE FEATURES (v1.1 -- prior-season carryforward, fills the
    # Week 1 gap in the wt_* columns above; same fallback ladder as
    # baseline_epa_per_opp)
    baseline_target_share, baseline_air_yards_share,
    baseline_snap_share, baseline_tgt_per_snap, baseline_team_total_plays,
    # DEFENSIVE COMPONENT VECTOR (short + deep pass vs TE; split at 7 air yards)
    def_short_pass_epa_adj, def_deep_pass_epa_adj,
    games_played_so_far, def_used_fallback
  )

opp_below <- feature_table_raw |>
  filter(opportunities < MIN_OPPORTUNITIES) |>
  count(season, opportunities, name = "n_rows")

cli_alert_info(
  "MIN_OPPORTUNITIES={MIN_OPPORTUNITIES}: dropping {sum(opp_below$n_rows)} rows, keeping {nrow(feature_table_raw)-sum(opp_below$n_rows)}"
)

feature_table <- feature_table_raw |>
  filter(opportunities >= MIN_OPPORTUNITIES)

cli_alert_success(
  "Feature table: {nrow(feature_table)} rows x {ncol(feature_table)} columns"
)

# ===========================================================================
# 11. SAVE
# ===========================================================================
cli_h1("Step 11: Save outputs")
dir.create("data",   showWarnings = FALSE, recursive = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)

saveRDS(te_plays,      "data/te_plays.rds")
saveRDS(te_outcomes,   "data/te_outcomes.rds")
saveRDS(def_final,     "data/te_def_rolling_final.rds")
saveRDS(feature_table, "data/te_feature_table.rds")
readr::write_csv(feature_table, "output/te_feature_table_v1.0.csv")

cli_alert_success("data/te_feature_table.rds")
cli_alert_success("output/te_feature_table_v1.0.csv")

# ===========================================================================
# 12. VALIDATION SUMMARY
# ===========================================================================
cli_h1("Step 12: Validation summary")

n_rows       <- nrow(feature_table)
n_players    <- n_distinct(feature_table$player_id)
n_sw         <- n_distinct(paste(feature_table$season, feature_table$week))
seasons_pres <- sort(unique(feature_table$season))
pct_cold     <- mean(feature_table$is_cold_start, na.rm = TRUE) * 100
pct_fallback <- mean(feature_table$def_used_fallback, na.rm = TRUE) * 100

recon <- feature_table |>
  filter(!is.na(epa_per_opp_obs), opportunities > 0L) |>
  mutate(diff = abs(epa_per_opp_obs * opportunities - total_epa))

cli_alert_info("Rows:                  {n_rows}")
cli_alert_info("Unique players:        {n_players}")
cli_alert_info("Season-weeks:          {n_sw}")
cli_alert_info("Seasons present:       {paste(seasons_pres, collapse=', ')}")
cli_alert_info("Cold-start rows:       {round(pct_cold,1)}%")
cli_alert_info("Def fallback rows:     {round(pct_fallback,1)}%")
cli_alert_info("EPA recon max err:     {format(max(recon$diff, na.rm=TRUE), scientific=TRUE)}")

if (max(recon$diff, na.rm = TRUE) < 1e-8) {
  cli_alert_success("efficiency x volume reconstructs total_epa -- PASS")
} else {
  cli::cli_warn("EPA reconstruction error too large -- FAIL, inspect outcome columns")
}

# Per-column NA audit
cli_h1("Per-column NA audit")
na_audit <- feature_table |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "col", values_to = "n_na") |>
  mutate(pct_na = round(100 * n_na / n_rows, 1)) |>
  filter(n_na > 0) |>
  arrange(desc(n_na))

if (nrow(na_audit) == 0) {
  cli_alert_success("Zero NA values across all columns")
} else {
  for (i in seq_len(nrow(na_audit))) {
    cli_alert_info("  {na_audit$col[i]}: {na_audit$n_na[i]} NA ({na_audit$pct_na[i]}%)")
  }
}

cli_h1("Done -- TE feature layer v1.1 (volume carryforward fix, unvalidated downstream)")
