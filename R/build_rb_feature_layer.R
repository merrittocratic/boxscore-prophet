# R/build_rb_feature_layer.R  (v1.5)
# Feature layer for in-season RB EPA model -- step 1 of 5.
# PREDICTION_SEASONS produce output rows. ANCHOR_SEASONS supply prior-season
# baselines only. All rolling windows reset at season boundaries (group by season).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

# ===========================================================================
# PARAMETERS
# ===========================================================================
ALL_SEASONS        <- 2013L:2025L
PREDICTION_SEASONS <- 2014L:2025L
ANCHOR_SEASONS     <- 2013L:2024L       # prior-season baselines only, no output rows

ROLLING_WINDOW     <- 5L
DECAY_RATE         <- 0.85
DEF_WINDOW         <- 6L
FALLBACK_MIN_GAMES <- 3L
SHORT_PASS_THRESH  <- 10L    # air_yards; < this = short pass, >= this = deep pass
MIN_PRIOR_OPP      <- 10L    # prior-season opportunities needed for a non-NA baseline
MIN_OPPORTUNITIES  <- 5L     # per-game opp floor; rows below this are dropped from output

# ===========================================================================
# HELPERS
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

cli_h1("RB Feature Layer v1.5 | Seasons {paste(PREDICTION_SEASONS, collapse=', ')}")

# ===========================================================================
# 1. PULL RAW DATA
# ===========================================================================
cli_h1("Step 1: Pull raw data")

cli_alert_info("PBP seasons {paste(ALL_SEASONS, collapse='-')}")
pbp_raw <- nflreadr::load_pbp(ALL_SEASONS)

cli_alert_info("Rosters seasons {paste(ALL_SEASONS, collapse='-')}")
rosters_raw <- nflreadr::load_rosters(ALL_SEASONS)

cli_alert_info("Draft picks (all available seasons)")
draft_raw <- nflreadr::load_draft_picks()

cli_alert_info("Snap counts seasons {paste(PREDICTION_SEASONS, collapse='-')}")
snaps_raw <- nflreadr::load_snap_counts(PREDICTION_SEASONS)

# ===========================================================================
# 2. COLUMN INVENTORY
# ===========================================================================
cli_h1("Step 2: Column inventory")
cli_alert_info("PBP ({ncol(pbp_raw)} cols): {paste(names(pbp_raw), collapse=', ')}")
cli_alert_info("Rosters ({ncol(rosters_raw)} cols): {paste(names(rosters_raw), collapse=', ')}")
cli_alert_info("Draft ({ncol(draft_raw)} cols): {paste(names(draft_raw), collapse=', ')}")
cli_alert_info("Snaps ({ncol(snaps_raw)} cols): {paste(names(snaps_raw), collapse=', ')}")

req_pbp <- c("season","week","season_type","game_id","posteam","defteam",
             "rush_attempt","pass_attempt","qb_scramble","epa",
             "rusher_player_id","receiver_player_id","air_yards","play","play_type")
missing <- setdiff(req_pbp, names(pbp_raw))
if (length(missing) > 0) cli::cli_abort("Missing PBP columns: {paste(missing, collapse=', ')}")
cli_alert_success("All required PBP columns present")

# ===========================================================================
# 3. LOOKUP TABLES
# ===========================================================================
cli_h1("Step 3: Build lookup tables")

rb_ids <- rosters_raw |>
  filter(position == "RB") |>
  pull(gsis_id) |>
  unique()
cli_alert_info("{length(rb_ids)} unique RB gsis_ids across all seasons")

# pfr_id <-> gsis_id crosswalk for snap count join.
# Keep most-recent season's mapping when a pfr_id maps to multiple gsis_ids
# (can happen when ID data is corrected across roster vintages).
id_xwalk <- rosters_raw |>
  filter(!is.na(gsis_id), !is.na(pfr_id)) |>
  arrange(desc(season)) |>
  distinct(pfr_id, .keep_all = TRUE) |>
  select(gsis_id, pfr_id)

# Player name + team keyed by (gsis_id, season) -- players change teams year-to-year
player_meta <- rosters_raw |>
  filter(season %in% PREDICTION_SEASONS, position == "RB", !is.na(gsis_id)) |>
  distinct(gsis_id, season, .keep_all = TRUE) |>
  select(gsis_id, season, player_name = full_name, team)

draft_meta <- draft_raw |>
  filter(position %in% c("RB","FB"), !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, draft_round = round, draft_pick = pick)

rb_draft <- rosters_raw |>
  filter(position == "RB") |>
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
# 4. FILTER PBP TO REGULAR-SEASON RB PLAYS
# ===========================================================================
cli_h1("Step 4: Filter PBP to RB plays")

pbp_reg <- pbp_raw |>
  filter(season_type == "REG", !is.na(epa))

rb_rush <- pbp_reg |>
  filter(
    rush_attempt == 1,
    (is.na(qb_scramble) | qb_scramble == 0),
    rusher_player_id %in% rb_ids
  ) |>
  transmute(
    game_id, season, week, posteam, defteam,
    player_id = rusher_player_id,
    epa,
    play_cat  = "rush"
  )

rb_target <- pbp_reg |>
  filter(pass_attempt == 1, receiver_player_id %in% rb_ids) |>
  transmute(
    game_id, season, week, posteam, defteam,
    player_id = receiver_player_id,
    epa,
    # NA air_yards coded as short pass (consistent with nflreadr behavior on some completions)
    play_cat  = if_else(!is.na(air_yards) & air_yards >= SHORT_PASS_THRESH,
                        "deep_pass", "short_pass")
  )

# Drop NA posteam/defteam -- these produce orphaned join keys that
# cause downstream target_share_obs NAs for players in those games.
rb_plays <- bind_rows(rb_rush, rb_target) |>
  filter(!is.na(posteam), !is.na(defteam))

cli_alert_success(
  "RB plays: {nrow(rb_plays)} total ({sum(rb_plays$play_cat=='rush')} rush, {sum(rb_plays$play_cat!='rush')} targets), all seasons"
)

# ===========================================================================
# 5. OBSERVED OUTCOMES (PREDICTION_SEASONS only)
# ===========================================================================
cli_h1("Step 5: Build observed outcome table")

# All scrimmage plays for share denominator (play==1 covers runs + passes in nflreadr)
team_plays_obs <- pbp_reg |>
  filter(play == 1, !is.na(posteam)) |>
  group_by(game_id, season, week, posteam) |>
  summarise(team_total_plays_obs = n(), .groups = "drop")

team_rb_opp <- rb_plays |>
  group_by(game_id, season, week, posteam) |>
  summarise(
    team_rb_carries = sum(play_cat == "rush"),
    team_rb_targets = sum(play_cat != "rush"),
    .groups = "drop"
  )

rb_game <- rb_plays |>
  filter(season %in% PREDICTION_SEASONS) |>
  group_by(game_id, season, week, posteam, defteam, player_id) |>
  summarise(
    total_epa    = sum(epa, na.rm = TRUE),
    carries      = sum(play_cat == "rush"),
    targets      = sum(play_cat != "rush"),
    .groups      = "drop"
  ) |>
  mutate(opportunities = carries + targets)

rb_outcomes <- rb_game |>
  left_join(team_plays_obs, by = c("game_id","season","week","posteam")) |>
  left_join(team_rb_opp,   by = c("game_id","season","week","posteam")) |>
  mutate(
    epa_per_opp_obs  = if_else(opportunities > 0L, total_epa / opportunities, NA_real_),
    carry_share_obs  = if_else(team_rb_carries > 0L, carries / team_rb_carries, 0),
    # 0 when team had zero RB targets (all 0-target-game rows have targets==0, so share=0/0->0)
    target_share_obs = if_else(team_rb_targets > 0L, targets / team_rb_targets, 0)
  ) |>
  select(-team_rb_carries, -team_rb_targets)

cli_alert_success(
  "Outcome table: {nrow(rb_outcomes)} player-game rows (seasons {paste(PREDICTION_SEASONS, collapse='+')})"
)
cli_alert_info(
  "target_share_obs NAs remaining: {sum(is.na(rb_outcomes$target_share_obs))}"
)

# ===========================================================================
# 6. EFFICIENCY PRIOR FEATURES
# ===========================================================================
cli_h1("Step 6: Efficiency prior features")

# Prior stats keyed on prediction_season = anchor_season + 1
prior_stats <- rb_plays |>
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

# Tier priors per prediction_season (keeps 2023 anchor -> 2023 prior, 2024 anchor -> 2024 prior)
tier_prior <- prior_stats |>
  left_join(rb_draft |> select(player_id = gsis_id, draft_tier), by = "player_id") |>
  filter(!is.na(prior_epa_per_opp), !is.na(draft_tier)) |>
  group_by(draft_tier, prediction_season) |>
  summarise(tier_epa_per_opp = median(prior_epa_per_opp, na.rm = TRUE), .groups = "drop")

position_prior <- prior_stats |>
  filter(!is.na(prior_epa_per_opp)) |>
  group_by(prediction_season) |>
  summarise(pos_prior = median(prior_epa_per_opp, na.rm = TRUE), .groups = "drop")

cli_alert_info(
  "Position prior EPA/opp: {paste(position_prior$prediction_season, round(position_prior$pos_prior,3), sep='=', collapse=' | ')}"
)

# Rolling form -- group by (player_id, season) so the window resets each season
rb_form <- rb_outcomes |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  mutate(
    rolling_epa_per_opp = roll_wt_mean_prior(epa_per_opp_obs, ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup() |>
  left_join(prior_stats,    by = c("player_id", "season" = "prediction_season")) |>
  left_join(rb_draft |> select(gsis_id, draft_tier), by = c("player_id" = "gsis_id")) |>
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

pct_cold <- mean(rb_form$is_cold_start, na.rm = TRUE) * 100
cli_alert_success(
  "Efficiency prior: {sum(rb_form$is_cold_start, na.rm=TRUE)} cold-start rows ({round(pct_cold,1)}%)"
)

# ===========================================================================
# 7. VOLUME FEATURES
# ===========================================================================
cli_h1("Step 7: Volume features")

# Group by (player_id, season) -- window resets each season
rb_volume <- rb_form |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  mutate(
    wt_carry_share  = roll_wt_mean_prior(carry_share_obs,  ROLLING_WINDOW, DECAY_RATE),
    wt_target_share = roll_wt_mean_prior(target_share_obs, ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup()

# Team total plays rolling -- group by (posteam, season)
team_plays_rolling <- rb_outcomes |>
  distinct(game_id, season, week, posteam, team_total_plays_obs) |>
  arrange(posteam, season, week) |>
  group_by(posteam, season) |>
  mutate(wt_team_total_plays = roll_wt_mean_prior(team_total_plays_obs, ROLLING_WINDOW, DECAY_RATE)) |>
  ungroup() |>
  select(game_id, season, posteam, wt_team_total_plays)

rb_volume <- rb_volume |>
  left_join(team_plays_rolling, by = c("game_id","season","posteam"))

cli_alert_success("Volume features built")

# ===========================================================================
# 8. SNAP SHARE FEATURES
# ===========================================================================
cli_h1("Step 8: Snap share features")

snap_pct_divisor <- if (max(snaps_raw$offense_pct, na.rm = TRUE) > 1.5) 100 else 1

# Filter by gsis_id in rb_ids (not by position field in snap counts -- position labels
# differ across sources; e.g. FB players listed as RB in rosters appear as FB in snaps)
snaps_clean <- snaps_raw |>
  filter(game_type == "REG", !is.na(pfr_player_id), !is.na(offense_pct)) |>
  mutate(snap_pct = offense_pct / snap_pct_divisor) |>
  left_join(id_xwalk, by = c("pfr_player_id" = "pfr_id")) |>
  filter(!is.na(gsis_id), gsis_id %in% rb_ids) |>
  select(gsis_id, season, week, snap_pct)

cli_alert_info(
  "Snap counts: {nrow(snaps_clean)} player-game records, {n_distinct(snaps_clean$gsis_id)} unique players"
)

# Rolling snap share -- group by (gsis_id, season)
snap_rolling <- snaps_clean |>
  arrange(gsis_id, season, week) |>
  group_by(gsis_id, season) |>
  mutate(wt_snap_share = roll_wt_mean_prior(snap_pct, ROLLING_WINDOW, DECAY_RATE)) |>
  ungroup() |>
  select(gsis_id, season, week, wt_snap_share)

rb_volume <- rb_volume |>
  left_join(snap_rolling, by = c("player_id" = "gsis_id", "season", "week"))

n_snap_matched <- sum(!is.na(rb_volume$wt_snap_share))
cli_alert_info(
  "wt_snap_share: {n_snap_matched} non-NA of {nrow(rb_volume)} rows ({round(100*n_snap_matched/nrow(rb_volume),1)}%)"
)

# ===========================================================================
# 9. DEFENSIVE COMPONENT VECTOR
# ===========================================================================
cli_h1("Step 9: Defensive component vector")

# Per-game adjusted defensive EPA per play (all seasons needed for prior-season baselines)
def_per_game <- rb_plays |>
  group_by(game_id, season, week, defteam, play_cat) |>
  summarise(epa_sum = sum(epa, na.rm = TRUE), n_plays = n(), .groups = "drop") |>
  mutate(epa_per_play = epa_sum / n_plays)

off_per_game <- rb_plays |>
  group_by(game_id, season, week, posteam, play_cat) |>
  summarise(off_epa = sum(epa, na.rm = TRUE), off_n = n(), .groups = "drop") |>
  mutate(off_epa_per_play = off_epa / off_n)

# Cross-season offensive rolling strength (window does NOT reset per season here,
# so week-1 opponents have a meaningful prior from the previous year)
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
  "League avg off EPA/play: {paste(lg_avg_off$play_cat, round(lg_avg_off$lg_avg,3), sep='=', collapse=' | ')}"
)

# Schedule adjustment: adj = raw - (opponent_rolling_strength - league_avg)
# Positive adj_factor = opponent above average -> defense gets credit
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

for (cat in c("rush","short_pass","deep_pass")) {
  col <- paste0("ps_", cat)
  if (!col %in% names(def_prior)) def_prior[[col]] <- NA_real_
}

# League-average fallback (pooled anchor seasons)
lg_rush_scalar       <- mean(def_adj$epa_per_play_adj[def_adj$play_cat == "rush"       & def_adj$season %in% ANCHOR_SEASONS], na.rm = TRUE)
lg_short_pass_scalar <- mean(def_adj$epa_per_play_adj[def_adj$play_cat == "short_pass" & def_adj$season %in% ANCHOR_SEASONS], na.rm = TRUE)
lg_deep_pass_scalar  <- mean(def_adj$epa_per_play_adj[def_adj$play_cat == "deep_pass"  & def_adj$season %in% ANCHOR_SEASONS], na.rm = TRUE)

cli_alert_info(
  "League avg def EPA/play (anchor seasons): rush={round(lg_rush_scalar,3)}, short={round(lg_short_pass_scalar,3)}, deep={round(lg_deep_pass_scalar,3)}"
)

# Wide format; rolling per (defteam, season) -- window resets each season
def_wide <- def_adj |>
  filter(season %in% PREDICTION_SEASONS) |>
  pivot_wider(
    names_from  = play_cat,
    values_from = c(epa_per_play_adj, n_plays),
    values_fill = list(epa_per_play_adj = 0, n_plays = 0)
  )

for (cat in c("rush","short_pass","deep_pass")) {
  if (!paste0("epa_per_play_adj_", cat) %in% names(def_wide)) def_wide[[paste0("epa_per_play_adj_", cat)]] <- 0
  if (!paste0("n_plays_",          cat) %in% names(def_wide)) def_wide[[paste0("n_plays_",          cat)]] <- 0L
}

def_rolling <- def_wide |>
  arrange(defteam, season, week) |>
  group_by(defteam, season) |>
  mutate(
    games_played_so_far    = row_number() - 1L,
    def_rush_epa_adj       = roll_wt_mean_prior(epa_per_play_adj_rush,       DEF_WINDOW, DECAY_RATE),
    def_short_pass_epa_adj = roll_wt_mean_prior(epa_per_play_adj_short_pass, DEF_WINDOW, DECAY_RATE),
    def_deep_pass_epa_adj  = roll_wt_mean_prior(epa_per_play_adj_deep_pass,  DEF_WINDOW, DECAY_RATE)
  ) |>
  ungroup()

# Fallback: join prior-season grade on (defteam, season = prediction_season)
def_final <- def_rolling |>
  left_join(def_prior, by = c("defteam", "season" = "prediction_season")) |>
  mutate(
    def_used_fallback = games_played_so_far < FALLBACK_MIN_GAMES,
    def_rush_epa_adj = if_else(
      def_used_fallback, coalesce(ps_rush,       lg_rush_scalar),       def_rush_epa_adj
    ),
    def_short_pass_epa_adj = if_else(
      def_used_fallback, coalesce(ps_short_pass, lg_short_pass_scalar), def_short_pass_epa_adj
    ),
    def_deep_pass_epa_adj = if_else(
      def_used_fallback, coalesce(ps_deep_pass,  lg_deep_pass_scalar),  def_deep_pass_epa_adj
    )
  ) |>
  select(game_id, season, week, defteam,
         def_rush_epa_adj, def_short_pass_epa_adj, def_deep_pass_epa_adj,
         games_played_so_far, def_used_fallback)

cli_alert_success(
  "Defensive vector built; {sum(def_final$def_used_fallback)} fallback rows"
)

# ===========================================================================
# 10. ASSEMBLE, FILTER, OUTPUT
# ===========================================================================
cli_h1("Step 10: Assemble feature table")

feature_table_raw <- rb_volume |>
  left_join(def_final,   by = c("game_id","season","week","defteam")) |>
  left_join(player_meta, by = c("player_id" = "gsis_id", "season")) |>
  select(
    # GROUPING KEYS (preserved; non-negotiable for hierarchical model downstream)
    player_id, player_name, posteam, defteam, season, week, game_id,
    # OBSERVED OUTCOMES: efficiency x volume = total_epa (kept separate)
    epa_per_opp_obs, opportunities, carries, targets,
    carry_share_obs, target_share_obs, team_total_plays_obs, total_epa,
    # EFFICIENCY PRIOR FEATURES
    prior_epa_per_opp, baseline_epa_per_opp, rolling_epa_per_opp, form_residual,
    is_cold_start, draft_tier,
    # VOLUME FEATURES (recency-weighted, backward-looking)
    wt_carry_share, wt_target_share, wt_snap_share, wt_team_total_plays,
    # DEFENSIVE COMPONENT VECTOR (schedule-adjusted, player/defense/week nesting intact)
    def_rush_epa_adj, def_short_pass_epa_adj, def_deep_pass_epa_adj,
    games_played_so_far, def_used_fallback
  )

# Report opportunity distribution before filtering so the floor can be tuned
opp_below <- feature_table_raw |>
  filter(opportunities < MIN_OPPORTUNITIES) |>
  count(season, opportunities, name = "n_rows")

cli_alert_info(
  "MIN_OPPORTUNITIES={MIN_OPPORTUNITIES}: dropping {sum(opp_below$n_rows)} rows, keeping {nrow(feature_table_raw)-sum(opp_below$n_rows)}"
)
cli_alert_info(
  "Dropped opp distribution: {paste(opp_below$season, opp_below$opportunities, opp_below$n_rows, sep='/', collapse=' | ')}"
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

saveRDS(rb_plays,      "data/rb_plays.rds")
saveRDS(rb_outcomes,   "data/rb_outcomes.rds")
saveRDS(def_final,     "data/def_rolling_final.rds")
saveRDS(feature_table, "data/rb_feature_table.rds")
readr::write_csv(feature_table, "output/rb_feature_table_v2.0.csv")

cli_alert_success("data/rb_feature_table.rds")
cli_alert_success("output/rb_feature_table_v1.csv")

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
cli_alert_info("EPA recon mean err:    {format(mean(recon$diff, na.rm=TRUE), scientific=TRUE)}")

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

cli_h1("Done -- feature layer v2.0 frozen")
