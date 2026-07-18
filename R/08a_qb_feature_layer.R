# R/08a_qb_feature_layer.R  (v1.0)
# Feature layer for in-season QB EPA model -- step 8a (QB build after the
# step-7 feasibility green light).
#
# ARCHITECTURE (locked with Steve 2026-07-07): HYBRID two-component design.
#   - Pass side clones the RB eff x vol recipe: pass_epa_per_db x dropbacks
#   - Rush side is modeled DIRECTLY (rush_epa per game) plus a separate
#     carries model for the FP translation layer (+0.62 FP per carry floor).
#     Per-carry rush efficiency is NOT a primary component (2-8 carries per
#     game is single-play noise) but the observed/rolling per-carry columns
#     are kept so 08b can bake off direct-vs-product on the rush side.
#   - Combined: total_epa = pass_epa + rush_epa (SUM, not product).
#
# DEFINITIONS (mirror 07_qb_feasibility.R so thresholds/tiers transfer):
#   - Pass play  = qb_dropback, NOT a scramble, NOT a spike (= attempts + sacks)
#   - Rush play  = QB carry incl. scrambles, EXCL. kneel-downs (kneels are
#     negative-EPA, zero-fantasy plays that would poison winning-team QBs)
#   - Starter game = dropbacks >= 15 (drops relief/mop-up appearances)
#   - Rush tiers (veto axis): statue 0-3 / mover 4-7 / scrambler 8+ carries
#
# PREDICTION_SEASONS, ANCHOR_SEASONS, and fold_map are identical to RB/WR so
# all three positions share the same walk-forward evaluation framework.

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
ANCHOR_SEASONS     <- 2013L:2024L

ROLLING_WINDOW     <- 5L
DECAY_RATE         <- 0.85
DEF_WINDOW         <- 6L
FALLBACK_MIN_GAMES <- 3L
SHORT_PASS_THRESH  <- 10L    # air_yards; < this = short pass, >= this = deep pass
MIN_PRIOR_DB       <- 100L   # prior-season dropbacks needed for a non-NA pass baseline
MIN_PRIOR_GAMES    <- 6L     # prior-season games needed for rush per-game priors
MIN_PRIOR_CARRIES  <- 20L    # prior-season carries needed for per-carry rush prior
MIN_DROPBACKS      <- 15L    # per-game starter floor; rows below this are dropped

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

cli_h1("QB Feature Layer v1.0 | Seasons {paste(range(PREDICTION_SEASONS), collapse='-')}")

# ===========================================================================
# 1. PULL RAW DATA
# ===========================================================================
cli_h1("Step 1: Pull raw data")

cli_alert_info("PBP seasons {paste(range(ALL_SEASONS), collapse='-')}")
pbp_raw <- nflreadr::load_pbp(ALL_SEASONS)

cli_alert_info("Rosters seasons {paste(range(ALL_SEASONS), collapse='-')}")
rosters_raw <- nflreadr::load_rosters(ALL_SEASONS)

cli_alert_info("Draft picks (all available seasons)")
draft_raw <- nflreadr::load_draft_picks()

cli_alert_info("Snap counts seasons {paste(range(PREDICTION_SEASONS), collapse='-')}")
snaps_raw <- nflreadr::load_snap_counts(PREDICTION_SEASONS)

# ===========================================================================
# 2. COLUMN CHECK
# ===========================================================================
cli_h1("Step 2: Column check")

req_pbp <- c("season","week","season_type","game_id","posteam","defteam",
             "epa","air_yards","play","qb_dropback","qb_scramble","qb_spike",
             "qb_kneel","rush_attempt","passer_player_id","rusher_player_id")
missing <- setdiff(req_pbp, names(pbp_raw))
if (length(missing) > 0) cli::cli_abort("Missing PBP columns: {paste(missing, collapse=', ')}")
cli_alert_success("All required PBP columns present")

# ===========================================================================
# 3. LOOKUP TABLES
# ===========================================================================
cli_h1("Step 3: Build lookup tables")

qb_ids <- rosters_raw |>
  filter(position == "QB") |>
  pull(gsis_id) |>
  unique()
cli_alert_info("{length(qb_ids)} unique QB gsis_ids across all seasons")

id_xwalk <- rosters_raw |>
  filter(!is.na(gsis_id), !is.na(pfr_id)) |>
  arrange(desc(season)) |>
  distinct(pfr_id, .keep_all = TRUE) |>
  select(gsis_id, pfr_id)

player_meta <- rosters_raw |>
  filter(season %in% PREDICTION_SEASONS, position == "QB", !is.na(gsis_id)) |>
  distinct(gsis_id, season, .keep_all = TRUE) |>
  select(gsis_id, season, player_name = full_name, team)

draft_meta <- draft_raw |>
  filter(position == "QB", !is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, draft_round = round, draft_pick = pick)

qb_draft <- rosters_raw |>
  filter(position == "QB") |>
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

game_teams <- pbp_raw |>
  filter(season_type == "REG") |>
  distinct(game_id, season, week, posteam, defteam)

# ===========================================================================
# 4. FILTER PBP TO QB PLAYS (pass side + rush side)
# ===========================================================================
cli_h1("Step 4: Filter PBP to QB plays")

pbp_reg <- pbp_raw |>
  filter(season_type == "REG", !is.na(epa))

# Pass side: dropbacks excluding scrambles and spikes (= attempts + sacks).
# Scrambles are credited to the rush side, matching nflreadr weekly stats and
# the step-7 feasibility definitions.
qb_pass_plays <- pbp_reg |>
  filter(
    qb_dropback == 1,
    coalesce(qb_scramble, 0) != 1,
    coalesce(qb_spike,    0) != 1,
    passer_player_id %in% qb_ids
  ) |>
  transmute(
    game_id, season, week, posteam, defteam,
    player_id = passer_player_id,
    epa,
    air_yards,
    # NA air_yards (sacks, throwaways) coded as short pass, consistent with WR layer
    play_cat = if_else(!is.na(air_yards) & air_yards >= SHORT_PASS_THRESH,
                       "deep_pass", "short_pass")
  ) |>
  filter(!is.na(posteam), !is.na(defteam))

# Rush side: QB carries including scrambles, EXCLUDING kneel-downs.
qb_rush_plays <- pbp_reg |>
  filter(
    (rush_attempt == 1 | coalesce(qb_scramble, 0) == 1),
    coalesce(qb_kneel, 0) != 1,
    rusher_player_id %in% qb_ids
  ) |>
  transmute(
    game_id, season, week, posteam, defteam,
    player_id = rusher_player_id,
    epa,
    is_scramble = as.integer(coalesce(qb_scramble, 0) == 1)
  ) |>
  filter(!is.na(posteam), !is.na(defteam))

n_kneels <- pbp_reg |>
  filter(coalesce(qb_kneel, 0) == 1, rusher_player_id %in% qb_ids) |>
  nrow()

cli_alert_success("QB pass plays (attempts + sacks): {nrow(qb_pass_plays)}")
cli_alert_success(
  "QB rush plays: {nrow(qb_rush_plays)} ({sum(qb_rush_plays$is_scramble)} scrambles) | {n_kneels} kneel-downs excluded"
)

# ===========================================================================
# 5. OBSERVED OUTCOMES (PREDICTION_SEASONS only)
# ===========================================================================
cli_h1("Step 5: Build observed outcome table")

# Team-level denominators from all offenses
team_plays_obs <- pbp_reg |>
  filter(play == 1, !is.na(posteam)) |>
  group_by(game_id, season, week, posteam) |>
  summarise(team_total_plays_obs = n(), .groups = "drop")

team_db_obs <- pbp_reg |>
  filter(qb_dropback == 1, coalesce(qb_spike, 0) != 1, !is.na(posteam)) |>
  group_by(game_id, season, week, posteam) |>
  summarise(team_dropbacks_obs = n(), .groups = "drop")

team_rush_obs <- pbp_reg |>
  filter(rush_attempt == 1, coalesce(qb_kneel, 0) != 1, !is.na(posteam)) |>
  group_by(game_id, season, week, posteam) |>
  summarise(team_rush_att_obs = n(), .groups = "drop")

qb_pass_game <- qb_pass_plays |>
  filter(season %in% PREDICTION_SEASONS) |>
  group_by(game_id, season, week, posteam, defteam, player_id) |>
  summarise(
    pass_epa  = sum(epa, na.rm = TRUE),
    dropbacks = n(),
    .groups   = "drop"
  )

qb_rush_game <- qb_rush_plays |>
  filter(season %in% PREDICTION_SEASONS) |>
  group_by(game_id, season, week, posteam, defteam, player_id) |>
  summarise(
    rush_epa = sum(epa, na.rm = TRUE),
    carries  = n(),
    .groups  = "drop"
  )

# Base table = pass-game rows (a starter game requires dropbacks); rush side
# joined on, zero-filled. QB games with carries but zero dropbacks are relief
# curiosities and are excluded by construction.
qb_outcomes <- qb_pass_game |>
  left_join(qb_rush_game,
            by = c("game_id","season","week","posteam","defteam","player_id")) |>
  mutate(
    rush_epa = coalesce(rush_epa, 0),
    carries  = coalesce(carries, 0L)
  ) |>
  left_join(team_plays_obs, by = c("game_id","season","week","posteam")) |>
  left_join(team_db_obs,    by = c("game_id","season","week","posteam")) |>
  left_join(team_rush_obs,  by = c("game_id","season","week","posteam")) |>
  mutate(
    pass_epa_per_db_obs    = pass_epa / dropbacks,
    rush_epa_per_carry_obs = if_else(carries > 0L, rush_epa / carries, NA_real_),
    total_epa              = pass_epa + rush_epa,
    team_rush_att_obs      = coalesce(team_rush_att_obs, 0L),
    carry_share_obs        = if_else(team_rush_att_obs > 0L,
                                     carries / team_rush_att_obs, 0),
    team_pass_rate_obs     = if_else(team_total_plays_obs > 0L,
                                     team_dropbacks_obs / team_total_plays_obs,
                                     NA_real_)
  )

cli_alert_success("Outcome table: {nrow(qb_outcomes)} QB player-game rows (all dropback counts)")
cli_alert_info("Rows with zero carries: {sum(qb_outcomes$carries == 0L)} ({round(100*mean(qb_outcomes$carries == 0L),1)}%)")

# ===========================================================================
# 6. PASS EFFICIENCY PRIOR FEATURES
# ===========================================================================
cli_h1("Step 6: Pass efficiency prior features")

prior_pass <- qb_pass_plays |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(player_id, season) |>
  summarise(
    prior_db       = n(),
    prior_pass_epa = sum(epa, na.rm = TRUE),
    prior_games    = n_distinct(game_id),
    .groups        = "drop"
  ) |>
  mutate(
    prior_pass_epa_per_db = if_else(prior_db >= MIN_PRIOR_DB,
                                    prior_pass_epa / prior_db,
                                    NA_real_),
    prediction_season = season + 1L
  ) |>
  select(player_id, prediction_season, prior_pass_epa_per_db, prior_games)

tier_prior <- prior_pass |>
  left_join(qb_draft |> select(player_id = gsis_id, draft_tier), by = "player_id") |>
  filter(!is.na(prior_pass_epa_per_db), !is.na(draft_tier)) |>
  group_by(draft_tier, prediction_season) |>
  summarise(tier_pass_epa_per_db = median(prior_pass_epa_per_db, na.rm = TRUE), .groups = "drop")

position_prior <- prior_pass |>
  filter(!is.na(prior_pass_epa_per_db)) |>
  group_by(prediction_season) |>
  summarise(pos_prior = median(prior_pass_epa_per_db, na.rm = TRUE), .groups = "drop")

cli_alert_info(
  "Position prior pass EPA/dropback: {paste(position_prior$prediction_season, round(position_prior$pos_prior,3), sep='=', collapse=' | ')}"
)

qb_form <- qb_outcomes |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  mutate(
    rolling_pass_epa_per_db = roll_wt_mean_prior(pass_epa_per_db_obs, ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup() |>
  left_join(prior_pass,     by = c("player_id", "season" = "prediction_season")) |>
  left_join(qb_draft |> select(gsis_id, draft_tier), by = c("player_id" = "gsis_id")) |>
  left_join(tier_prior,     by = c("draft_tier", "season" = "prediction_season")) |>
  left_join(position_prior, by = c("season" = "prediction_season")) |>
  mutate(
    is_cold_start           = is.na(prior_pass_epa_per_db),
    baseline_pass_epa_per_db = if_else(
      is_cold_start,
      coalesce(tier_pass_epa_per_db, pos_prior),
      prior_pass_epa_per_db
    ),
    form_residual = rolling_pass_epa_per_db - baseline_pass_epa_per_db
  ) |>
  select(-tier_pass_epa_per_db, -pos_prior)

pct_cold <- mean(qb_form$is_cold_start, na.rm = TRUE) * 100
cli_alert_success(
  "Pass efficiency prior: {sum(qb_form$is_cold_start, na.rm=TRUE)} cold-start rows ({round(pct_cold,1)}%)"
)

# ===========================================================================
# 7. RUSH PRODUCTION PRIOR FEATURES
# ===========================================================================
cli_h1("Step 7: Rush production prior features")

# Per-game rush priors keyed on prior-season pass games (games = dropback games,
# so a statue's zero-rush seasons still yield a valid 0-ish per-game prior).
prior_rush_raw <- qb_rush_plays |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(player_id, season) |>
  summarise(
    prior_carries        = n(),
    prior_rush_epa_total = sum(epa, na.rm = TRUE),
    .groups              = "drop"
  )

prior_rush <- prior_pass |>
  select(player_id, prediction_season, prior_games) |>
  mutate(anchor_season = prediction_season - 1L) |>
  left_join(prior_rush_raw, by = c("player_id", "anchor_season" = "season")) |>
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
  select(player_id, prediction_season,
         prior_rush_epa_pg, prior_carries_pg, prior_rush_epa_per_carry)

qb_form <- qb_form |>
  left_join(prior_rush, by = c("player_id", "season" = "prediction_season"))

cli_alert_success(
  "Rush priors: {sum(!is.na(qb_form$prior_rush_epa_pg))} rows with per-game prior, {sum(!is.na(qb_form$prior_rush_epa_per_carry))} with per-carry prior"
)

# ===========================================================================
# 8. ROLLING VOLUME + RUSH FEATURES
# ===========================================================================
cli_h1("Step 8: Rolling volume and rush features")

qb_volume <- qb_form |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  mutate(
    wt_dropbacks          = roll_wt_mean_prior(dropbacks,              ROLLING_WINDOW, DECAY_RATE),
    wt_carries            = roll_wt_mean_prior(carries,                ROLLING_WINDOW, DECAY_RATE),
    wt_carry_share        = roll_wt_mean_prior(carry_share_obs,        ROLLING_WINDOW, DECAY_RATE),
    wt_rush_epa_pg        = roll_wt_mean_prior(rush_epa,               ROLLING_WINDOW, DECAY_RATE),
    wt_rush_epa_per_carry = roll_wt_mean_prior(rush_epa_per_carry_obs, ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup()

# Team totals rolling -- group by (posteam, season)
team_rolling <- qb_outcomes |>
  distinct(game_id, season, week, posteam,
           team_total_plays_obs, team_pass_rate_obs) |>
  arrange(posteam, season, week) |>
  group_by(posteam, season) |>
  mutate(
    wt_team_total_plays = roll_wt_mean_prior(team_total_plays_obs, ROLLING_WINDOW, DECAY_RATE),
    wt_team_pass_rate   = roll_wt_mean_prior(team_pass_rate_obs,   ROLLING_WINDOW, DECAY_RATE)
  ) |>
  ungroup() |>
  select(game_id, season, posteam, wt_team_total_plays, wt_team_pass_rate)

qb_volume <- qb_volume |>
  left_join(team_rolling, by = c("game_id","season","posteam"))

cli_alert_success("Rolling volume and rush features built")

# ===========================================================================
# 9. SNAP SHARE FEATURES
# ===========================================================================
cli_h1("Step 9: Snap share features")

snap_pct_divisor <- if (max(snaps_raw$offense_pct, na.rm = TRUE) > 1.5) 100 else 1

snaps_clean <- snaps_raw |>
  filter(game_type == "REG", !is.na(pfr_player_id), !is.na(offense_pct)) |>
  mutate(snap_pct = offense_pct / snap_pct_divisor) |>
  left_join(id_xwalk, by = c("pfr_player_id" = "pfr_id")) |>
  filter(!is.na(gsis_id), gsis_id %in% qb_ids) |>
  select(gsis_id, season, week, snap_pct)

snap_rolling <- snaps_clean |>
  arrange(gsis_id, season, week) |>
  group_by(gsis_id, season) |>
  mutate(wt_snap_share = roll_wt_mean_prior(snap_pct, ROLLING_WINDOW, DECAY_RATE)) |>
  ungroup() |>
  select(gsis_id, season, week, wt_snap_share)

qb_volume <- qb_volume |>
  left_join(snap_rolling, by = c("player_id" = "gsis_id", "season", "week"))

n_snap_matched <- sum(!is.na(qb_volume$wt_snap_share))
cli_alert_info(
  "wt_snap_share: {n_snap_matched} non-NA of {nrow(qb_volume)} rows ({round(100*n_snap_matched/nrow(qb_volume),1)}%)"
)

# ===========================================================================
# 10. DEFENSIVE COMPONENT VECTOR (short_pass + deep_pass + rush)
# ===========================================================================
cli_h1("Step 10: Defensive component vector")

# Pass components measured vs ALL QB pass plays; rush component vs ALL rush
# attempts (any rusher, kneels excluded) -- a stable proxy for run-defense
# strength that a scrambling QB faces.
def_plays <- bind_rows(
  qb_pass_plays |>
    select(game_id, season, week, posteam, defteam, epa, play_cat),
  pbp_reg |>
    filter(rush_attempt == 1, coalesce(qb_kneel, 0) != 1,
           !is.na(posteam), !is.na(defteam)) |>
    transmute(game_id, season, week, posteam, defteam, epa, play_cat = "rush")
)

DEF_CATS <- c("short_pass","deep_pass","rush")

def_per_game <- def_plays |>
  group_by(game_id, season, week, defteam, play_cat) |>
  summarise(epa_sum = sum(epa, na.rm = TRUE), n_plays = n(), .groups = "drop") |>
  mutate(epa_per_play = epa_sum / n_plays)

off_per_game <- def_plays |>
  group_by(game_id, season, week, posteam, play_cat) |>
  summarise(off_epa = sum(epa, na.rm = TRUE), off_n = n(), .groups = "drop") |>
  mutate(off_epa_per_play = off_epa / off_n)

# Cross-season offensive rolling strength (window does NOT reset per season)
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

def_prior <- def_adj |>
  filter(season %in% ANCHOR_SEASONS) |>
  group_by(defteam, season, play_cat) |>
  summarise(ps_epa = mean(epa_per_play_adj, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = play_cat, values_from = ps_epa, names_prefix = "ps_") |>
  mutate(prediction_season = season + 1L) |>
  select(defteam, prediction_season, starts_with("ps_"))

for (cat in DEF_CATS) {
  col <- paste0("ps_", cat)
  if (!col %in% names(def_prior)) def_prior[[col]] <- NA_real_
}

lg_scalar <- map_dbl(DEF_CATS, function(cat) {
  mean(def_adj$epa_per_play_adj[def_adj$play_cat == cat & def_adj$season %in% ANCHOR_SEASONS],
       na.rm = TRUE)
})
names(lg_scalar) <- DEF_CATS

cli_alert_info(
  "League avg def EPA/play (anchor seasons): {paste(DEF_CATS, round(lg_scalar,3), sep='=', collapse=' | ')}"
)

def_wide <- def_adj |>
  filter(season %in% PREDICTION_SEASONS) |>
  pivot_wider(
    names_from  = play_cat,
    values_from = c(epa_per_play_adj, n_plays),
    values_fill = list(epa_per_play_adj = 0, n_plays = 0)
  )

for (cat in DEF_CATS) {
  if (!paste0("epa_per_play_adj_", cat) %in% names(def_wide)) def_wide[[paste0("epa_per_play_adj_", cat)]] <- 0
  if (!paste0("n_plays_",          cat) %in% names(def_wide)) def_wide[[paste0("n_plays_",          cat)]] <- 0L
}

def_rolling <- def_wide |>
  arrange(defteam, season, week) |>
  group_by(defteam, season) |>
  mutate(
    games_played_so_far    = row_number() - 1L,
    def_short_pass_epa_adj = roll_wt_mean_prior(epa_per_play_adj_short_pass, DEF_WINDOW, DECAY_RATE),
    def_deep_pass_epa_adj  = roll_wt_mean_prior(epa_per_play_adj_deep_pass,  DEF_WINDOW, DECAY_RATE),
    def_rush_epa_adj       = roll_wt_mean_prior(epa_per_play_adj_rush,       DEF_WINDOW, DECAY_RATE)
  ) |>
  ungroup()

def_final <- def_rolling |>
  left_join(def_prior, by = c("defteam", "season" = "prediction_season")) |>
  mutate(
    def_used_fallback = games_played_so_far < FALLBACK_MIN_GAMES,
    def_short_pass_epa_adj = if_else(
      def_used_fallback, coalesce(ps_short_pass, lg_scalar[["short_pass"]]), def_short_pass_epa_adj
    ),
    def_deep_pass_epa_adj = if_else(
      def_used_fallback, coalesce(ps_deep_pass, lg_scalar[["deep_pass"]]), def_deep_pass_epa_adj
    ),
    def_rush_epa_adj = if_else(
      def_used_fallback, coalesce(ps_rush, lg_scalar[["rush"]]), def_rush_epa_adj
    )
  ) |>
  select(game_id, season, week, defteam,
         def_short_pass_epa_adj, def_deep_pass_epa_adj, def_rush_epa_adj,
         games_played_so_far, def_used_fallback)

cli_alert_success(
  "Defensive vector built; {sum(def_final$def_used_fallback)} fallback rows"
)

# ===========================================================================
# 11. ASSEMBLE, FILTER, OUTPUT
# ===========================================================================
cli_h1("Step 11: Assemble feature table")

feature_table_raw <- qb_volume |>
  left_join(def_final,   by = c("game_id","season","week","defteam")) |>
  left_join(player_meta, by = c("player_id" = "gsis_id", "season")) |>
  select(
    # GROUPING KEYS (preserved; non-negotiable)
    player_id, player_name, posteam, defteam, season, week, game_id,
    # OBSERVED OUTCOMES: pass side eff x vol; rush side kept as level + count
    pass_epa_per_db_obs, dropbacks, pass_epa,
    rush_epa, carries, rush_epa_per_carry_obs, carry_share_obs,
    team_total_plays_obs, team_pass_rate_obs, total_epa,
    # PASS EFFICIENCY PRIOR FEATURES
    prior_pass_epa_per_db, baseline_pass_epa_per_db, rolling_pass_epa_per_db,
    form_residual, is_cold_start, draft_tier,
    # RUSH PRIOR FEATURES
    prior_rush_epa_pg, prior_carries_pg, prior_rush_epa_per_carry,
    # VOLUME + RUSH ROLLING FEATURES (recency-weighted, backward-looking)
    wt_dropbacks, wt_carries, wt_carry_share,
    wt_rush_epa_pg, wt_rush_epa_per_carry,
    wt_snap_share, wt_team_total_plays, wt_team_pass_rate,
    # DEFENSIVE COMPONENT VECTOR (three separate columns, never collapsed)
    def_short_pass_epa_adj, def_deep_pass_epa_adj, def_rush_epa_adj,
    games_played_so_far, def_used_fallback
  )

n_below <- sum(feature_table_raw$dropbacks < MIN_DROPBACKS)
cli_alert_info(
  "MIN_DROPBACKS={MIN_DROPBACKS}: dropping {n_below} relief/mop-up rows, keeping {nrow(feature_table_raw) - n_below}"
)

feature_table <- feature_table_raw |>
  filter(dropbacks >= MIN_DROPBACKS)

cli_alert_success(
  "Feature table: {nrow(feature_table)} rows x {ncol(feature_table)} columns"
)

# ===========================================================================
# 12. SAVE
# ===========================================================================
cli_h1("Step 12: Save outputs")
dir.create("data",   showWarnings = FALSE, recursive = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)

saveRDS(qb_pass_plays, "data/qb_pass_plays.rds")
saveRDS(qb_rush_plays, "data/qb_rush_plays.rds")
saveRDS(qb_outcomes,   "data/qb_outcomes.rds")
saveRDS(def_final,     "data/qb_def_rolling_final.rds")
# def_adj persisted for the deployment slate builder (10b4): the rush
# component measures ALL rushers, whose raw plays are not otherwise saved,
# and the slate's next-value carry-forward needs the per-game adjusted
# inputs. Additive artifact only -- no frozen values change.
saveRDS(def_adj,       "data/qb_def_adj.rds")
saveRDS(feature_table, "data/qb_feature_table.rds")
readr::write_csv(feature_table, "output/qb_feature_table_v1.0.csv")

cli_alert_success("data/qb_feature_table.rds")
cli_alert_success("output/qb_feature_table_v1.0.csv")

# ===========================================================================
# 13. VALIDATION SUMMARY
# ===========================================================================
cli_h1("Step 13: Validation summary")

n_rows       <- nrow(feature_table)
n_players    <- n_distinct(feature_table$player_id)
n_sw         <- n_distinct(paste(feature_table$season, feature_table$week))
seasons_pres <- sort(unique(feature_table$season))
pct_cold     <- mean(feature_table$is_cold_start, na.rm = TRUE) * 100
pct_fallback <- mean(feature_table$def_used_fallback, na.rm = TRUE) * 100

# Reconstruction gates:
#   (1) pass side: eff x vol reconstructs pass_epa
#   (2) additive:  pass_epa + rush_epa reconstructs total_epa
recon_pass <- feature_table |>
  mutate(diff = abs(pass_epa_per_db_obs * dropbacks - pass_epa))
recon_tot <- feature_table |>
  mutate(diff = abs(pass_epa + rush_epa - total_epa))

cli_alert_info("Rows:                    {n_rows}")
cli_alert_info("Unique players:          {n_players}")
cli_alert_info("Season-weeks:            {n_sw}")
cli_alert_info("Seasons present:         {paste(seasons_pres, collapse=', ')}")
cli_alert_info("Cold-start rows:         {round(pct_cold,1)}%")
cli_alert_info("Def fallback rows:       {round(pct_fallback,1)}%")
cli_alert_info("Pass recon max err:      {format(max(recon_pass$diff, na.rm=TRUE), scientific=TRUE)}")
cli_alert_info("Additive recon max err:  {format(max(recon_tot$diff, na.rm=TRUE), scientific=TRUE)}")

if (max(recon_pass$diff, na.rm = TRUE) < 1e-8) {
  cli_alert_success("pass eff x dropbacks reconstructs pass_epa -- PASS")
} else {
  cli::cli_warn("Pass reconstruction error too large -- FAIL, inspect outcome columns")
}
if (max(recon_tot$diff, na.rm = TRUE) < 1e-8) {
  cli_alert_success("pass_epa + rush_epa reconstructs total_epa -- PASS")
} else {
  cli::cli_warn("Additive reconstruction error too large -- FAIL, inspect outcome columns")
}

# Rush tier distribution (veto axis sanity). Feasibility (kneels included)
# saw 60.5/30.4/9.1; excluding kneels shifts borderline QBs down a tier, so
# expect a higher statue share here (~70/23/7).
tier_dist <- feature_table |>
  mutate(rush_tier = cut(carries, c(-Inf, 4, 8, Inf),
                         labels = c("statue (0-3)", "mover (4-7)", "scrambler (8+)"),
                         right = FALSE)) |>
  count(rush_tier) |>
  mutate(pct = round(100 * n / sum(n), 1))
cli_h2("Rush tier distribution (starter games)")
print(as.data.frame(tier_dist))

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

cli_h1("Done -- QB feature layer v1.0 frozen")
