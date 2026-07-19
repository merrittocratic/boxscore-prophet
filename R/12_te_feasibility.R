# R/12_te_feasibility.R
# Step 12 (feasibility): Is the TE position a clone of the WR spine, or does
# it need its own architecture? Answered with data BEFORE any model work,
# per house discipline (same pattern as 07_qb_feasibility.R).
#
# QUESTIONS THIS SCRIPT SETTLES:
#   Q1. Thresholds: under PPR, where do TE-specific start/boom thresholds land
#       so hit rates match the ~27% start / ~13% boom of starter games that
#       made 15/20 work for RB/WR?
#   Q2. Blocking state: among high-snap games (>= 50% offensive snaps), what
#       fraction of TE weeks get < 3 targets, vs WR? If large, snap share does
#       NOT imply target volume for TEs and the WR volume features misfire.
#   Q3. TD dependence: what share of TE PPR scoring comes from receiving TDs,
#       vs WR? How TD-gated are boom games?
#   Q4. Sample: TE player-weeks at the 3-target floor, 2014-2025; is the
#       sample above the noise-floor sizes validated in the bake-off?
#   Q5. Route depth: TE aDOT distribution vs WR; does the WR layer's
#       short/deep split at 10 air yards make sense for TEs?
#   Q6. Volume stickiness: within-season lag-1 autocorrelation of targets,
#       TE vs WR (is TE volume more or less predictable week to week?).
#
# EXPECTED RESULTS (stated before running, per house stop-condition rule):
#   Q1: TE thresholds ~11-12 start / ~15-16 boom (WR 15/20 hits far too rarely)
#   Q2: 25-40% of high-snap TE weeks under 3 targets; < 10% for WR
#   Q3: TD share of PPR scoring several points higher for TE than WR
#   Q4: ~6-9k TE weeks at 3+ targets (smaller than WR, above noise floor)
#   Q5: TE aDOT ~6-7 vs WR ~10-11; 10-air-yard split calls most TE work short
# If TE mirrors WR on Q1-Q3/Q5, verdict flips to "WR clone with new constants".
#
# DATA: nflreadr weekly player stats (PPR = pipeline standard per 06),
# snap counts (pfr crosswalk join, same as 04a), rosters.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS     <- 2014L:2025L
MIN_TARGETS <- 3L      # WR pipeline starter floor (04a MIN_OPPORTUNITIES)
HIGH_SNAP   <- 0.5
WR_START    <- 15
WR_BOOM     <- 20

skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
fmt  <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)
pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)

# ===========================================================================
# 1. PULL DATA
# ===========================================================================
cli_h1("Step 1: Pull TE + WR weekly stats, snaps, rosters")

ps <- nflreadr::load_player_stats(seasons = SEASONS) |>
  filter(season_type == "REG", position %in% c("TE", "WR"),
         !is.na(player_id), !is.na(fantasy_points_ppr)) |>
  mutate(targets = coalesce(targets, 0))

rosters_raw <- nflreadr::load_rosters(SEASONS)
snaps_raw   <- nflreadr::load_snap_counts(SEASONS)

id_xwalk <- rosters_raw |>
  filter(!is.na(gsis_id), !is.na(pfr_id)) |>
  arrange(desc(season)) |>
  distinct(pfr_id, .keep_all = TRUE) |>
  select(gsis_id, pfr_id)

snap_pct_divisor <- if (max(snaps_raw$offense_pct, na.rm = TRUE) > 1.5) 100 else 1

snaps_clean <- snaps_raw |>
  filter(game_type == "REG", !is.na(pfr_player_id), !is.na(offense_pct)) |>
  mutate(snap_pct = offense_pct / snap_pct_divisor) |>
  left_join(id_xwalk, by = c("pfr_player_id" = "pfr_id")) |>
  filter(!is.na(gsis_id)) |>
  select(gsis_id, season, week, snap_pct)

ps <- ps |>
  left_join(snaps_clean,
            by = c("player_id" = "gsis_id", "season", "week"))

starters <- ps |> filter(targets >= MIN_TARGETS)

cli_alert_success(
  "Player-weeks: {nrow(ps)} total | {sum(ps$position=='TE')} TE, {sum(ps$position=='WR')} WR"
)
cli_alert_success(
  "Starter games ({MIN_TARGETS}+ targets): {sum(starters$position=='TE')} TE | {sum(starters$position=='WR')} WR"
)
n_snap <- sum(!is.na(ps$snap_pct))
cli_alert_info("Snap match rate: {pct(n_snap / nrow(ps))}")

# ===========================================================================
# 2. Q1: THRESHOLDS
# ===========================================================================
cli_h1("Step 2 (Q1): PPR thresholds -- where do TE start/boom cuts land?")

wr_s <- starters |> filter(position == "WR")
te_s <- starters |> filter(position == "TE")

wr_start_rate <- mean(wr_s$fantasy_points_ppr >= WR_START)
wr_boom_rate  <- mean(wr_s$fantasy_points_ppr >= WR_BOOM)

thresh_grid <- tibble(threshold = seq(6, 24, by = 1)) |>
  mutate(
    te_hit = map_dbl(threshold, \(t) mean(te_s$fantasy_points_ppr >= t)),
    wr_hit = map_dbl(threshold, \(t) mean(wr_s$fantasy_points_ppr >= t))
  )

te_start_thresh <- thresh_grid$threshold[which.min(abs(thresh_grid$te_hit - wr_start_rate))]
te_boom_thresh  <- thresh_grid$threshold[which.min(abs(thresh_grid$te_hit - wr_boom_rate))]

cli_alert_info("WR reference rates: P(FP>={WR_START}) = {pct(wr_start_rate)} | P(FP>={WR_BOOM}) = {pct(wr_boom_rate)}")
cli_alert_info("TE at WR cuts:      P(FP>={WR_START}) = {pct(mean(te_s$fantasy_points_ppr >= WR_START))} | P(FP>={WR_BOOM}) = {pct(mean(te_s$fantasy_points_ppr >= WR_BOOM))}")
cli_alert_success("Rate-matched TE thresholds: start ~{te_start_thresh} | boom ~{te_boom_thresh}")

qtab <- bind_rows(
  te_s |> summarise(position = "TE",
                    q25 = quantile(fantasy_points_ppr, .25),
                    q50 = quantile(fantasy_points_ppr, .50),
                    q75 = quantile(fantasy_points_ppr, .75),
                    q90 = quantile(fantasy_points_ppr, .90),
                    mean = mean(fantasy_points_ppr),
                    skew = skew(fantasy_points_ppr)),
  wr_s |> summarise(position = "WR",
                    q25 = quantile(fantasy_points_ppr, .25),
                    q50 = quantile(fantasy_points_ppr, .50),
                    q75 = quantile(fantasy_points_ppr, .75),
                    q90 = quantile(fantasy_points_ppr, .90),
                    mean = mean(fantasy_points_ppr),
                    skew = skew(fantasy_points_ppr))
)
print(as.data.frame(qtab), row.names = FALSE, digits = 3)

# ===========================================================================
# 3. Q2: BLOCKING STATE (high snaps, low targets)
# ===========================================================================
cli_h1("Step 3 (Q2): Blocking state -- do snaps imply targets?")

hs <- ps |> filter(!is.na(snap_pct), snap_pct >= HIGH_SNAP)

block_tab <- hs |>
  group_by(position) |>
  summarise(
    n_high_snap    = n(),
    under_floor    = mean(targets < MIN_TARGETS),
    zero_or_one    = mean(targets <= 1),
    cor_snap_tgt   = cor(snap_pct, targets),
    .groups = "drop"
  )
print(as.data.frame(block_tab), row.names = FALSE, digits = 3)

te_block <- block_tab |> filter(position == "TE") |> pull(under_floor)
wr_block <- block_tab |> filter(position == "WR") |> pull(under_floor)
cli_alert_success(
  "High-snap games under the {MIN_TARGETS}-target floor: TE {pct(te_block)} vs WR {pct(wr_block)}"
)

# ===========================================================================
# 4. Q3: TD DEPENDENCE
# ===========================================================================
cli_h1("Step 4 (Q3): TD dependence of scoring")

td_tab <- starters |>
  group_by(position) |>
  summarise(
    td_share_of_fp  = sum(6 * receiving_tds) / sum(fantasy_points_ppr),
    boom_rate       = mean(fantasy_points_ppr >= if_else(position[1] == "TE", te_boom_thresh, WR_BOOM)),
    boom_given_td   = mean(fantasy_points_ppr[receiving_tds >= 1] >=
                             if_else(position[1] == "TE", te_boom_thresh, WR_BOOM)),
    boom_no_td      = mean(fantasy_points_ppr[receiving_tds == 0] >=
                             if_else(position[1] == "TE", te_boom_thresh, WR_BOOM)),
    .groups = "drop"
  )
print(as.data.frame(td_tab), row.names = FALSE, digits = 3)

te_td <- td_tab |> filter(position == "TE")
cli_alert_success(
  "TE booms (>= {te_boom_thresh}) without a TD: {pct(te_td$boom_no_td)} of no-TD games vs {pct(te_td$boom_given_td)} of TD games"
)

# ===========================================================================
# 5. Q4: SAMPLE SIZE + ELITE CONCENTRATION
# ===========================================================================
cli_h1("Step 5 (Q4): Sample size and elite-tier concentration")

per_season <- te_s |> count(season, name = "te_starter_games")
cli_alert_info("TE starter games/season: min {min(per_season$te_starter_games)}, max {max(per_season$te_starter_games)}")
cli_alert_success(
  "TE sample at {MIN_TARGETS}+ targets: {nrow(te_s)} rows, {n_distinct(te_s$player_id)} players (WR: {nrow(wr_s)} rows)"
)

conc <- starters |>
  group_by(position, season, player_id) |>
  summarise(fp = sum(fantasy_points_ppr), .groups = "drop_last") |>
  arrange(desc(fp), .by_group = TRUE) |>
  mutate(rank = row_number(), total = sum(fp)) |>
  summarise(top12_share = sum(fp[rank <= 12]) / total[1], .groups = "drop_last") |>
  summarise(mean_top12_share = mean(top12_share), .groups = "drop")
print(as.data.frame(conc), row.names = FALSE, digits = 3)

# ===========================================================================
# 6. Q5: ROUTE DEPTH (aDOT)
# ===========================================================================
cli_h1("Step 6 (Q5): aDOT -- does the 10-air-yard short/deep split fit TEs?")

adot_tab <- starters |>
  filter(!is.na(receiving_air_yards), targets > 0) |>
  mutate(adot = receiving_air_yards / targets) |>
  group_by(position) |>
  summarise(
    adot_q25 = quantile(adot, .25),
    adot_med = quantile(adot, .50),
    adot_q75 = quantile(adot, .75),
    pct_games_adot_ge_10 = mean(adot >= 10),
    .groups = "drop"
  )
print(as.data.frame(adot_tab), row.names = FALSE, digits = 3)

# ===========================================================================
# 7. Q6: VOLUME STICKINESS (lag-1 target autocorrelation)
# ===========================================================================
cli_h1("Step 7 (Q6): Week-to-week target stickiness")

stick <- ps |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  filter(n() >= 6) |>
  mutate(lag_tgt = lag(targets)) |>
  ungroup() |>
  filter(!is.na(lag_tgt)) |>
  group_by(position) |>
  summarise(lag1_target_cor = cor(targets, lag_tgt), .groups = "drop")
print(as.data.frame(stick), row.names = FALSE, digits = 3)

# ===========================================================================
# 8. WRITE OUTPUTS
# ===========================================================================
cli_h1("Step 8: Write outputs")

dir.create("output", showWarnings = FALSE)
write_csv(thresh_grid, "output/12_te_threshold_grid.csv")

summary_out <- tibble(
  metric = c(
    "te_start_thresh_rate_matched", "te_boom_thresh_rate_matched",
    "te_hit_at_wr15", "te_hit_at_wr20",
    "te_highsnap_under_floor", "wr_highsnap_under_floor",
    "te_td_share_of_fp", "wr_td_share_of_fp",
    "te_starter_rows", "te_unique_players",
    "te_adot_median", "wr_adot_median",
    "te_lag1_target_cor", "wr_lag1_target_cor"
  ),
  value = c(
    te_start_thresh, te_boom_thresh,
    mean(te_s$fantasy_points_ppr >= WR_START), mean(te_s$fantasy_points_ppr >= WR_BOOM),
    te_block, wr_block,
    td_tab$td_share_of_fp[td_tab$position == "TE"],
    td_tab$td_share_of_fp[td_tab$position == "WR"],
    nrow(te_s), n_distinct(te_s$player_id),
    adot_tab$adot_med[adot_tab$position == "TE"],
    adot_tab$adot_med[adot_tab$position == "WR"],
    stick$lag1_target_cor[stick$position == "TE"],
    stick$lag1_target_cor[stick$position == "WR"]
  )
)
write_csv(summary_out, "output/12_te_feasibility_summary.csv")
cli_alert_success("Wrote output/12_te_threshold_grid.csv and output/12_te_feasibility_summary.csv")

cli_h1("VERDICT INPUTS COMPLETE -- interpret against expected results in header")
