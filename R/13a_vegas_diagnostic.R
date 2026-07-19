# R/13a_vegas_diagnostic.R
# Ablation ladder rung 2, step 0: SIZE the Vegas-lines signal before building
# any feature layer. Pre-registered (building_in_public_log.md 2026-07-18):
# "Vegas lines (spread, total, implied team total from load_schedules): the
# cheap control. Expect flat RMSE on the solved component; run it to have
# the receipt."
#
# CLOSING-LINE CAVEAT (design, stated up front): nflverse schedule lines are
# CLOSING lines -- not Friday-lock reconstructable, so they can never enter
# a trained feature set as-is. This diagnostic is therefore an UPPER-BOUND
# probe: closing lines contain strictly more information than any line
# available at Friday lock. If the closing line is flat against the shipped
# models, the family is dead (including any paid odds feed -- per the
# paid-data policy, odds are bought only if this ablation shows signal).
# If it shows signal, the A/B step must source point-in-time lines.
#
# THREE CELLS, measured on SHIPPED backtest fold residuals:
#   1. PRIMARY -- volume-model residual (observed - predicted opportunities;
#      QB: dropbacks and carries) by spread bucket and implied-total bucket,
#      per position, plus OLS slope and Spearman vs the continuous lines.
#   2. Total-EPA residual by implied-total bucket, per position.
#   3. Conditional honesty of the SHIPPED product probabilities (the
#      deployed recal method per position x threshold) by Vegas bucket:
#      stated vs empirical hit rate.
#
# PRE-STATED EXPECTATIONS (2026-07-19, before first run):
#   - Volume: flat; |mean residual| < 0.5 opportunities in every bucket
#     (RB-vs-spread is the one cell with a live game-script story; still
#     expected < 1.0 because wt_team_total_plays + defense features absorb
#     most of it)
#   - Total-EPA residual: flat across implied-total buckets
#   - Conditional honesty: within +-2pp per Vegas stratum
#
# PRE-COMMITTED PROCEED RULE (locked before the run; not a tolerance to
# widen after seeing results):
#   Rung 2 advances to a trained-feature A/B only if
#     (a) any position x bucket with n >= 300 has |mean vol residual| >=
#         1.0 opportunities (>= 2.0 for QB dropbacks), OR
#     (b) any position x threshold x bucket with n >= 500 has
#         |stated - empirical| >= 3pp.
#   Otherwise the null is PUBLISHED (log + README) and the ladder moves to
#   rung 3 (weather).
# VALIDITY GATE: line coverage >= 95% of joined player-weeks, and the
# spread sign convention must verify against game results (slope of
# home-margin on spread_line ~ +1) or the run aborts.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS <- 2014L:2025L
fmt <- function(x, d = 2) sprintf("%+.*f", d, x)

PROCEED_VOL_OPP   <- 1.0
PROCEED_VOL_DB    <- 2.0
PROCEED_VOL_MIN_N <- 300L
PROCEED_CAL_PP    <- 3.0
PROCEED_CAL_MIN_N <- 500L

# ===========================================================================
# 1. VEGAS LINES PER TEAM-GAME
# ===========================================================================

cli_h1("13a step 1: Vegas lines from load_schedules ({min(SEASONS)}-{max(SEASONS)})")

sched <- nflreadr::load_schedules(SEASONS) |>
  filter(game_type == "REG")

# Sign-convention verification: nflverse documents spread_line as positive
# when the home team is favored; `result` = home - away margin. The slope
# of result on spread_line must be ~ +1 or every downstream sign is wrong.
sign_fit <- lm(result ~ spread_line, data = sched)
slope <- unname(coef(sign_fit)["spread_line"])
cli_alert_info("Sign check: home margin ~ spread_line slope = {round(slope, 3)} (want ~ +1)")
if (!is.finite(slope) || slope < 0.5) {
  cli_abort("Spread sign convention failed verification -- do not trust any cell below.")
}

team_lines <- bind_rows(
  sched |> transmute(game_id, posteam = home_team, team_spread =  spread_line, total_line),
  sched |> transmute(game_id, posteam = away_team, team_spread = -spread_line, total_line)
) |>
  mutate(implied_total = (total_line + team_spread) / 2)

na_lines <- mean(is.na(team_lines$team_spread) | is.na(team_lines$total_line))
cli_alert_info("Line coverage: {round(100 * (1 - na_lines), 2)}% of team-games")
if (na_lines > 0.05) cli_abort("Line coverage below 95% -- diagnostic invalid.")

spread_bucket <- function(s) cut(s, c(-Inf, -6.5, -2.5, 2.5, 6.5, Inf),
  labels = c("big_dog", "dog", "close", "fav", "big_fav"))
itotal_bucket <- function(it) cut(it, c(-Inf, 20, 26, Inf),
  labels = c("low_implied", "mid_implied", "high_implied"))

# ===========================================================================
# 2. CELL 1 + 2: FOLD RESIDUALS BY VEGAS BUCKET
# ===========================================================================

cli_h1("13a step 2: shipped fold residuals by Vegas bucket")

# (player_id, season, week) -> (game_id, posteam) via frozen feature tables
team_key <- function(table_path) {
  readRDS(table_path) |>
    filter(!is.na(player_id)) |>
    distinct(player_id, season, week, game_id, posteam)
}

POS <- list(
  RB = list(preds = "output/11c_rb_injury_fold_predictions.csv",
            table = "data/rb_feature_table.rds"),
  WR = list(preds = "output/04c_wr_asym_fold_predictions.csv",
            table = "data/wr_feature_table.rds"),
  TE = list(preds = "output/12c_te_asym_fold_predictions.csv",
            table = "data/te_feature_table.rds")
)

resid_rows <- imap(POS, function(cfg, pos) {
  readr::read_csv(cfg$preds, show_col_types = FALSE) |>
    filter(!is.na(player_id)) |>
    transmute(position = pos, player_id, season, week,
              vol_resid = as.numeric(opportunities) - pred_vol,
              tot_resid = total_epa - pred_tot) |>
    inner_join(team_key(cfg$table), by = c("player_id", "season", "week")) |>
    inner_join(team_lines, by = c("game_id", "posteam"))
}) |> list_rbind()

# QB: dropback volume is the primary axis; carries secondary; const-additive tot
qb_rows <- readr::read_csv("output/08c_qb_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id)) |>
  transmute(position = "QB", player_id, season, week,
            vol_resid = as.numeric(dropbacks) - pred_db,
            carry_resid = as.numeric(carries) - pred_carry,
            tot_resid = total_epa - pred_tot) |>
  inner_join(team_key("data/qb_feature_table.rds"), by = c("player_id", "season", "week")) |>
  inner_join(team_lines, by = c("game_id", "posteam"))

all_resid <- bind_rows(resid_rows, qb_rows) |>
  filter(!is.na(team_spread)) |>
  mutate(sb = spread_bucket(team_spread), ib = itotal_bucket(implied_total))

join_rate <- all_resid |> count(position)
cli_alert_success("Joined rows: {paste(join_rate$position, join_rate$n, sep='=', collapse=' | ')}")

cell1_spread <- all_resid |>
  group_by(position, bucket = sb) |>
  summarise(n = n(), mean_vol_resid = mean(vol_resid),
            se = sd(vol_resid) / sqrt(n()), .groups = "drop") |>
  mutate(axis = "spread")
cell1_itotal <- all_resid |>
  group_by(position, bucket = ib) |>
  summarise(n = n(), mean_vol_resid = mean(vol_resid),
            se = sd(vol_resid) / sqrt(n()), .groups = "drop") |>
  mutate(axis = "implied_total")
cell1 <- bind_rows(cell1_spread, cell1_itotal)

cli_h2("Cell 1: volume residual (obs - pred opportunities; QB = dropbacks)")
print(cell1 |>
        mutate(mean_vol_resid = fmt(mean_vol_resid), se = round(se, 2)) |>
        arrange(position, axis, bucket) |>
        as.data.frame(), row.names = FALSE)

# QB carries as a secondary volume axis (game script: trailing QBs scramble?)
cell1_qb_carry <- qb_rows |>
  filter(!is.na(team_spread)) |>
  mutate(sb = spread_bucket(team_spread)) |>
  group_by(bucket = sb) |>
  summarise(n = n(), mean_carry_resid = mean(carry_resid), .groups = "drop")
cli_h2("QB carries residual by spread bucket (secondary)")
print(cell1_qb_carry |> mutate(mean_carry_resid = fmt(mean_carry_resid)) |>
        as.data.frame(), row.names = FALSE)

slopes <- all_resid |>
  group_by(position) |>
  summarise(
    n = n(),
    vol_slope_per_7pt_spread = 7 * coef(lm(vol_resid ~ team_spread))[["team_spread"]],
    vol_spearman_spread      = cor(vol_resid, team_spread, method = "spearman"),
    vol_slope_per_3pt_itotal = 3 * coef(lm(vol_resid ~ implied_total))[["implied_total"]],
    tot_slope_per_3pt_itotal = 3 * coef(lm(tot_resid ~ implied_total))[["implied_total"]],
    tot_spearman_itotal      = cor(tot_resid, implied_total, method = "spearman"),
    .groups = "drop"
  )
cli_h2("Continuous slopes (per 7 spread points / per 3 implied-total points)")
print(slopes |> mutate(across(where(is.numeric), ~ round(.x, 3))) |>
        as.data.frame(), row.names = FALSE)

cell2 <- all_resid |>
  group_by(position, bucket = ib) |>
  summarise(n = n(), mean_tot_resid = mean(tot_resid),
            se = sd(tot_resid) / sqrt(n()), .groups = "drop")
cli_h2("Cell 2: total-EPA residual by implied-total bucket")
print(cell2 |> mutate(mean_tot_resid = fmt(mean_tot_resid), se = round(se, 2)) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 3. CELL 3: CONDITIONAL HONESTY OF SHIPPED PROBABILITIES BY VEGAS BUCKET
# ===========================================================================

cli_h1("13a step 3: shipped-product calibration by Vegas bucket")

# Deployed method per position x threshold, read from the shipped maps so
# this tracks what actually publishes (same resolution logic as 10c recon).
fp_maps <- readRDS("data/fp_recal_maps.rds")
te_maps <- readRDS("data/te_fp_recal_maps.rds")
qb_maps <- readRDS("data/qb_fp_recal_maps.rds")
col_of <- function(stem, method) if (method == "raw") stem else paste0(stem, "_", method)

RECAL <- list(
  RB = list(file = "output/06c_recal_probabilities.csv", filter_pos = "RB",
            table = "data/rb_feature_table.rds",
            start = col_of("p_start", fp_maps[["RB_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["RB_20+"]]$method)),
  WR = list(file = "output/06c_recal_probabilities.csv", filter_pos = "WR",
            table = "data/wr_feature_table.rds",
            start = col_of("p_start", fp_maps[["WR_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["WR_20+"]]$method)),
  TE = list(file = "output/12e_te_recal_probabilities.csv", filter_pos = "TE",
            table = "data/te_feature_table.rds",
            start = col_of("p_start", te_maps[["TE_12+"]]$method),
            boom  = col_of("p_boom",  te_maps[["TE_17+"]]$method)),
  QB = list(file = "output/09b_qb_recal_probabilities.csv", filter_pos = NA,
            table = "data/qb_feature_table.rds",
            start = col_of("p_start", qb_maps[["QB_20+"]]$method),
            boom  = col_of("p_boom",  qb_maps[["QB_25+"]]$method))
)

cal_rows <- imap(RECAL, function(cfg, pos) {
  df <- readr::read_csv(cfg$file, show_col_types = FALSE)
  if (!is.na(cfg$filter_pos)) df <- df |> filter(position == cfg$filter_pos)
  df |>
    transmute(position = pos, player_id, season, week,
              p_start = .data[[cfg$start]], p_boom = .data[[cfg$boom]],
              hit_start, hit_boom) |>
    inner_join(team_key(cfg$table), by = c("player_id", "season", "week")) |>
    inner_join(team_lines, by = c("game_id", "posteam"))
}) |> list_rbind() |>
  filter(!is.na(team_spread)) |>
  mutate(sb = spread_bucket(team_spread), ib = itotal_bucket(implied_total))

cal_cell <- function(df, bucket_col, axis_name) {
  bind_rows(
    df |> group_by(position, bucket = .data[[bucket_col]]) |>
      summarise(n = n(), stated = mean(p_start), emp = mean(as.numeric(hit_start)),
                .groups = "drop") |>
      mutate(threshold = "start"),
    df |> group_by(position, bucket = .data[[bucket_col]]) |>
      summarise(n = n(), stated = mean(p_boom), emp = mean(as.numeric(hit_boom)),
                .groups = "drop") |>
      mutate(threshold = "boom")
  ) |> mutate(delta_pp = 100 * (emp - stated), axis = axis_name)
}

cell3 <- bind_rows(cal_cell(cal_rows, "sb", "spread"),
                   cal_cell(cal_rows, "ib", "implied_total"))

cli_h2("Cell 3: shipped probability honesty by Vegas bucket (delta pp = emp - stated)")
print(cell3 |>
        mutate(stated = round(stated, 3), emp = round(emp, 3),
               delta_pp = fmt(delta_pp, 1)) |>
        arrange(position, threshold, axis, bucket) |>
        as.data.frame(), row.names = FALSE)

# ===========================================================================
# 4. VERDICT AGAINST THE PRE-COMMITTED PROCEED RULE
# ===========================================================================

cli_h1("13a verdict (pre-committed rule)")

vol_trigger <- cell1 |>
  mutate(thresh = if_else(position == "QB", PROCEED_VOL_DB, PROCEED_VOL_OPP)) |>
  filter(n >= PROCEED_VOL_MIN_N, abs(mean_vol_resid) >= thresh)

cal_trigger <- cell3 |>
  filter(n >= PROCEED_CAL_MIN_N, abs(delta_pp) >= PROCEED_CAL_PP)

if (nrow(vol_trigger) == 0 && nrow(cal_trigger) == 0) {
  cli_alert_success("NO TRIGGER: all volume cells < {PROCEED_VOL_OPP} opp (QB < {PROCEED_VOL_DB} db) and all calibration cells < {PROCEED_CAL_PP}pp at the pre-committed n floors.")
  cli_alert_success("RUNG 2 VERDICT: NULL -- publish the receipt; ladder moves to rung 3 (weather). No odds feed purchase.")
} else {
  if (nrow(vol_trigger) > 0) {
    cli_alert_warning("VOLUME TRIGGER ({nrow(vol_trigger)} cell{?s}):")
    print(vol_trigger |> mutate(mean_vol_resid = fmt(mean_vol_resid)) |>
            as.data.frame(), row.names = FALSE)
  }
  if (nrow(cal_trigger) > 0) {
    cli_alert_warning("CALIBRATION TRIGGER ({nrow(cal_trigger)} cell{?s}):")
    print(cal_trigger |> mutate(delta_pp = fmt(delta_pp, 1)) |>
            as.data.frame(), row.names = FALSE)
  }
  cli_alert_warning("RUNG 2 VERDICT: PROCEED to 13b feature A/B on the triggered position(s) -- point-in-time line source required before anything trains.")
}

# ===========================================================================
# 5. SAVE RECEIPTS
# ===========================================================================

cli_h1("Save receipts")
readr::write_csv(cell1,  "output/13a_vegas_vol_residuals.csv")
readr::write_csv(cell2,  "output/13a_vegas_epa_residuals.csv")
readr::write_csv(cell3,  "output/13a_vegas_calibration.csv")
readr::write_csv(slopes, "output/13a_vegas_slopes.csv")
cli_alert_success("output/13a_vegas_{{vol_residuals,epa_residuals,calibration,slopes}}.csv")

cli_h1("13a complete")
