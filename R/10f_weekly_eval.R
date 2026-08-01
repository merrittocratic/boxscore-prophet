# R/10f_weekly_eval.R
# Weekly model-performance scorecard (the shadow-leaderboard-style eval).
# Read-only consumer of ledgers + actuals + frozen backtest artifacts.
# NEVER fails the production run: every section is tryCatch-guarded and
# degrades to a report line. Measures only -- no adaptation of anything.
#
# SECTIONS
#   A calibration: stated vs empirical per position x threshold, weekly +
#     season-to-date, judged against BACKTEST BANDS (the [2.5, 97.5]
#     percentile of weekly deltas across the 2016-2025 walk-forward recal
#     files, deployed method columns).
#   B interval coverage: 80/90 combined-interval coverage vs nominal
#     (ledger lo/hi vs feature-table total_epa).
#   C skill: Brier vs frozen climatology (backtest base rates) and vs an
#     ECR-rank-bucket baseline (accumulates in-season; "warming up" < 4 wks).
#   D watch-cell accumulator: data/10f_watch_registry.csv (FROZEN seed from
#     D20/D21/D22 receipts). Recomputes each cell on current-season data
#     with the frozen axis definitions, pools with registration-era effect
#     (n-weighted), and prints a DECISION POINT flag when pooled n >= floor
#     AND |pooled effect| >= bar. A flag pre-commits a human decision, never
#     an automatic change. Trench/continuity axes need >= 5 completed weeks
#     of current-season data (within-season terciles) -- cells report
#     "insufficient season data" before that.
#   E drift alarms (pre-committed, Steve 2026-08-01):
#     (a) a position x threshold outside its backtest weekly band TWO
#         consecutive weeks (needs the eval ledger history);
#     (b) season-to-date |delta| >= 4pp at n >= 400 (ladder cal bar).
#
# Usage: Rscript R/10f_weekly_eval.R [season]
# Cadence: weekly_run.sh full mode, after 10e (Tuesday; prior week final).
# Outputs: output/10f_weekly_eval_<season>_w<wk>.csv (scorecard)
#          output/10f_eval_ledger_<season>.csv (append-safe time series)

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10b_roster_helpers.R")   # load_season_or_empty()

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L

cli_h1("Step 10f: weekly eval -- {TARGET_SEASON}")

guard <- function(label, expr, default = NULL) {
  tryCatch(expr, error = function(e) {
    cli_alert_warning("{label}: skipped ({conditionMessage(e)})")
    default
  })
}

POOLS <- tribble(
  ~position, ~threshold, ~stem,
  "RB", "start", "p_start", "RB", "boom", "p_boom",
  "WR", "start", "p_start", "WR", "boom", "p_boom",
  "TE", "start", "p_start", "TE", "boom", "p_boom",
  "QB", "start", "p_start", "QB", "boom", "p_boom"
) |> distinct()

# ===========================================================================
# 0. GRADED SET: latest pre-kickoff ledger row x realized outcome
# ===========================================================================
ledger_files <- list.files("output", sprintf("^10c_ledger_%d_w", TARGET_SEASON),
                           full.names = TRUE)
stats <- load_season_or_empty(nflreadr::load_player_stats, TARGET_SEASON) |>
  filter(season_type == "REG") |>
  transmute(player_id, week, fp_ppr = fantasy_points_ppr, fp_std = fantasy_points)

graded <- if (length(ledger_files) > 0 && nrow(stats) > 0) {
  map(ledger_files, read_csv, show_col_types = FALSE) |>
    list_rbind() |>
    group_by(player_id, week) |>
    slice_tail(n = 1) |>                      # latest pre-kickoff (append order)
    ungroup() |>
    inner_join(stats, by = c("player_id", "week")) |>
    mutate(fp = if_else(position == "QB", fp_std, fp_ppr),
           hit_start = as.numeric(fp >= thresh_start),
           hit_boom  = as.numeric(fp >= thresh_boom))
} else tibble()

completed_weeks <- if (nrow(graded) > 0) sort(unique(graded$week)) else integer(0)
LATEST_W <- if (length(completed_weeks) > 0) max(completed_weeks) else 0L
cli_alert_info("Graded player-weeks: {nrow(graded)} across {length(completed_weeks)} completed week{?s}")

# ===========================================================================
# A. CALIBRATION vs BACKTEST BANDS
# ===========================================================================
cli_h1("10f section A: calibration vs backtest bands")

fp_maps <- readRDS("data/fp_recal_maps.rds")
te_maps <- readRDS("data/te_fp_recal_maps.rds")
qb_maps <- readRDS("data/qb_fp_recal_maps.rds")
col_of <- function(stem, method) if (method == "raw") stem else paste0(stem, "_", method)
DEPLOYED <- list(
  RB = list(file = "output/06c_recal_probabilities.csv", pos = "RB",
            start = col_of("p_start", fp_maps[["RB_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["RB_20+"]]$method)),
  WR = list(file = "output/06c_recal_probabilities.csv", pos = "WR",
            start = col_of("p_start", fp_maps[["WR_15+"]]$method),
            boom  = col_of("p_boom",  fp_maps[["WR_20+"]]$method)),
  TE = list(file = "output/12e_te_recal_probabilities.csv", pos = "TE",
            start = col_of("p_start", te_maps[["TE_12+"]]$method),
            boom  = col_of("p_boom",  te_maps[["TE_17+"]]$method)),
  QB = list(file = "output/09b_qb_recal_probabilities.csv", pos = NA,
            start = col_of("p_start", qb_maps[["QB_20+"]]$method),
            boom  = col_of("p_boom",  qb_maps[["QB_25+"]]$method))
)

bands <- guard("backtest bands", {
  imap(DEPLOYED, function(cfg, pos) {
    df <- read_csv(cfg$file, show_col_types = FALSE)
    if (!is.na(cfg$pos)) df <- df |> filter(position == cfg$pos)
    map(c(start = "start", boom = "boom"), function(th) {
      col <- cfg[[th]]
      hit <- paste0("hit_", th)
      wk <- df |>
        group_by(season, week) |>
        summarise(delta = 100 * (mean(as.numeric(.data[[hit]])) - mean(.data[[col]])),
                  base_rate = mean(as.numeric(.data[[hit]])),
                  brier = mean((.data[[col]] - as.numeric(.data[[hit]]))^2),
                  .groups = "drop")
      tibble(position = pos, threshold = th,
             band_lo = quantile(wk$delta, 0.025), band_hi = quantile(wk$delta, 0.975),
             climatology = mean(wk$base_rate), bt_brier = mean(wk$brier))
    }) |> list_rbind()
  }) |> list_rbind()
})

calib <- if (nrow(graded) > 0) {
  bind_rows(
    graded |> group_by(position) |>
      summarise(threshold = "start", n = n(), stated = mean(p_start),
                emp = mean(hit_start),
                brier = mean((p_start - hit_start)^2), .groups = "drop"),
    graded |> group_by(position) |>
      summarise(threshold = "boom", n = n(), stated = mean(p_boom),
                emp = mean(hit_boom),
                brier = mean((p_boom - hit_boom)^2), .groups = "drop")
  ) |>
    mutate(delta_pp = 100 * (emp - stated)) |>
    left_join(bands, by = c("position", "threshold")) |>
    mutate(std_flag = abs(delta_pp) >= 4 & n >= 400)
  # weekly (latest completed week) for the band check
} else tibble()

calib_wk <- if (nrow(graded) > 0) {
  g <- graded |> filter(week == LATEST_W)
  bind_rows(
    g |> group_by(position) |> summarise(threshold = "start", n = n(),
      delta_pp = 100 * (mean(hit_start) - mean(p_start)), .groups = "drop"),
    g |> group_by(position) |> summarise(threshold = "boom", n = n(),
      delta_pp = 100 * (mean(hit_boom) - mean(p_boom)), .groups = "drop")
  ) |>
    left_join(bands |> select(position, threshold, band_lo, band_hi),
              by = c("position", "threshold")) |>
    mutate(out_of_band = delta_pp < band_lo | delta_pp > band_hi)
} else tibble()

if (nrow(calib) > 0) {
  cli_h2("Season-to-date calibration (delta pp = emp - stated)")
  print(calib |> mutate(across(where(is.numeric), ~round(.x, 3))) |>
          as.data.frame(), row.names = FALSE)
} else if (!is.null(bands)) {
  cli_alert_info("No graded weeks yet -- bands ready: {nrow(bands)} pools")
}

# ===========================================================================
# B. INTERVAL COVERAGE
# ===========================================================================
cli_h1("10f section B: interval coverage")

coverage <- guard("coverage", {
  if (nrow(graded) == 0) tibble() else {
    actual_epa <- map(c(RB = "rb", WR = "wr", TE = "te", QB = "qb"), function(p) {
      readRDS(paste0("data/", p, "_feature_table.rds")) |>
        filter(season == TARGET_SEASON) |>
        select(player_id, week, total_epa)
    }) |> list_rbind()
    graded |>
      inner_join(actual_epa, by = c("player_id", "week")) |>
      group_by(position) |>
      summarise(n = n(),
                cov_80 = mean(total_epa >= lo_80_tot & total_epa <= hi_80_tot),
                cov_90 = mean(total_epa >= lo_90_tot & total_epa <= hi_90_tot),
                .groups = "drop")
  }
}, default = tibble())
if (nrow(coverage) > 0) print(coverage |> mutate(across(where(is.numeric), ~round(.x, 3))) |> as.data.frame(), row.names = FALSE) else cli_alert_info("No coverage rows yet")

# ===========================================================================
# C. SKILL vs BASELINES
# ===========================================================================
cli_h1("10f section C: skill vs baselines")

skill <- guard("skill", {
  if (nrow(graded) == 0 || is.null(bands)) tibble() else {
    # climatology + bt_brier already live on calib (joined from bands in A).
    # ECR-rank-bucket baseline: needs >= 4 weekly in-season drops before the
    # bucket hit-rates are estimable; the join lands then (rank buckets
    # 1-12 / 13-24 / 25-36 / 37+ per position, hit rate accumulated STD).
    ecr_files <- list.files("data/ecr", sprintf("^ecr_%d_", TARGET_SEASON), full.names = TRUE)
    calib |>
      mutate(brier_clim = climatology * (1 - climatology),
             skill_vs_clim = 1 - brier / brier_clim,
             brier_vs_backtest = brier - bt_brier,
             ecr_baseline = if (length(ecr_files) < 4) "warming_up" else "pending_build") |>
      select(position, threshold, n, brier, brier_clim, skill_vs_clim,
             brier_vs_backtest, ecr_baseline)
  }
}, default = tibble())
if (nrow(skill) > 0) print(skill |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE) else cli_alert_info("No skill rows yet")

# ===========================================================================
# D. WATCH-CELL ACCUMULATOR
# ===========================================================================
cli_h1("10f section D: watch-cell accumulator (D20/D21/D22 registry)")

registry <- read_csv("data/10f_watch_registry.csv", show_col_types = FALSE)

# Current-season cell inputs (all guarded; empty pre-season)
watch <- guard("watch cells", {
  season_rows <- function() {
    if (nrow(graded) == 0) return(tibble())
    actual_epa <- map(c(RB = "rb", WR = "wr", TE = "te", QB = "qb"), function(p) {
      readRDS(paste0("data/", p, "_feature_table.rds")) |>
        filter(season == TARGET_SEASON) |> select(player_id, week, total_epa)
    }) |> list_rbind()
    graded |> left_join(actual_epa, by = c("player_id", "week")) |>
      mutate(resid = total_epa - pred_tot)
  }
  rows <- season_rows()
  n_weeks <- length(completed_weeks)

  # -- axis: wind (game slates carry point-in-time kickoff forecasts)
  windy_games <- {
    fs <- list.files("output", sprintf("^10b_game_slate_%d_w", TARGET_SEASON), full.names = TRUE)
    if (length(fs) > 0) map(fs, read_csv, show_col_types = FALSE) |> list_rbind() |>
        filter(!is_indoor, !is.na(wind_mph), wind_mph >= 15) |> pull(game_id) else character(0)
  }
  # -- axis: rookie tier
  rk <- nflreadr::load_players() |>
    transmute(player_id = gsis_id, rookie_season, draft_round)
  # -- axes: trench terciles + continuity (need >= 5 completed weeks)
  trench <- if (n_weeks >= 5) {
    pbp <- nflreadr::load_pbp(TARGET_SEASON) |>
      filter(season_type == "REG", !is.na(posteam), week <= LATEST_W)
    off <- pbp |> group_by(team = posteam) |>
      summarise(sack_rate = sum(sack == 1) / pmax(sum(qb_dropback == 1), 1),
                stuff_rate = sum(rush_attempt == 1 & qb_scramble != 1 & yards_gained <= 0) /
                  pmax(sum(rush_attempt == 1 & qb_scramble != 1), 1), .groups = "drop")
    def <- pbp |> group_by(team = defteam) |>
      summarise(sack_rate = sum(sack == 1) / pmax(sum(qb_dropback == 1), 1), .groups = "drop")
    list(own_sack_hi  = off |> filter(ntile(sack_rate, 3) == 3) |> pull(team),
         own_stuff_hi = off |> filter(ntile(stuff_rate, 3) == 3) |> pull(team),
         opp_sack_hi  = def |> filter(ntile(sack_rate, 3) == 3) |> pull(team))
  } else NULL
  cont_broken <- if (n_weeks >= 2) {
    guard("continuity", {
      OL <- c("C","G","T","OL","OT","OG","LT","RT","LG","RG")
      s <- load_season_or_empty(nflreadr::load_snap_counts, TARGET_SEASON) |>
        filter(game_type == "REG", position %in% OL) |>
        group_by(week, team) |> slice_max(offense_pct, n = 5, with_ties = FALSE) |>
        summarise(top5 = list(sort(pfr_player_id)), .groups = "drop") |>
        arrange(team, week) |> group_by(team) |>
        mutate(overlap = map2_int(top5, lag(top5),
                                  ~ if (is.null(.y)) NA_integer_ else length(intersect(.x, .y)))) |>
        ungroup() |> filter(!is.na(overlap), overlap <= 3)
      s |> select(week, team)
    }, default = tibble(week = integer(), team = character()))
  } else tibble(week = integer(), team = character())

  new_cell <- function(cell) {
    if (nrow(rows) == 0) return(tibble(n_new = 0L, effect_new = NA_real_, status = "no graded weeks"))
    sub <- switch(cell$cell_id,
      windy15_wr_start = rows |> filter(position == "WR", game_id %in% windy_games),
      cont_broken_qb   = rows |> filter(position == "QB") |>
        semi_join(cont_broken, by = c("week", "posteam" = "team")),
      own_sack_hi_qb   = if (is.null(trench)) NULL else rows |> filter(position == "QB", posteam %in% trench$own_sack_hi),
      own_stuff_hi_qb  = if (is.null(trench)) NULL else rows |> filter(position == "QB", posteam %in% trench$own_stuff_hi),
      opp_sack_hi_qb   = if (is.null(trench)) NULL else rows |> filter(position == "QB", defteam %in% trench$opp_sack_hi),
      rookie_day2_qb   = rows |> filter(position == "QB") |> inner_join(rk, by = "player_id") |>
        filter(rookie_season == TARGET_SEASON, draft_round %in% 2:3),
      rookie_day3_qb   = rows |> filter(position == "QB") |> inner_join(rk, by = "player_id") |>
        filter(rookie_season == TARGET_SEASON, draft_round >= 4 | is.na(draft_round))
    )
    if (is.null(sub)) return(tibble(n_new = 0L, effect_new = NA_real_, status = "insufficient season data (<5 wks)"))
    if (nrow(sub) == 0) return(tibble(n_new = 0L, effect_new = NA_real_, status = "0 rows so far"))
    eff <- if (cell$metric == "cal") {
      h <- paste0("hit_", cell$threshold); p <- paste0("p_", cell$threshold)
      100 * (mean(sub[[h]]) - mean(sub[[p]]))
    } else mean(sub$resid, na.rm = TRUE)
    tibble(n_new = nrow(sub), effect_new = eff, status = "accumulating")
  }

  registry |>
    rowwise() |>
    mutate(new = list(new_cell(pick(everything())))) |>
    unnest(new) |>
    mutate(
      n_pooled = n_reg + n_new,
      effect_pooled = if_else(n_new > 0,
                              (n_reg * effect_reg + n_new * effect_new) / n_pooled,
                              effect_reg),
      decision_point = n_pooled >= n_floor & abs(effect_pooled) >= bar
    ) |>
    select(cell_id, source_d, n_reg, effect_reg, n_new, effect_new,
           n_pooled, effect_pooled, n_floor, bar, decision_point, status)
}, default = registry |> mutate(n_new = 0L, status = "guarded-skip"))

print(watch |> mutate(across(where(is.numeric), ~round(.x, 2))) |> as.data.frame(), row.names = FALSE)
if (any(watch$decision_point %||% FALSE, na.rm = TRUE)) {
  cli_alert_warning("DECISION POINT: {paste(watch$cell_id[watch$decision_point], collapse=', ')} crossed pre-registered floor+bar -- pre-register next steps (ladder discipline); nothing changes automatically.")
}

# ===========================================================================
# E. DRIFT ALARMS + OUTPUTS
# ===========================================================================
cli_h1("10f section E: drift alarms + outputs")

eval_ledger_path <- sprintf("output/10f_eval_ledger_%d.csv", TARGET_SEASON)
prior_ledger <- if (file.exists(eval_ledger_path)) {
  read_csv(eval_ledger_path, show_col_types = FALSE)
} else tibble()

alarms <- character(0)
if (nrow(calib) > 0) {
  a2 <- calib |> filter(std_flag)
  if (nrow(a2) > 0) alarms <- c(alarms, sprintf(
    "STD-DELTA: %s %s at %+.1fpp (n=%d) >= 4pp bar",
    a2$position, a2$threshold, a2$delta_pp, a2$n))
  if (nrow(calib_wk) > 0 && nrow(prior_ledger) > 0) {
    prev <- prior_ledger |>
      filter(section == "weekly_band", week == LATEST_W - 1L, out_of_band == TRUE) |>
      select(position, threshold)
    a1 <- calib_wk |> filter(out_of_band) |> semi_join(prev, by = c("position", "threshold"))
    if (nrow(a1) > 0) alarms <- c(alarms, sprintf(
      "BAND x2: %s %s out of backtest band two consecutive weeks", a1$position, a1$threshold))
  }
}
if (length(alarms) > 0) walk(alarms, cli_alert_warning) else cli_alert_success("No drift alarms")

scorecard <- bind_rows(
  if (nrow(calib) > 0) calib |> mutate(section = "std_calibration", week = LATEST_W),
  if (nrow(calib_wk) > 0) calib_wk |> mutate(section = "weekly_band", week = LATEST_W),
  if (nrow(coverage) > 0) coverage |> mutate(section = "coverage", week = LATEST_W),
  if (nrow(skill) > 0) skill |> mutate(section = "skill", week = LATEST_W),
  watch |> mutate(section = "watch", week = LATEST_W)
)
out_wk <- sprintf("output/10f_weekly_eval_%d_w%02d.csv", TARGET_SEASON, LATEST_W)
write_csv(scorecard, out_wk)

updated_ledger <- bind_rows(
  if (nrow(prior_ledger) > 0) prior_ledger |> filter(week != LATEST_W) else prior_ledger,  # idempotent re-run
  scorecard
)
write_csv(updated_ledger, eval_ledger_path)

cli_alert_success("{out_wk} ({nrow(scorecard)} rows) | eval ledger: {nrow(updated_ledger)} rows")
cli_h1("Step 10f complete")
