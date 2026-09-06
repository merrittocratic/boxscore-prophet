# R/13f_vegas_closure_check.R
# Rung 2 ship pass, final gate: did the Vegas features CLOSE the conditional
# calibration holes 13a found in the published probabilities?
#
# Re-runs the 13a cell-3 measurement (shipped-product honesty by Vegas
# bucket) against the REBUILT chain: 13e canonical fold predictions ->
# 06b/12d/09a sims -> 06c/12e/09b recal maps (fresh picks). Buckets use
# CLOSING-line implied totals exactly as 13a did -- the opener is the
# trained feature, the closer is the honest environment stratifier, and
# keeping the stratifier fixed makes every cell directly comparable to the
# 13a receipts.
#
# PRE-COMMITTED CLOSURE BOUND (stated 2026-07-26 BEFORE this script ran,
# recorded in the session before the chain rebuild): the rebuild CLOSES
# rung 2 iff ZERO cells fire the original 13a trigger (|stated - empirical|
# >= 3pp at n >= 500). The +-2pp conditional-honesty standard the volume
# strata meet is reported as the aspirational target per cell. A surviving
# trigger cell = the rung does NOT close; investigate before deployment.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS <- 2014L:2025L
PROCEED_CAL_PP    <- 3.0
PROCEED_CAL_MIN_N <- 500L
fmt <- function(x, d = 1) sprintf("%+.*f", d, x)

cli_h1("13f: Vegas-bucket closure check (vs the 13a receipts)")

# Stratifier: closing lines by default (the 13a standard). BUCKETS=open
# re-buckets by the OPENER lines the model actually consumes -- the
# ex-ante-fair variant: a model can only be asked for conditional honesty
# w.r.t. information it is allowed to know at lock. Divergence between the
# two runs MEASURES the priced-in late-week information (cost of $0 lines).
BUCKETS <- Sys.getenv("BUCKETS", "close")
if (BUCKETS == "open") {
  team_lines <- readRDS("data/vegas_open_lines.rds")
  cli_alert_info("Stratifier: OPENER lines (ex-ante-fair variant)")
} else {
  sched <- nflreadr::load_schedules(SEASONS) |> filter(game_type == "REG")
  team_lines <- bind_rows(
    sched |> transmute(game_id, posteam = home_team, team_spread =  spread_line, total_line),
    sched |> transmute(game_id, posteam = away_team, team_spread = -spread_line, total_line)
  ) |>
    mutate(implied_total = (total_line + team_spread) / 2)
  cli_alert_info("Stratifier: closing lines (13a standard)")
}

spread_bucket <- function(s) cut(s, c(-Inf, -6.5, -2.5, 2.5, 6.5, Inf),
  labels = c("big_dog", "dog", "close", "fav", "big_fav"))
itotal_bucket <- function(it) cut(it, c(-Inf, 20, 26, Inf),
  labels = c("low_implied", "mid_implied", "high_implied"))

team_key <- function(table_path) {
  readRDS(table_path) |>
    filter(!is.na(player_id)) |>
    distinct(player_id, season, week, game_id, posteam)
}

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

cli_alert_info("Deployed methods: {paste(names(fp_maps), map_chr(fp_maps, 'method'), collapse = ' | ')} | {paste(names(te_maps), map_chr(te_maps, 'method'), collapse = ' | ')} | {paste(names(qb_maps), map_chr(qb_maps, 'method'), collapse = ' | ')}")

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
                .groups = "drop") |> mutate(threshold = "start"),
    df |> group_by(position, bucket = .data[[bucket_col]]) |>
      summarise(n = n(), stated = mean(p_boom), emp = mean(as.numeric(hit_boom)),
                .groups = "drop") |> mutate(threshold = "boom")
  ) |> mutate(delta_pp = 100 * (emp - stated), axis = axis_name)
}

cells <- bind_rows(cal_cell(cal_rows, "sb", "spread"),
                   cal_cell(cal_rows, "ib", "implied_total"))

# Side-by-side vs the 13a receipts
before <- readr::read_csv("output/13a_vegas_calibration.csv", show_col_types = FALSE) |>
  select(position, bucket, threshold, axis, delta_pp_before = delta_pp)
compare <- cells |>
  left_join(before, by = c("position", "bucket", "threshold", "axis")) |>
  arrange(axis, position, threshold, bucket)

cli_h2("All cells: before (13a) vs after (rebuilt chain), delta pp")
print(compare |>
        mutate(before = fmt(delta_pp_before), after = fmt(delta_pp)) |>
        select(axis, position, threshold, bucket, n, before, after) |>
        as.data.frame(), row.names = FALSE)

trigger <- cells |> filter(n >= PROCEED_CAL_MIN_N, abs(delta_pp) >= PROCEED_CAL_PP)
target_misses <- cells |> filter(n >= PROCEED_CAL_MIN_N, abs(delta_pp) > 2.0)

cli_h1("13f verdict (pre-committed closure bound)")
worst <- cells |> filter(n >= PROCEED_CAL_MIN_N) |> slice_max(abs(delta_pp), n = 1)
cli_alert_info("Worst cell at n>={PROCEED_CAL_MIN_N}: {worst$position} {worst$threshold} {worst$bucket} {fmt(worst$delta_pp)}pp (was {fmt(compare$delta_pp_before[compare$position == worst$position & compare$threshold == worst$threshold & compare$bucket == worst$bucket & compare$axis == worst$axis])}pp)")

if (nrow(trigger) == 0) {
  cli_alert_success("CLOSED: zero cells fire the 13a trigger (|delta| >= {PROCEED_CAL_PP}pp at n >= {PROCEED_CAL_MIN_N}). Rung 2 closes.")
  if (nrow(target_misses) > 0) {
    cli_alert_info("{nrow(target_misses)} cell{?s} above the +-2pp aspirational target (printed above) -- acceptable, watch-listed.")
  } else {
    cli_alert_success("All cells also within the +-2pp conditional-honesty target.")
  }
} else {
  cli_alert_danger("NOT CLOSED: {nrow(trigger)} cell{?s} still fire the trigger -- investigate before deployment.")
  print(trigger |> mutate(delta_pp = fmt(delta_pp)) |> as.data.frame(), row.names = FALSE)
}

out_13f <- if (BUCKETS == "open") "output/13f_vegas_closure_openbuckets.csv" else "output/13f_vegas_closure.csv"
readr::write_csv(compare, out_13f)
cli_alert_success("{out_13f}")
cli_h1("13f complete")
