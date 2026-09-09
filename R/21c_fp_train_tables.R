# R/21c_fp_train_tables.R
# Stage B of the D29 single-stage rebuild: build the training tables the new
# fantasy-points target actually needs. Two things this script exists for:
#
#   1. fantasy_points_ppr is NOT in data/{rb,wr,te}_feature_table.rds --
#      it's computed downstream in R/06b_fp_simulation.R from
#      nflreadr::load_player_stats(), joined on (player_id, season, week).
#      A single-stage model predicting FP directly needs it joined onto the
#      feature table itself, once, here.
#   2. The floor-free arm (A1b in the ablation, see plan doc): MIN_OPPORTUNITIES
#      (5 for RB, 3 for WR/TE) drops 34/38/52% of played weeks from training,
#      averaging ~1.5-2.0 FP -- the floor exists only because epa_per_opp is
#      undefined at zero opportunities, and fantasy points are defined at
#      zero, so the floor's reason to exist goes away with this target.
#      R/build_rb_feature_layer.R, R/04a_wr_feature_layer.R, and
#      R/12a_te_feature_layer.R each got a MIN_OPP/FT_RDS_OUT/FT_CSV_OUT env
#      seam (2026-09-06) so this floor-free variant is a real ablation arm,
#      not a hand-patched one-off. Both floor-5(RB)/3(WR,TE) tables and their
#      MIN_OPP=1 floor-free counterparts were verified byte-identical on
#      every row they share (max numeric diff 0 across all columns, one
#      known pre-existing TE duplicate row excluded from that check --
#      unrelated to this change, present identically in both variants).
#
# encode_features()/join_vegas() are copied from R/10a_deployment_models.R
# (each stage script stays self-contained, repo convention -- see R/18a/18b
# for the same pattern with the ECR crosswalk before R/21a centralized it).
#
# Usage: Rscript R/21c_fp_train_tables.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

cli_h1("21c: build FP-target training tables (base + floor-free)")

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |>
    mutate(
      draft_tier_int        = TIER_ORDER[draft_tier],
      is_cold_start_int     = as.integer(is_cold_start),
      def_used_fallback_int = as.integer(def_used_fallback)
    )
}

vegas_lines <- readRDS("data/vegas_open_lines.rds")
join_vegas <- function(ft) {
  out <- ft |> left_join(vegas_lines, by = c("game_id", "posteam"))
  cli_alert_info("Vegas join: {sum(!is.na(out$team_spread))} of {nrow(out)} rows with opener lines")
  out
}

injury_rb <- readRDS("data/injury_states_rb.rds")

# nflverse's player-stats release lags real-time (unlike load_pbp) -- caps
# at most_recent_season() even when later-season games have already been
# played. Feature-table rows past that season (e.g. 2026 W1, already in the
# tables) simply won't find a match below and drop out of the inner join,
# which is correct: there is no FP target for them yet.
#
# FETCHED PER-SEASON, not batched (2026-09-09, hardened proactively --
# same GitHub #1 pattern: most_recent_season() including the current
# season doesn't guarantee load_player_stats() actually has that season's
# data yet, and a batched multi-season fetch does not gracefully skip one
# bad season). Reachable via the S1 shadow retrain in weekly_run.sh
# (`|| true` guarded at the shell level, so it wouldn't crash the real
# run, but it WOULD silently break the shadow retrain every week without
# this).
FP_SEASONS <- 2014:nflreadr::most_recent_season()
REQ_STATS_COLS <- c("player_id", "season", "week", "season_type", "fantasy_points_ppr")
fp_weekly <- map(FP_SEASONS, function(s) {
  d <- tryCatch(load_player_stats(s), error = function(e) {
    cli_alert_warning("Player stats unavailable for {s} ({conditionMessage(e)}) -- skipping (expected before that season's games are played)")
    NULL
  })
  if (!is.null(d) && !all(REQ_STATS_COLS %in% names(d))) {
    cli_alert_warning("Player stats for {s} came back missing required columns ({paste(setdiff(REQ_STATS_COLS, names(d)), collapse=', ')}) -- skipping (expected before that season's games are played)")
    d <- NULL
  }
  d
}) |> compact() |> list_rbind() |>
  filter(season_type == "REG", !is.na(player_id), !is.na(fantasy_points_ppr)) |>
  select(player_id, season, week, fantasy_points_ppr)
if (nrow(fp_weekly) == 0) {
  cli_abort("No player stats fetched for ANY season in {paste(range(FP_SEASONS), collapse='-')} -- this is a real failure (network/nflreadr issue), not the expected single-new-season gap")
}

cli_alert_info("Weekly PPR fantasy points: {nrow(fp_weekly)} player-game rows")

# ---------------------------------------------------------------------------
# One build function, reused for the base and floor-free variant of each
# position -- identical logic, different input/output paths.
# ---------------------------------------------------------------------------
build_fp_table <- function(position, ft_path, has_injury = FALSE) {
  ft <- readRDS(ft_path) |> encode_features()
  if (has_injury) {
    ft <- ft |> left_join(injury_rb, by = c("player_id", "season", "week"))
  }
  ft <- ft |> join_vegas()

  before <- nrow(ft)
  out <- ft |>
    filter(!is.na(player_id)) |>
    inner_join(fp_weekly, by = c("player_id", "season", "week"))

  cli_alert_success(
    "{position} [{basename(ft_path)}]: {before} rows -> {sum(is.na(ft$player_id))} dropped (no player_id) -> {nrow(out)} with a matched FP target"
  )
  out
}

specs <- tribble(
  ~position, ~ft_path,                                  ~has_injury, ~out_path,
  "RB",      "data/rb_feature_table.rds",                TRUE,       "data/fp_train_rb.rds",
  "RB",      "data/rb_feature_table_floorfree.rds",       TRUE,       "data/fp_train_rb_floorfree.rds",
  "WR",      "data/wr_feature_table.rds",                 FALSE,      "data/fp_train_wr.rds",
  "WR",      "data/wr_feature_table_floorfree.rds",       FALSE,      "data/fp_train_wr_floorfree.rds",
  "TE",      "data/te_feature_table.rds",                 FALSE,      "data/fp_train_te.rds",
  "TE",      "data/te_feature_table_floorfree.rds",       FALSE,      "data/fp_train_te_floorfree.rds"
)

pwalk(specs, function(position, ft_path, has_injury, out_path) {
  out <- build_fp_table(position, ft_path, has_injury)
  saveRDS(out, out_path)
})

cli_h1("21c complete -- data/fp_train_<pos>[_floorfree].rds written for RB/WR/TE")
