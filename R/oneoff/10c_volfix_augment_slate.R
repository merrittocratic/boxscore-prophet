# R/oneoff/10c_volfix_augment_slate.R
# ONE-OFF, candidate-only utility for the wr-rb-vol-carryforward-fix branch
# hindcast (2026-08-31). NOT part of the production pipeline.
#
# GAP FOUND: R/10a_deployment_models_volfix.R trains RB_VOL_FEATURES/
# WR_VOL_FEATURES/TE_VOL_FEATURES with the new baseline_* prior-season
# carryforward columns, but the live slate builders (10b2_player_slate.R,
# 10b3_wr_slate.R, 10b5_te_slate.R) replicate the feature-layer logic for
# efficiency features only -- they never learned to compute the NEW
# baseline_carry_share / baseline_target_share / baseline_air_yards_share /
# baseline_snap_share / baseline_tgt_per_snap / baseline_team_total_plays
# columns, so 10c_weekly_score.R errors with "Elements ... don't exist"
# when asked to score the candidate volfix model against a real slate CSV.
#
# This script does NOT replicate that feature-layer logic (which pulls prior
# full-season PBP and applies a tier/league fallback ladder -- involved, and
# already correctly built in R/build_rb_feature_layer.R / 04a_wr / 12a_te).
# Instead it exploits a verified fact: baseline_carry_share/target_share/
# air_yards_share/snap_share/tgt_per_snap are CONSTANT within
# (player_id, season) in the already-built rb/wr/te_feature_table.rds, and
# baseline_team_total_plays is constant within (posteam, season). So for a
# player already on file this season, the season-level baseline value can be
# looked up directly rather than recomputed. This is exact, not an
# approximation -- verified n_distinct == 1 per key before use below.
#
# LIMITATION: a player who has NO 2025 rows yet in rb/wr/te_feature_table.rds
# (true Week-1-of-career rookie with zero games played, if any slipped onto
# a W13-15 slate) would have no season-level value to look up here; the
# script reports and drops any such row explicitly rather than guessing.
#
# OUTPUT: candidate-only augmented copies of the W13/14/15 slate CSVs, new
# filenames (*_volfixaug.csv) -- the real output/10b2_rb_slate_2025_w1N.csv
# etc. are read-only inputs here and are never modified.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

WEEKS <- 13:15
SEASON <- 2025L

augment <- function(pos_stem, slate_stem, ft_path, player_cols, team_col = NULL) {
  ft <- readRDS(ft_path) |> filter(season == SEASON)

  # Player-level baseline_* columns are constant within (player_id, season)
  # by construction (season-level carryforward). team_col
  # (baseline_team_total_plays) is constant within (posteam, season) instead
  # -- a traded player has two different team baselines in one season, so it
  # is looked up separately, keyed on posteam, not player_id.
  key_cols <- c("player_id", "season")
  chk <- ft |> group_by(across(all_of(key_cols))) |>
    summarise(across(all_of(player_cols), n_distinct), .groups = "drop")
  bad <- chk |> filter(if_any(all_of(player_cols), ~ .x > 1))
  if (nrow(bad) > 0) {
    cli_abort("{pos_stem}: {nrow(bad)} player-season(s) have non-constant baseline_* player columns -- lookup assumption violated")
  }
  player_lookup <- ft |> select(all_of(c(key_cols, player_cols))) |> distinct()

  team_lookup <- NULL
  if (!is.null(team_col)) {
    tkey <- c("posteam", "season")
    tchk <- ft |> group_by(across(all_of(tkey))) |> summarise(n = n_distinct(.data[[team_col]]), .groups = "drop")
    if (any(tchk$n > 1)) cli_abort("{pos_stem}: {team_col} not constant within (posteam, season)")
    team_lookup <- ft |> select(all_of(c(tkey, team_col))) |> distinct()
  }

  baseline_cols <- c(player_cols, team_col)

  for (wk in WEEKS) {
    in_path  <- sprintf("output/%s_%d_w%02d.csv", slate_stem, SEASON, wk)
    out_path <- sprintf("output/%s_%d_w%02d_volfixaug.csv", slate_stem, SEASON, wk)
    slate <- readr::read_csv(in_path, show_col_types = FALSE)

    aug <- slate |> left_join(player_lookup, by = c("player_id", "season"))
    if (!is.null(team_lookup)) {
      aug <- aug |> left_join(team_lookup, by = c("posteam", "season"))
    }

    n_missing <- sum(rowSums(is.na(aug[baseline_cols])) > 0)
    if (n_missing > 0) {
      missing_players <- aug |> filter(rowSums(is.na(across(all_of(baseline_cols)))) > 0) |>
        pull(player_name)
      cli_alert_warning("{pos_stem} W{wk}: {n_missing} row(s) with no season-level baseline (dropped): {paste(missing_players, collapse = ', ')}")
      aug <- aug |> filter(rowSums(is.na(across(all_of(baseline_cols)))) == 0)
    }

    readr::write_csv(aug, out_path)
    cli_alert_success("{out_path} ({nrow(aug)} rows, {ncol(aug)} cols)")
  }
}

cli_h1("Augmenting RB/WR/TE 2025 W13-15 slates with baseline_* volume carryforward (candidate-only)")

augment("RB", "10b2_rb_slate", "data/rb_feature_table.rds",
        c("baseline_carry_share", "baseline_target_share", "baseline_snap_share"),
        team_col = "baseline_team_total_plays")

augment("WR", "10b3_wr_slate", "data/wr_feature_table.rds",
        c("baseline_target_share", "baseline_air_yards_share", "baseline_snap_share"),
        team_col = "baseline_team_total_plays")

augment("TE", "10b5_te_slate", "data/te_feature_table.rds",
        c("baseline_target_share", "baseline_air_yards_share",
          "baseline_snap_share", "baseline_tgt_per_snap"),
        team_col = "baseline_team_total_plays")

cli_h1("Done -- candidate-only *_volfixaug.csv files written, real slate CSVs untouched")
