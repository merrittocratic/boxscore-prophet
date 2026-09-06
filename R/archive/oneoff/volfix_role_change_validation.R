# Objective role-change validation for the WR/RB volume-model carryforward fix
# (04c_wr_asymmetric_conformal_volfix.R / 11c_rb_injury_volfix.R).
#
# Purpose: the informal check (hand-picked players like A.J. Brown) showed
# raw pred_vol de-compressing across seasons in the expected direction, but
# neither AUC nor conformal coverage is sensitive to within-tier re-ranking,
# so that isn't evidence the fix improves accuracy for players whose role
# actually changed year over year. This script builds a role-change cohort
# from real observed data (season-level target_share / carry_share deltas,
# no hand-picking) and compares OLD vs NEW model Week-1 pred_vol error and
# rank correlation against real Week-1 outcomes, split by role-change vs
# stable-role cohort, for both positions.
#
# Reviewable intermediate outputs are written to:
#   output/volfix_role_change_cohort_wr.csv
#   output/volfix_role_change_cohort_rb.csv
#   output/volfix_role_change_summary.csv
#
# Read-only with respect to data/deploy_models/ and data/deployment_params.rds
# (not touched).

suppressMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(cli)
})

# ---------------------------------------------------------------------------
# 1. Load outcomes (real observed data) and fold predictions (old vs new)
# ---------------------------------------------------------------------------

wr_outcomes <- readRDS("data/wr_outcomes.rds") |> filter(!is.na(player_id))
rb_outcomes <- readRDS("data/rb_outcomes.rds") |> filter(!is.na(player_id))

# NB: WR fold-prediction files carry ~3.9k rows (of 21k) with NA player_id
# (unmapped/untracked players) -- these can never be part of a real
# player-season role-change comparison, and if left in, NA==NA join
# semantics in dplyr blow the join up into a many-to-many cross product.
# Drop them here, at the source, for both positions (RB has none, WR does).
wr_old <- read_csv("output/04c_wr_asym_fold_predictions.csv",        show_col_types = FALSE) |> filter(!is.na(player_id))
wr_new <- read_csv("output/04c_wr_asym_fold_predictions_volfix.csv", show_col_types = FALSE) |> filter(!is.na(player_id))
rb_old <- read_csv("output/11c_rb_injury_fold_predictions.csv",        show_col_types = FALSE) |> filter(!is.na(player_id))
rb_new <- read_csv("output/11c_rb_injury_fold_predictions_volfix.csv", show_col_types = FALSE) |> filter(!is.na(player_id))

# ---------------------------------------------------------------------------
# 2. Season-level actual share per player-season (real observed data)
# ---------------------------------------------------------------------------

season_share <- function(outcomes, share_col) {
  outcomes |>
    filter(!is.na(.data[[share_col]])) |>
    group_by(player_id, season) |>
    summarise(
      season_share = mean(.data[[share_col]]),
      n_weeks      = n(),
      .groups = "drop"
    )
}

wr_season <- season_share(wr_outcomes, "target_share_obs")
rb_season <- season_share(rb_outcomes, "carry_share_obs")

# ---------------------------------------------------------------------------
# 3. Year-over-year delta: this season's actual share vs prior season's
#    actual share (both real, both observed -- no model predictions here)
# ---------------------------------------------------------------------------

build_role_change_table <- function(season_tbl) {
  prior <- season_tbl |>
    transmute(player_id, season = season + 1L, prior_share = season_share, prior_n_weeks = n_weeks)

  season_tbl |>
    inner_join(prior, by = c("player_id", "season")) |>
    mutate(abs_delta = abs(season_share - prior_share))
}

wr_role <- build_role_change_table(wr_season)
rb_role <- build_role_change_table(rb_season)

cli_alert_info("WR player-seasons with a prior season on record: {nrow(wr_role)}")
cli_alert_info("RB player-seasons with a prior season on record: {nrow(rb_role)}")

# ---------------------------------------------------------------------------
# 4. Data-driven threshold: 75th and 90th percentile of |delta| distribution
# ---------------------------------------------------------------------------

thresholds <- function(role_tbl) {
  q <- quantile(role_tbl$abs_delta, probs = c(0.75, 0.90), na.rm = TRUE)
  list(p75 = unname(q[1]), p90 = unname(q[2]))
}

wr_thresh <- thresholds(wr_role)
rb_thresh <- thresholds(rb_role)

cli_alert_info("WR abs_delta(target_share) p75={round(wr_thresh$p75,4)} p90={round(wr_thresh$p90,4)}")
cli_alert_info("RB abs_delta(carry_share)  p75={round(rb_thresh$p75,4)} p90={round(rb_thresh$p90,4)}")

assign_cohort <- function(role_tbl, threshold) {
  role_tbl |> mutate(cohort = if_else(abs_delta >= threshold, "role_change", "stable"))
}

wr_role_p75 <- assign_cohort(wr_role, wr_thresh$p75)
wr_role_p90 <- assign_cohort(wr_role, wr_thresh$p90)
rb_role_p75 <- assign_cohort(rb_role, rb_thresh$p75)
rb_role_p90 <- assign_cohort(rb_role, rb_thresh$p90)

# Save the primary (p75) cohort tables for review
write_csv(wr_role_p75, "output/volfix_role_change_cohort_wr.csv")
write_csv(rb_role_p75, "output/volfix_role_change_cohort_rb.csv")

# ---------------------------------------------------------------------------
# 5. Week-1 pred_vol (old vs new) vs actual Week-1 opportunities, by cohort
# ---------------------------------------------------------------------------

week1_compare <- function(old_preds, new_preds, role_tbl) {
  old_wk1 <- old_preds |>
    filter(week == 1) |>
    select(player_id, season, actual = opportunities, pred_old = pred_vol)

  new_wk1 <- new_preds |>
    filter(week == 1) |>
    select(player_id, season, actual_new = opportunities, pred_new = pred_vol)

  joined <- role_tbl |>
    select(player_id, season, cohort, abs_delta) |>
    inner_join(old_wk1, by = c("player_id", "season")) |>
    inner_join(new_wk1, by = c("player_id", "season")) |>
    mutate(
      # sanity check: actual outcome should agree between old/new fold files
      actual_mismatch = actual != actual_new,
      err_old = abs(pred_old - actual),
      err_new = abs(pred_new - actual)
    )

  n_mismatch <- sum(joined$actual_mismatch)
  if (n_mismatch > 0) {
    cli_alert_warning("{n_mismatch} rows where old/new fold files disagree on actual Week-1 opportunities (using old-file actual)")
  }

  joined
}

wr_wk1_p75 <- week1_compare(wr_old, wr_new, wr_role_p75)
wr_wk1_p90 <- week1_compare(wr_old, wr_new, wr_role_p90)
rb_wk1_p75 <- week1_compare(rb_old, rb_new, rb_role_p75)
rb_wk1_p90 <- week1_compare(rb_old, rb_new, rb_role_p90)

cli_alert_info("WR Week-1 matched rows (p75 cohort table): {nrow(wr_wk1_p75)}")
cli_alert_info("RB Week-1 matched rows (p75 cohort table): {nrow(rb_wk1_p75)}")

# ---------------------------------------------------------------------------
# 6. Error summary by cohort (mean/median abs error, n) + Spearman rank corr
#    within the role-change cohort only
# ---------------------------------------------------------------------------

error_summary <- function(wk1_tbl, position, threshold_label) {
  by_cohort <- wk1_tbl |>
    group_by(cohort) |>
    summarise(
      n            = n(),
      mae_old      = mean(err_old),
      mae_new      = mean(err_new),
      median_old   = median(err_old),
      median_new   = median(err_new),
      mae_delta    = mae_new - mae_old,          # negative = NEW better
      pct_improve  = 100 * (mae_old - mae_new) / mae_old,
      wilcox_p     = suppressWarnings(wilcox.test(err_old, err_new, paired = TRUE)$p.value),
      .groups = "drop"
    )

  overall <- wk1_tbl |>
    summarise(
      cohort       = "overall",
      n            = n(),
      mae_old      = mean(err_old),
      mae_new      = mean(err_new),
      median_old   = median(err_old),
      median_new   = median(err_new),
      mae_delta    = mae_new - mae_old,
      pct_improve  = 100 * (mae_old - mae_new) / mae_old,
      wilcox_p     = suppressWarnings(wilcox.test(err_old, err_new, paired = TRUE)$p.value)
    )

  bind_rows(by_cohort, overall) |>
    mutate(position = position, threshold = threshold_label, .before = 1)
}

spearman_summary <- function(wk1_tbl, position, threshold_label) {
  rc <- wk1_tbl |> filter(cohort == "role_change")
  tibble(
    position   = position,
    threshold  = threshold_label,
    n          = nrow(rc),
    spearman_old = suppressWarnings(cor(rc$pred_old, rc$actual, method = "spearman", use = "complete.obs")),
    spearman_new = suppressWarnings(cor(rc$pred_new, rc$actual, method = "spearman", use = "complete.obs"))
  )
}

err_wr_p75 <- error_summary(wr_wk1_p75, "WR", "p75")
err_wr_p90 <- error_summary(wr_wk1_p90, "WR", "p90")
err_rb_p75 <- error_summary(rb_wk1_p75, "RB", "p75")
err_rb_p90 <- error_summary(rb_wk1_p90, "RB", "p90")

sp_wr_p75 <- spearman_summary(wr_wk1_p75, "WR", "p75")
sp_wr_p90 <- spearman_summary(wr_wk1_p90, "WR", "p90")
sp_rb_p75 <- spearman_summary(rb_wk1_p75, "RB", "p75")
sp_rb_p90 <- spearman_summary(rb_wk1_p90, "RB", "p90")

err_all <- bind_rows(err_wr_p75, err_wr_p90, err_rb_p75, err_rb_p90)
sp_all  <- bind_rows(sp_wr_p75, sp_wr_p90, sp_rb_p75, sp_rb_p90)

write_csv(err_all, "output/volfix_role_change_error_summary.csv")
write_csv(sp_all,  "output/volfix_role_change_spearman_summary.csv")

cli_h1("Error summary (MAE of pred_vol vs actual Week-1 opportunities)")
print(as.data.frame(err_all), row.names = FALSE)

cli_h1("Spearman rank correlation (role-change cohort only, Week 1)")
print(as.data.frame(sp_all), row.names = FALSE)

cli_alert_success("Wrote output/volfix_role_change_cohort_wr.csv, _rb.csv, _error_summary.csv, _spearman_summary.csv")

# ---------------------------------------------------------------------------
# 7. Robustness check: same p75 cohort logic, but require both the current
#    and prior season to have >= MIN_WEEKS games on record, so the cohort
#    isn't picking up small-sample noise (e.g. a 1-game garbage-time share)
#    as a "role change". This should not flip the qualitative conclusion.
# ---------------------------------------------------------------------------

MIN_WEEKS <- 4

robust_cohort <- function(season_tbl) {
  build_role_change_table(season_tbl) |>
    filter(n_weeks >= MIN_WEEKS, prior_n_weeks >= MIN_WEEKS)
}

wr_role_robust <- robust_cohort(wr_season)
rb_role_robust <- robust_cohort(rb_season)

wr_robust_thresh <- unname(quantile(wr_role_robust$abs_delta, 0.75, na.rm = TRUE))
rb_robust_thresh <- unname(quantile(rb_role_robust$abs_delta, 0.75, na.rm = TRUE))

wr_role_robust <- assign_cohort(wr_role_robust, wr_robust_thresh)
rb_role_robust <- assign_cohort(rb_role_robust, rb_robust_thresh)

wr_wk1_robust <- week1_compare(wr_old, wr_new, wr_role_robust)
rb_wk1_robust <- week1_compare(rb_old, rb_new, rb_role_robust)

err_wr_robust <- error_summary(wr_wk1_robust, "WR", "p75_min4wk")
err_rb_robust <- error_summary(rb_wk1_robust, "RB", "p75_min4wk")
err_robust_all <- bind_rows(err_wr_robust, err_rb_robust)

write_csv(err_robust_all, "output/volfix_role_change_error_summary_robustness.csv")

cli_h1("Robustness check: p75 threshold, both seasons require >= {MIN_WEEKS} weeks played")
print(as.data.frame(err_robust_all), row.names = FALSE)
cli_alert_success("Wrote output/volfix_role_change_error_summary_robustness.csv")
