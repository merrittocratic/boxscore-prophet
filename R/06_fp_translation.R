# R/06_fp_translation.R
# Step 6: EPA -> PPR fantasy points translation layer.
#
# PURPOSE: Bridge between the EPA prediction intervals (model output) and the
# content-facing probabilities P(FP >= 15) and P(FP >= 20) per player per week.
#
# APPROACH:
#   1. Pull weekly PPR fantasy points from nflreadr::load_player_stats()
#   2. Join to rb_outcomes and wr_outcomes on (player_id, season, week)
#   3. Fit per-position OLS: fp_ppr ~ total_epa + opportunities
#      Regression captures average EPA->FP mapping; residual SD captures
#      translation noise (same EPA can come from different yards/TD combos)
#   4. Load fold predictions (03a_v2 for RB, 04b for WR)
#   5. Propagate both EPA and volume prediction uncertainty through the regression
#      sigma_fp = sqrt((b_epa * sigma_epa)^2 + (b_opp * sigma_opp)^2 + sigma_resid^2)
#      where sigma_epa and sigma_opp are derived from the 80% conformal intervals
#   6. Compute P(FP >= 15) and P(FP >= 20) per player-game using normal approximation
#   7. Calibration check: are the stated probabilities honest?
#
# SCORING FORMAT: PPR (1 pt per reception)
# THRESHOLDS: 15 pts (start/sit decision), 20 pts (boom week / X content)

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

PREDICTION_SEASONS <- 2014L:2025L
THRESH_START <- 15   # RB1 start/sit threshold
THRESH_BOOM  <- 20   # elite week threshold

# Normal approximation: hw_80 = 1.282 * sigma
Z_80 <- qnorm(0.90)  # 1.282

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)

# ===========================================================================
# 1. PULL WEEKLY PPR FANTASY POINTS
# ===========================================================================

cli_h1("Step 1: Pull weekly PPR fantasy points (nflreadr)")

player_stats_raw <- nflreadr::load_player_stats(
  seasons   = PREDICTION_SEASONS,
  stat_type = "offense"
)

cli_alert_info(
  "player_stats: {nrow(player_stats_raw)} rows | cols: {paste(names(player_stats_raw), collapse=', ')}"
)

fp_weekly <- player_stats_raw |>
  # NA player_id rows are unattributed stat lines; dplyr matches NA to NA in
  # joins, which cartesian-explodes against the NA rows in the outcome tables.
  filter(season_type == "REG", !is.na(fantasy_points_ppr), !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points_ppr,
         carries, rushing_yards, rushing_tds,
         receptions, targets, receiving_yards, receiving_tds)

cli_alert_success(
  "PPR fantasy points: {nrow(fp_weekly)} player-game rows, {length(unique(fp_weekly$player_id))} unique players"
)

# ===========================================================================
# 2. JOIN TO OUTCOME TABLES
# ===========================================================================

cli_h1("Step 2: Join to RB and WR outcome tables")

rb_outcomes <- readRDS("data/rb_outcomes.rds") |> filter(!is.na(player_id))
wr_outcomes <- readRDS("data/wr_outcomes.rds") |> filter(!is.na(player_id))

rb_fp <- rb_outcomes |>
  inner_join(fp_weekly, by = c("player_id", "season", "week")) |>
  mutate(position = "RB")

wr_fp <- wr_outcomes |>
  inner_join(fp_weekly, by = c("player_id", "season", "week")) |>
  mutate(position = "WR")

cli_alert_success(
  "RB join: {nrow(rb_fp)} player-game rows ({nrow(rb_outcomes)} before join)"
)
cli_alert_success(
  "WR join: {nrow(wr_fp)} player-game rows ({nrow(wr_outcomes)} before join)"
)

# Quick sanity check: FP should correlate strongly with total_epa
rb_cor <- cor(rb_fp$fantasy_points_ppr, rb_fp$total_epa, use = "complete.obs")
wr_cor <- cor(wr_fp$fantasy_points_ppr, wr_fp$total_epa, use = "complete.obs")
cli_alert_info("Pearson r(FP, total_epa): RB={round(rb_cor, 3)} | WR={round(wr_cor, 3)}")

# ===========================================================================
# 3. FIT PER-POSITION REGRESSION
# ===========================================================================

cli_h1("Step 3: Fit EPA -> PPR regression per position")

# RB: total_epa captures EPA from carries + targets; opportunities (carries+targets)
# adds back the PPR reception bonus not captured in EPA alone.
fit_rb <- lm(fantasy_points_ppr ~ total_epa + opportunities, data = rb_fp)
fit_wr <- lm(fantasy_points_ppr ~ total_epa + opportunities, data = wr_fp)

sigma_resid_rb <- sigma(fit_rb)
sigma_resid_wr <- sigma(fit_wr)

cli_h2("RB regression: fp_ppr ~ total_epa + opportunities")
print(summary(fit_rb)$coefficients)
cli_alert_info("R-squared: {round(summary(fit_rb)$r.squared, 3)}")
cli_alert_info("Residual SD (translation noise): {round(sigma_resid_rb, 3)} pts")

cli_h2("WR regression: fp_ppr ~ total_epa + opportunities")
print(summary(fit_wr)$coefficients)
cli_alert_info("R-squared: {round(summary(fit_wr)$r.squared, 3)}")
cli_alert_info("Residual SD (translation noise): {round(sigma_resid_wr, 3)} pts")

# Extract coefficients
b_epa_rb  <- coef(fit_rb)[["total_epa"]]
b_opp_rb  <- coef(fit_rb)[["opportunities"]]
b_int_rb  <- coef(fit_rb)[["(Intercept)"]]
b_epa_wr  <- coef(fit_wr)[["total_epa"]]
b_opp_wr  <- coef(fit_wr)[["opportunities"]]
b_int_wr  <- coef(fit_wr)[["(Intercept)"]]

# Residual distribution check: should be approximately normal
rb_resid_skew <- mean((residuals(fit_rb) - mean(residuals(fit_rb)))^3) /
  sd(residuals(fit_rb))^3
wr_resid_skew <- mean((residuals(fit_wr) - mean(residuals(fit_wr)))^3) /
  sd(residuals(fit_wr))^3
cli_alert_info(
  "Residual skewness: RB={round(rb_resid_skew, 2)} | WR={round(wr_resid_skew, 2)} (|skew| < 1 = normal approx ok)"
)

# ===========================================================================
# 4. LOAD FOLD PREDICTIONS
# ===========================================================================

cli_h1("Step 4: Load fold predictions")

rb_preds <- readr::read_csv("output/03a_v2_lgbm_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id))
wr_preds <- readr::read_csv("output/04b_wr_lgbm_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id))

cli_alert_success("RB fold predictions: {nrow(rb_preds)} rows")
cli_alert_success("WR fold predictions: {nrow(wr_preds)} rows")

# ===========================================================================
# 5. COMPUTE PROBABILITIES
# ===========================================================================

cli_h1("Step 5: Compute P(FP >= 15) and P(FP >= 20)")

compute_fp_probs <- function(preds, fp_data,
                             b_int, b_epa, b_opp, sigma_resid,
                             position_label) {

  # Derive sigma_epa and sigma_opp from 80% conformal half-widths.
  # hw_80 = Z_80 * sigma  =>  sigma = hw_80 / Z_80
  out <- preds |>
    mutate(
      hw_80_tot = (hi_80_tot - lo_80_tot) / 2,
      hw_80_vol = (hi_80_vol - lo_80_vol) / 2,
      sigma_epa = hw_80_tot / Z_80,
      sigma_opp = hw_80_vol / Z_80,

      # Point prediction in FP space
      mu_fp = b_int + b_epa * pred_tot + b_opp * pred_vol,

      # Total FP uncertainty: EPA model + volume model + translation residual
      sigma_fp = sqrt(
        (b_epa * sigma_epa)^2 +
        (b_opp * sigma_opp)^2 +
        sigma_resid^2
      ),

      # P(FP >= threshold) under normal approximation
      p_start = pnorm(THRESH_START, mean = mu_fp, sd = sigma_fp, lower.tail = FALSE),
      p_boom  = pnorm(THRESH_BOOM,  mean = mu_fp, sd = sigma_fp, lower.tail = FALSE),

      position = position_label
    ) |>
    # Join actual FP for calibration
    left_join(
      fp_data |>
        select(player_id, season, week, fantasy_points_ppr) |>
        distinct(player_id, season, week, .keep_all = TRUE),
      by = c("player_id", "season", "week")
    ) |>
    mutate(
      hit_start = fantasy_points_ppr >= THRESH_START,
      hit_boom  = fantasy_points_ppr >= THRESH_BOOM
    )

  out
}

rb_probs <- compute_fp_probs(
  rb_preds, rb_fp,
  b_int_rb, b_epa_rb, b_opp_rb, sigma_resid_rb,
  "RB"
)

wr_probs <- compute_fp_probs(
  wr_preds, wr_fp,
  b_int_wr, b_epa_wr, b_opp_wr, sigma_resid_wr,
  "WR"
)

all_probs <- bind_rows(rb_probs, wr_probs)

cli_alert_success(
  "Probabilities computed: {nrow(all_probs)} total rows | {sum(!is.na(all_probs$fantasy_points_ppr))} with observed FP for calibration"
)

# ===========================================================================
# 6. CALIBRATION CHECK
# ===========================================================================

cli_h1("Step 6: Calibration check")

# Bin predicted probabilities into deciles; compare to empirical hit rate.
calibration_check <- function(probs_df, prob_col, hit_col, label, pos) {
  probs_df |>
    filter(!is.na(.data[[hit_col]]), !is.na(.data[[prob_col]])) |>
    mutate(prob_bin = cut(.data[[prob_col]],
                          breaks = seq(0, 1, by = 0.10),
                          include.lowest = TRUE, right = FALSE)) |>
    group_by(prob_bin) |>
    summarise(
      n          = n(),
      pred_prob  = mean(.data[[prob_col]]),
      empirical  = mean(.data[[hit_col]]),
      delta      = empirical - pred_prob,
      .groups    = "drop"
    ) |>
    mutate(threshold = label, position = pos, delta_pp = fmt_pp(delta))
}

rb_cal_start <- calibration_check(rb_probs, "p_start", "hit_start", "15+", "RB")
rb_cal_boom  <- calibration_check(rb_probs, "p_boom",  "hit_boom",  "20+", "RB")
wr_cal_start <- calibration_check(wr_probs, "p_start", "hit_start", "15+", "WR")
wr_cal_boom  <- calibration_check(wr_probs, "p_boom",  "hit_boom",  "20+", "WR")

cli_h2("RB P(FP >= 15) calibration")
print(rb_cal_start |> select(prob_bin, n, pred_prob, empirical, delta_pp), n = Inf)

cli_h2("RB P(FP >= 20) calibration")
print(rb_cal_boom |> select(prob_bin, n, pred_prob, empirical, delta_pp), n = Inf)

cli_h2("WR P(FP >= 15) calibration")
print(wr_cal_start |> select(prob_bin, n, pred_prob, empirical, delta_pp), n = Inf)

cli_h2("WR P(FP >= 20) calibration")
print(wr_cal_boom |> select(prob_bin, n, pred_prob, empirical, delta_pp), n = Inf)

# Summary calibration metrics
cal_summary <- bind_rows(rb_cal_start, rb_cal_boom, wr_cal_start, wr_cal_boom) |>
  group_by(position, threshold) |>
  summarise(
    mean_abs_delta = mean(abs(delta), na.rm = TRUE),
    max_abs_delta  = max(abs(delta),  na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mean_abs_delta_pp = fmt_pp(mean_abs_delta),
    max_abs_delta_pp  = fmt_pp(max_abs_delta)
  )

cli_h2("Calibration summary")
print(cal_summary |> select(position, threshold, mean_abs_delta_pp, max_abs_delta_pp), n = Inf)

# ===========================================================================
# 7. SUMMARY STATISTICS
# ===========================================================================

cli_h1("Step 7: Distribution of probabilities")

prob_summary <- all_probs |>
  filter(!is.na(fantasy_points_ppr)) |>
  group_by(position) |>
  summarise(
    n              = n(),
    pct_hit_start  = round(100 * mean(hit_start, na.rm = TRUE), 1),
    pct_hit_boom   = round(100 * mean(hit_boom,  na.rm = TRUE), 1),
    med_p_start    = round(median(p_start), 3),
    med_p_boom     = round(median(p_boom),  3),
    mean_mu_fp     = round(mean(mu_fp),     1),
    mean_sigma_fp  = round(mean(sigma_fp),  1),
    .groups = "drop"
  )

print(prob_summary, n = Inf)

cli_alert_info("RB: {prob_summary$pct_hit_start[prob_summary$position=='RB']}% of games hit 15+ | {prob_summary$pct_hit_boom[prob_summary$position=='RB']}% hit 20+")
cli_alert_info("WR: {prob_summary$pct_hit_start[prob_summary$position=='WR']}% of games hit 15+ | {prob_summary$pct_hit_boom[prob_summary$position=='WR']}% hit 20+")

# ===========================================================================
# 8. SAVE OUTPUTS
# ===========================================================================

cli_h1("Step 8: Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# Translation coefficients (needed for weekly deployment)
coef_table <- tibble(
  position    = c("RB", "RB", "RB", "WR", "WR", "WR"),
  term        = c("intercept", "total_epa", "opportunities",
                  "intercept", "total_epa", "opportunities"),
  estimate    = c(b_int_rb, b_epa_rb, b_opp_rb,
                  b_int_wr, b_epa_wr, b_opp_wr),
  sigma_resid = c(rep(sigma_resid_rb, 3), rep(sigma_resid_wr, 3))
)

readr::write_csv(coef_table,    "output/06_fp_translation_coefs.csv")
readr::write_csv(all_probs,     "output/06_fp_probabilities.csv")
readr::write_csv(
  bind_rows(rb_cal_start, rb_cal_boom, wr_cal_start, wr_cal_boom),
  "output/06_fp_calibration.csv"
)

cli_alert_success("output/06_fp_translation_coefs.csv")
cli_alert_success("output/06_fp_probabilities.csv  ({nrow(all_probs)} rows)")
cli_alert_success("output/06_fp_calibration.csv")

cli_h1("Step 6 complete -- translation layer ready")
cli_alert_info("Content outputs: P(FP >= {THRESH_START}) = start/sit | P(FP >= {THRESH_BOOM}) = boom indicator")
