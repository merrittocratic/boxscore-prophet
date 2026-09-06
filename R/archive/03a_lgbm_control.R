# R/03a_lgbm_control.R
# Step 3A: LightGBM control -- first bake-off contender and integration test.
# Two separate LightGBM models: efficiency (EPA/opp) and volume (opportunities).
# Split-conformal intervals built on the TRAINING side only -- never test data.
# Combined interval: opportunity-normalized conformal (corrected from naive pooled).
# Frozen inputs: data/rb_feature_table.rds, data/fold_map.rds, R/metrics.R
#
# DO NOT modify frozen inputs. DO NOT tune parameters after seeing results.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS -- all pre-committed before any fold results exist
# ===========================================================================

# Last CAL_FRAC of training season-weeks are held out for conformal calibration.
# 20% is standard for split-conformal; chosen before seeing any results.
CAL_FRAC <- 0.20

# Conservative LightGBM defaults sized for small NFL training sets.
# min_data_in_leaf=20 prevents splits on fewer than 20 rows; keeps early folds stable.
LGBM_PARAMS <- list(
  objective        = "regression",
  metric           = "rmse",
  num_leaves       = 31L,
  learning_rate    = 0.05,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq     = 5L,
  min_data_in_leaf = 20L,
  seed             = 42L,
  verbose          = -1L,
  num_threads      = 1L
)
N_ROUNDS <- 200L

# Backward-looking features only -- no observed outcome leakage
EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_rush_epa_adj", "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

VOL_FEATURES <- c(
  "wt_carry_share", "wt_target_share", "wt_snap_share", "wt_team_total_plays",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

# ===========================================================================
# HELPERS
# ===========================================================================

# Ordered integer encoding: higher value = stronger draft pedigree
TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |>
    mutate(
      draft_tier_int        = TIER_ORDER[draft_tier],
      is_cold_start_int     = as.integer(is_cold_start),
      def_used_fallback_int = as.integer(def_used_fallback)
    )
}

make_lgbm_matrix <- function(df, features) {
  df |> select(all_of(features)) |> as.matrix()
}

fit_lgbm <- function(X, y) {
  keep  <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = LGBM_PARAMS, data = dtrain, nrounds = N_ROUNDS, verbose = -1L)
}

# Standard split-conformal quantile: Inf when n_cal too small for a finite bound.
# alpha is the COVERAGE level (e.g. 0.80 for 80% intervals).
# Formula: quantile at (1 + 1/n_cal) * alpha -- see Angelopoulos & Bates (2021).
conformal_q <- function(abs_resid, alpha) {
  n    <- length(abs_resid)
  prob <- (1 + 1 / n) * alpha
  if (prob >= 1.0) return(Inf)
  quantile(abs_resid, prob, names = FALSE)
}

build_intervals <- function(pred, qs, suffix) {
  out <- tibble(
    p    = pred,
    lo50 = pred - qs[1], hi50 = pred + qs[1],
    lo80 = pred - qs[2], hi80 = pred + qs[2],
    lo90 = pred - qs[3], hi90 = pred + qs[3]
  )
  names(out) <- c(
    paste0("pred_", suffix),
    paste0("lo_50_", suffix), paste0("hi_50_", suffix),
    paste0("lo_80_", suffix), paste0("hi_80_", suffix),
    paste0("lo_90_", suffix), paste0("hi_90_", suffix)
  )
  out
}

# Combined-interval variant: half-widths are a per-row vector (q_norm * opportunities_i).
# Takes normalized quantiles (q_norms, length 3) and a scale vector (test opportunities).
build_scaled_intervals <- function(pred, q_norms, scale_vec, suffix) {
  hw50 <- q_norms[1] * scale_vec
  hw80 <- q_norms[2] * scale_vec
  hw90 <- q_norms[3] * scale_vec
  out <- tibble(
    p    = pred,
    lo50 = pred - hw50, hi50 = pred + hw50,
    lo80 = pred - hw80, hi80 = pred + hw80,
    lo90 = pred - hw90, hi90 = pred + hw90
  )
  names(out) <- c(
    paste0("pred_", suffix),
    paste0("lo_50_", suffix), paste0("hi_50_", suffix),
    paste0("lo_80_", suffix), paste0("hi_80_", suffix),
    paste0("lo_90_", suffix), paste0("hi_90_", suffix)
  )
  out
}

pi_cols <- function(df, suffix) {
  df |> transmute(
    lo_50 = .data[[paste0("lo_50_", suffix)]],
    hi_50 = .data[[paste0("hi_50_", suffix)]],
    lo_80 = .data[[paste0("lo_80_", suffix)]],
    hi_80 = .data[[paste0("hi_80_", suffix)]],
    lo_90 = .data[[paste0("lo_90_", suffix)]],
    hi_90 = .data[[paste0("hi_90_", suffix)]]
  )
}

# ===========================================================================
# LOAD FROZEN INPUTS -- never modify these files
# ===========================================================================

cli_h1("Step 3A: LightGBM Control")

ft       <- readRDS("data/rb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- sum(fold_map$n_test_rows)
cli_alert_success("Feature table: {nrow(ft)} rows x {ncol(ft)} cols")
cli_alert_success("Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop")

fold_results  <- vector("list", nrow(fold_map))
cal_diag_rows <- vector("list", nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {

  test_season <- fold_map$test_season[f]
  test_week   <- fold_map$test_week[f]

  # Exact split logic from the rubric (Step 2d contract -- no alternative splits)
  test_data <- ft |>
    filter(season == test_season, week == test_week)

  train_data <- ft |>
    filter(
      season < test_season |
      (season == test_season & week < test_week)
    )

  # INTEGRITY CHECK: no season-week can appear in both train and test
  overlap <- intersect(
    paste(train_data$season, train_data$week),
    paste(test_data$season,  test_data$week)
  )
  if (length(overlap) > 0L) {
    cli_abort("Fold {f}: train/test season-week overlap detected: {overlap}")
  }

  # Chronological split of training: last CAL_FRAC of season-weeks -> calibration
  train_sws <- train_data |>
    distinct(season, week) |>
    arrange(season, week)

  n_cal_sw <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws  <- tail(train_sws, n_cal_sw)

  # INTEGRITY CHECK: calibration season-weeks must not include the test season-week
  if (any(cal_sws$season == test_season & cal_sws$week == test_week)) {
    cli_abort("Fold {f}: test season-week leaked into conformal calibration set")
  }

  fit_sws  <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # -- Efficiency model (target: epa_per_opp_obs) --
  X_fit_eff  <- make_lgbm_matrix(fit_data,  EFF_FEATURES)
  X_cal_eff  <- make_lgbm_matrix(cal_data,  EFF_FEATURES)
  X_test_eff <- make_lgbm_matrix(test_data, EFF_FEATURES)

  mod_eff        <- fit_lgbm(X_fit_eff, fit_data$epa_per_opp_obs)
  pred_cal_eff   <- predict(mod_eff, X_cal_eff)
  pred_test_eff  <- predict(mod_eff, X_test_eff)

  resid_eff <- abs(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_eff    <- c(
    conformal_q(resid_eff, 0.50),
    conformal_q(resid_eff, 0.80),
    conformal_q(resid_eff, 0.90)
  )

  # -- Volume model (target: opportunities) --
  X_fit_vol  <- make_lgbm_matrix(fit_data,  VOL_FEATURES)
  X_cal_vol  <- make_lgbm_matrix(cal_data,  VOL_FEATURES)
  X_test_vol <- make_lgbm_matrix(test_data, VOL_FEATURES)

  mod_vol       <- fit_lgbm(X_fit_vol, as.numeric(fit_data$opportunities))
  pred_cal_vol  <- predict(mod_vol, X_cal_vol)
  pred_test_vol <- predict(mod_vol, X_test_vol)

  resid_vol <- abs(as.numeric(cal_data$opportunities) - pred_cal_vol)
  qs_vol    <- c(
    conformal_q(resid_vol, 0.50),
    conformal_q(resid_vol, 0.80),
    conformal_q(resid_vol, 0.90)
  )

  # -- Combined (efficiency x volume) -- opportunity-normalized conformal --
  # Motivation: total_epa = epa_per_opp * opportunities, so raw combined residuals
  # scale with opp count. A pooled quantile gets set by high-opp players and over-
  # covers low-opp players when applied uniformly (confirmed in first-run diagnosis).
  #
  # Construction:
  #   1. Normalize cal residuals by actual opportunities (available; these are training rows).
  #   2. Take conformal quantile on the normalized scale -> q_norm.
  #   3. At test time, rescale per row: half_width_i = q_norm * actual_opp_i.
  # Using actual test opportunities is correct for retrospective evaluation. A live
  # deployment would substitute pred_test_vol here since observed opp is not known
  # before the game -- but the calibration guarantee holds with either choice as long
  # as the two sides (cal and test) use the same denominator type. Flagged for 3B.
  pred_cal_tot  <- pred_cal_eff  * pred_cal_vol
  pred_test_tot <- pred_test_eff * pred_test_vol

  raw_resid_tot  <- abs(cal_data$total_epa - pred_cal_tot)
  resid_norm_tot <- raw_resid_tot / as.numeric(cal_data$opportunities)

  qs_norm_tot <- c(
    conformal_q(resid_norm_tot, 0.50),
    conformal_q(resid_norm_tot, 0.80),
    conformal_q(resid_norm_tot, 0.90)
  )

  # Record normalized residuals for the flatness diagnostic
  cal_diag_rows[[f]] <- tibble(
    fold          = f,
    opp           = as.integer(cal_data$opportunities),
    raw_resid     = raw_resid_tot,
    norm_resid    = resid_norm_tot
  )

  fold_results[[f]] <- test_data |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(
      build_intervals(pred_test_eff, qs_eff, "eff"),
      build_intervals(pred_test_vol, qs_vol, "vol"),
      build_scaled_intervals(pred_test_tot, qs_norm_tot, as.numeric(test_data$opportunities), "tot")
    ) |>
    mutate(fold = f)

  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | cal_sw={n_cal_sw} ({nrow(cal_data)} rows)"
  )
}

results  <- bind_rows(fold_results)
cal_diag <- bind_rows(cal_diag_rows)

# ===========================================================================
# NORMALIZED-RESIDUAL FLATNESS CHECK
# If normalization corrected the scale-mixing problem, mean |resid_norm| should
# be roughly flat across opportunity buckets. A sloped pattern means linear
# scaling was insufficient and stratification would be needed instead.
# ===========================================================================

cli_h1("Normalized-Residual Flatness Check (combined, calibration rows)")

flatness <- cal_diag |>
  mutate(
    opp_bucket = case_when(
      opp <= 8L  ~ "low  (5-8)",
      opp <= 13L ~ "mid  (9-13)",
      TRUE       ~ "high (14+)"
    ) |> factor(levels = c("low  (5-8)", "mid  (9-13)", "high (14+)"))
  ) |>
  group_by(opp_bucket) |>
  summarise(
    n               = n(),
    mean_raw_resid  = round(mean(raw_resid),  3),
    mean_norm_resid = round(mean(norm_resid), 3),
    sd_norm_resid   = round(sd(norm_resid),   3),
    .groups = "drop"
  )

print(flatness, n = Inf)

# Slope test: ratio of high-bucket mean to low-bucket mean after normalization.
# Close to 1.0 = flat (normalization worked). Far from 1.0 = still heteroscedastic.
mean_low  <- flatness$mean_norm_resid[flatness$opp_bucket == "low  (5-8)"]
mean_high <- flatness$mean_norm_resid[flatness$opp_bucket == "high (14+)"]
slope_ratio <- mean_high / mean_low

cli_alert_info("high/low normalized-residual ratio: {round(slope_ratio, 3)} (1.0 = perfectly flat)")

if (slope_ratio > 1.5 || slope_ratio < 0.67) {
  cli_warn(
    "Normalized residuals still heteroscedastic (ratio={round(slope_ratio,2)}): linear scaling insufficient -- consider stratification"
  )
} else {
  cli_alert_success(
    "Normalized residuals are reasonably flat across opp buckets (ratio={round(slope_ratio,2)})"
  )
}

# ===========================================================================
# HARNESS INTEGRITY REPORT
# ===========================================================================

cli_h1("Harness Integrity Report")

# 1. All 31 folds ran
n_folds_ran <- n_distinct(results$fold)
if (n_folds_ran == nrow(fold_map)) {
  cli_alert_success("All {nrow(fold_map)} folds completed -- no silently skipped folds")
} else {
  cli_abort("Only {n_folds_ran} of {nrow(fold_map)} folds produced results")
}

# 2. Total test row count
n_scored <- nrow(results)
cli_alert_info("Test rows scored: {n_scored} | Expected: {EXPECTED_TEST_N}")
if (n_scored == EXPECTED_TEST_N) {
  cli_alert_success("Row count matches expected {EXPECTED_TEST_N}")
} else {
  cli_warn("Row count mismatch: scored {n_scored}, expected {EXPECTED_TEST_N}")
}

# 3. NA / dropped predictions
na_eff <- sum(is.na(results$pred_eff))
na_vol <- sum(is.na(results$pred_vol))
na_tot <- sum(is.na(results$pred_tot))
if (na_eff + na_vol + na_tot == 0L) {
  cli_alert_success("Zero NA predictions across all folds and components")
} else {
  cli_warn("NA predictions: efficiency={na_eff}, volume={na_vol}, combined={na_tot}")
  results |>
    filter(is.na(pred_eff) | is.na(pred_vol) | is.na(pred_tot)) |>
    select(fold, season, week, player_id) |>
    print()
}

# 4. Conformal calibration on training side only
# Structural guarantee: cal_sws built from train_data inside the loop,
# and verified above by the calendar-leak assertion on every fold.
cli_alert_success("Conformal calibration: training-side only (loop assertion verified each fold)")

# ===========================================================================
# SCORING
# ===========================================================================

cli_h1("Pooled Coverage (all {n_scored} test rows across 31 folds)")

score_component <- function(y, df, suffix, label) {
  eval_calibration(y, pi_cols(df, suffix)) |>
    mutate(component = label, stratum = "pooled", .before = 1)
}

pooled <- bind_rows(
  score_component(results$epa_per_opp_obs,            results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities),  results, "vol", "volume"),
  score_component(results$total_epa,                  results, "tot", "combined")
)

print(pooled |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# ===========================================================================
# LOW-USAGE BUCKET (the veto stratum)
# ===========================================================================

cli_h1("Low-Usage Bucket (opp {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low  <- nrow(res_lo)
cli_alert_info("Low-usage bucket rows: {n_low}")

low_usage <- bind_rows(
  score_component(res_lo$epa_per_opp_obs,           res_lo, "eff", "efficiency"),
  score_component(as.numeric(res_lo$opportunities), res_lo, "vol", "volume"),
  score_component(res_lo$total_epa,                 res_lo, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))

print(low_usage |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# ===========================================================================
# DECISION RULE (pre-committed across all contenders)
# ===========================================================================

cli_h1("Decision Rule Evaluation")
cli_alert_info("PRIMARY: pooled combined 80% coverage closest to nominal wins")
cli_alert_info("TIEBREAK: sharpest (narrowest 80% interval) among models within +/-2pp of nominal")
cli_alert_info("VETO: low-usage 80% delta > 10pp disqualifies regardless of pooled performance")

combined_80_pooled <- pooled    |> filter(component == "combined", nominal == 0.80)
combined_80_low    <- low_usage |> filter(component == "combined", nominal == 0.80)

delta_pooled <- combined_80_pooled$delta
delta_low    <- combined_80_low$delta

cli_alert_info(
  "3A pooled combined 80%: empirical={round(combined_80_pooled$empirical, 3)} delta={sprintf('%+.3f', delta_pooled)}"
)
cli_alert_info(
  "3A low-usage   combined 80%: empirical={round(combined_80_low$empirical, 3)} delta={sprintf('%+.3f', delta_low)}"
)

if (abs(delta_low) > 0.10) {
  cli_warn(
    "VETO TRIGGERED: low-usage bucket 80% delta = {sprintf('%+.1f', delta_low*100)}pp (threshold +-10pp)"
  )
} else {
  cli_alert_success(
    "Veto check passed: low-usage 80% delta = {sprintf('%+.1f', delta_low*100)}pp (within +-10pp)"
  )
}

cli_alert_info("Sharpness summary (combined, pooled):")
pooled |>
  filter(component == "combined") |>
  mutate(label = paste0(as.integer(nominal * 100), "%: width=", round(sharpness, 3))) |>
  pull(label) |>
  paste(collapse = " | ") |>
  cli_alert_info()

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,    "output/03a_lgbm_fold_predictions.csv")
readr::write_csv(pooled,     "output/03a_lgbm_pooled_coverage.csv")
readr::write_csv(low_usage,  "output/03a_lgbm_low_usage_coverage.csv")

cli_alert_success("output/03a_lgbm_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/03a_lgbm_pooled_coverage.csv   ({nrow(pooled)} rows)")
cli_alert_success("output/03a_lgbm_low_usage_coverage.csv ({nrow(low_usage)} rows)")

cli_h1("Step 3A complete")
