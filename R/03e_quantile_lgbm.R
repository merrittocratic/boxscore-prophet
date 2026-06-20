# R/03e_quantile_lgbm.R
# Step 3E: Direct-quantile LightGBM -- combined interval via learned quantile regression.
#
# WHY THIS EXISTS:
#   All prior contenders use RMSE point predictors + conformal calibration wrappers.
#   With ~12,000 training rows (vs ~2,000 in the original bake-off), there is enough
#   data for LightGBM to learn the combined EPA interval directly via quantile
#   regression. The hypothesis: a quantile model on total_epa learns opp-conditional
#   interval width implicitly from the data, without a power-law assumption or a
#   separate calibration step.
#
# ARCHITECTURE:
#   Efficiency / volume components: LightGBM RMSE (fixed hyperparams) + Mechanism A
#   conformal, same construction as 3A-v2. Included for rubric compliance.
#   The innovation is isolated to the combined interval.
#
#   Combined interval: 3x LightGBM quantile models (q10, q50, q90) on total_epa.
#   Features: union of EFF_FEATURES and VOL_FEATURES.
#   80% PI: [q50 - hw, q50 + hw] where hw = (q90 - q10) / 2.
#   50%/90% PIs: Gaussian-ratio scaling from hw_80 (no additional model fits).
#
# HYPERPARAMS: fixed across all models (no inner tuning). This is an architecture
# test. The tuning dimension is covered by 3A-v2.
#
# LEAKAGE DISCIPLINE: identical to all contenders. Cal split used only for
# conformal calibration of component intervals. Quantile models are fit on
# fit_data, scored on test_data.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS
# ===========================================================================

CAL_FRAC   <- 0.20
LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

FIXED_PARAMS_RMSE <- list(
  objective        = "regression",
  metric           = "rmse",
  num_leaves       = 31L,
  min_data_in_leaf = 20L,
  learning_rate    = 0.05,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq     = 5L,
  seed             = 42L,
  verbose          = -1L,
  num_threads      = 1L
)

make_quantile_params <- function(alpha) {
  list(
    objective        = "quantile",
    metric           = "quantile",
    alpha            = alpha,
    num_leaves       = 31L,
    min_data_in_leaf = 20L,
    learning_rate    = 0.05,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq     = 5L,
    seed             = 42L,
    verbose          = -1L,
    num_threads      = 1L
  )
}

MAX_ROUNDS <- 500L
EARLY_STOP <- 30L

# Gaussian half-width scaling: qnorm(p) values at p = 0.75, 0.90, 0.95.
# hw_80 = (q90 - q10) / 2 is the observed 80% half-width.
# hw_50 = hw_80 * (qnorm(0.75) / qnorm(0.90))
# hw_90 = hw_80 * (qnorm(0.95) / qnorm(0.90))
SCALE_50_FROM_80 <- qnorm(0.75) / qnorm(0.90)
SCALE_90_FROM_80 <- qnorm(0.95) / qnorm(0.90)

ALPHA_LO       <- 0.20
ALPHA_HI       <- 0.90
ALPHA_FALLBACK <- 0.50

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

TOT_FEATURES <- union(EFF_FEATURES, VOL_FEATURES)

# ===========================================================================
# HELPERS
# ===========================================================================

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |>
    mutate(
      draft_tier_int        = TIER_ORDER[draft_tier],
      is_cold_start_int     = as.integer(is_cold_start),
      def_used_fallback_int = as.integer(def_used_fallback)
    )
}

make_matrix <- function(df, features) {
  df |> select(all_of(features)) |> as.matrix()
}

conformal_q <- function(abs_resid, alpha) {
  n    <- length(abs_resid)
  prob <- (1 + 1 / n) * alpha
  if (prob >= 1.0) return(Inf)
  quantile(abs_resid, prob, names = FALSE)
}

fit_power_alpha <- function(opp, raw_resid) {
  df  <- data.frame(log_opp = log(opp), log_resid = log(raw_resid + 1e-8))
  fit <- tryCatch(lm(log_resid ~ log_opp, data = df), error = function(e) NULL)
  if (is.null(fit)) return(ALPHA_FALLBACK)
  alpha <- unname(coef(fit)["log_opp"])
  if (!is.finite(alpha)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, alpha))
}

fit_lgbm_rmse <- function(X, y) {
  keep   <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = FIXED_PARAMS_RMSE, data = dtrain, nrounds = MAX_ROUNDS, verbose = -1L)
}

fit_lgbm_quantile <- function(X_fit, y_fit, q, X_val, y_val) {
  keep   <- !is.na(y_fit)
  dtrain <- lgb.Dataset(X_fit[keep, , drop = FALSE], label = y_fit[keep])
  dval   <- lgb.Dataset(X_val, label = y_val, reference = dtrain)
  lgb.train(
    params                = make_quantile_params(q),
    data                  = dtrain,
    nrounds               = MAX_ROUNDS,
    valids                = list(val = dval),
    early_stopping_rounds = EARLY_STOP,
    verbose               = -1L
  )
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

score_component <- function(y, df, suffix, label, stratum = "pooled") {
  eval_calibration(y, pi_cols(df, suffix)) |>
    mutate(component = label, stratum = stratum, .before = 1)
}

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 3E: Direct-Quantile LightGBM")
cli_alert_info("Combined: q10/q50/q90 quantile regression on total_epa (no conformal wrapper)")
cli_alert_info("Components: Mechanism A conformal, fixed hyperparams (rubric compliance)")
cli_alert_info("TOT_FEATURES: {length(TOT_FEATURES)} ({paste(TOT_FEATURES, collapse=', ')})")

ft       <- readRDS("data/rb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- sum(fold_map$n_test_rows)
cli_alert_success("Feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds -- 5 LightGBM fits per fold)")

fold_results <- vector("list", nrow(fold_map))
alpha_log    <- numeric(nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {

  t0 <- proc.time()[["elapsed"]]

  test_season <- fold_map$test_season[f]
  test_week   <- fold_map$test_week[f]

  test_data  <- ft |> filter(season == test_season, week == test_week)
  train_data <- ft |> filter(
    season < test_season |
    (season == test_season & week < test_week)
  )

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)
  fit_sws   <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data  <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data  <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  if (any(cal_sws$season == test_season & cal_sws$week == test_week)) {
    cli_abort("Fold {f}: test season-week leaked into cal set")
  }

  # --- Component point predictors (RMSE, fixed hyperparams) ---
  X_fit_eff  <- make_matrix(fit_data, EFF_FEATURES)
  X_cal_eff  <- make_matrix(cal_data, EFF_FEATURES)
  X_test_eff <- make_matrix(test_data, EFF_FEATURES)
  X_fit_vol  <- make_matrix(fit_data, VOL_FEATURES)
  X_cal_vol  <- make_matrix(cal_data, VOL_FEATURES)
  X_test_vol <- make_matrix(test_data, VOL_FEATURES)

  mod_eff <- fit_lgbm_rmse(X_fit_eff, fit_data$epa_per_opp_obs)
  mod_vol <- fit_lgbm_rmse(X_fit_vol, as.numeric(fit_data$opportunities))

  pred_cal_eff  <- predict(mod_eff, X_cal_eff)
  pred_test_eff <- predict(mod_eff, X_test_eff)
  pred_cal_vol  <- predict(mod_vol, X_cal_vol)
  pred_test_vol <- predict(mod_vol, X_test_vol)

  resid_eff <- abs(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_eff    <- c(conformal_q(resid_eff, 0.50),
                 conformal_q(resid_eff, 0.80),
                 conformal_q(resid_eff, 0.90))

  resid_vol <- abs(as.numeric(cal_data$opportunities) - pred_cal_vol)
  qs_vol    <- c(conformal_q(resid_vol, 0.50),
                 conformal_q(resid_vol, 0.80),
                 conformal_q(resid_vol, 0.90))

  # --- Combined: direct quantile regression on total_epa ---
  X_fit_tot  <- make_matrix(fit_data, TOT_FEATURES)
  X_cal_tot  <- make_matrix(cal_data, TOT_FEATURES)
  X_test_tot <- make_matrix(test_data, TOT_FEATURES)

  mod_q10 <- fit_lgbm_quantile(X_fit_tot, fit_data$total_epa, 0.10, X_cal_tot, cal_data$total_epa)
  mod_q50 <- fit_lgbm_quantile(X_fit_tot, fit_data$total_epa, 0.50, X_cal_tot, cal_data$total_epa)
  mod_q90 <- fit_lgbm_quantile(X_fit_tot, fit_data$total_epa, 0.90, X_cal_tot, cal_data$total_epa)

  p10 <- predict(mod_q10, X_test_tot)
  p50 <- predict(mod_q50, X_test_tot)
  p90 <- predict(mod_q90, X_test_tot)

  hw_80 <- pmax(0, (p90 - p10) / 2)
  hw_50 <- hw_80 * SCALE_50_FROM_80
  hw_90 <- hw_80 * SCALE_90_FROM_80

  # Observed alpha for comparison with Mechanism A models (informational only)
  raw_resid_cal <- abs(cal_data$total_epa - predict(mod_q50, X_cal_tot))
  alpha_log[f]  <- fit_power_alpha(as.numeric(cal_data$opportunities), raw_resid_cal)

  tot_intervals <- tibble(
    pred_tot  = p50,
    lo_50_tot = p50 - hw_50, hi_50_tot = p50 + hw_50,
    lo_80_tot = p50 - hw_80, hi_80_tot = p50 + hw_80,
    lo_90_tot = p50 - hw_90, hi_90_tot = p50 + hw_90
  )

  fold_results[[f]] <- test_data |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(
      build_intervals(pred_test_eff, qs_eff, "eff"),
      build_intervals(pred_test_vol, qs_vol, "vol"),
      tot_intervals
    ) |>
    mutate(fold = f, alpha_obs = alpha_log[f])

  t1 <- proc.time()[["elapsed"]]
  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | alpha_obs={round(alpha_log[f],3)} | {round(t1-t0)}s"
  )
}

results <- bind_rows(fold_results)

# ===========================================================================
# INTEGRITY REPORT
# ===========================================================================

cli_h1("Harness Integrity Report")

n_scored <- nrow(results)
if (n_scored == EXPECTED_TEST_N) {
  cli_alert_success("Row count: {n_scored} / {EXPECTED_TEST_N}")
} else {
  cli_warn("Row count mismatch: scored {n_scored}, expected {EXPECTED_TEST_N}")
}

na_check <- sum(is.na(results$pred_eff)) + sum(is.na(results$pred_vol)) + sum(is.na(results$pred_tot))
if (na_check == 0L) {
  cli_alert_success("Zero NA predictions")
} else {
  cli_warn("NA predictions detected: {na_check} total")
}

cli_alert_info(
  "alpha_obs range [{round(min(alpha_log),3)}, {round(max(alpha_log),3)}] | median {round(median(alpha_log),3)}"
)

# ===========================================================================
# COVERAGE SCORING
# ===========================================================================

cli_h1("3E Pooled Coverage (all {n_scored} test rows)")

pooled_e <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_e |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("3E Low-Usage Bucket (opp {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo_e <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
cli_alert_info("Low-usage rows: {nrow(res_lo_e)}")

low_e <- bind_rows(
  score_component(res_lo_e$epa_per_opp_obs,           res_lo_e, "eff", "efficiency"),
  score_component(as.numeric(res_lo_e$opportunities), res_lo_e, "vol", "volume"),
  score_component(res_lo_e$total_epa,                 res_lo_e, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_e |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h2("Stratified combined coverage at 80%")
res_strat_e <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
      opportunities <= 13L        ~ "mid  (9-13)",
      TRUE                        ~ "high (14+)"
    ) |> factor(levels = c(paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
                            "mid  (9-13)", "high (14+)"))
  )

strat_e <- eval_calibration_stratified(
  res_strat_e$total_epa,
  pi_cols(res_strat_e, "tot"),
  strata = res_strat_e$opp_bucket
) |> mutate(component = "combined")

print(
  strat_e |> filter(nominal == 0.80) |>
    select(stratum, n, empirical, delta, sharpness) |>
    mutate(delta_pp = fmt_pp(delta)),
  n = Inf
)

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,  "output/03e_quantile_lgbm_fold_predictions.csv")
readr::write_csv(pooled_e, "output/03e_quantile_lgbm_pooled_coverage.csv")
readr::write_csv(low_e,    "output/03e_quantile_lgbm_low_usage_coverage.csv")

cli_alert_success("output/03e_quantile_lgbm_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/03e_quantile_lgbm_pooled_coverage.csv")
cli_alert_success("output/03e_quantile_lgbm_low_usage_coverage.csv")

cli_h1("Step 3E complete")
