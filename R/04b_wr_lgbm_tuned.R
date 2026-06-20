# R/04b_wr_lgbm_tuned.R
# Step 4b: Nested-walk-forward-tuned LightGBM for WR EPA model.
#
# CLONES 03a_v2_lgbm_tuned.R with WR-specific adjustments:
#   - EFF_FEATURES: drop def_rush_epa_adj; add wt_air_yards_per_target
#   - VOL_FEATURES: wt_target_share + wt_air_yards_share (no carry share)
#   - Defensive inputs: short_pass + deep_pass only
#   - Low-opp bucket: 3-5 targets (vs 5-8 for RBs)
#   - EXPECTED_TEST_N computed from WR feature table (WR rows per fold > RB rows)
#
# LEAKAGE DISCIPLINE: identical to 03a_v2.
#   Inner split uses last 20% of season-weeks as RMSE holdout.
#   Tuning selects params on inner RMSE only; conformal runs fresh on same cal rows.
#   Outer test rows never seen until final scoring.
#
# CONSTRUCTION: FROZEN Mechanism A power-law (same as 03a_v2/3A/3B).
# RUBRIC: FROZEN (same veto threshold, decision rule, fold_map).

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
LOW_OPP_LO <- 3L    # WR target floor (vs 5 for RB carries+targets)
LOW_OPP_HI <- 5L

# Tuning grid (32 combos per component per fold) -- identical to 03a_v2
TUNE_GRID <- expand.grid(
  num_leaves       = c(7L, 15L, 31L, 63L),
  min_data_in_leaf = c(10L, 20L, 50L, 100L),
  lr               = c(0.02, 0.05)
)

# Inner early stopping settings
INNER_MAX_ROUNDS  <- 500L
INNER_EARLY_STOP  <- 20L

REFIT_ROUNDS_MIN  <- 10L

# Fixed params shared across all combos
LGBM_FIXED <- list(
  objective          = "regression",
  metric             = "rmse",
  feature_fraction   = 0.8,
  bagging_fraction   = 0.8,
  bagging_freq       = 5L,
  seed               = 42L,
  verbose            = -1L,
  num_threads        = 1L,
  feature_pre_filter = FALSE
)

# WR efficiency features: drop def_rush_epa_adj; add wt_air_yards_per_target
EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_air_yards_per_target",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

# WR volume features: target share + air yards share replace carry share
VOL_FEATURES <- c(
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_team_total_plays",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

# Mechanism A power-law constants (frozen from 3A/3A-v2)
ALPHA_LO       <- 0.20
ALPHA_HI       <- 0.90
ALPHA_FALLBACK <- 0.50

# ===========================================================================
# HELPERS  (identical to 03a_v2)
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

build_row_intervals <- function(pred, hw50, hw80, hw90, suffix) {
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

score_component <- function(y, df, suffix, label, stratum = "pooled") {
  eval_calibration(y, pi_cols(df, suffix)) |>
    mutate(component = label, stratum = stratum, .before = 1)
}

# Inner grid search for one model component.
tune_lgbm_component <- function(X_fit, y_fit, X_val, y_val, label = "") {
  keep <- !is.na(y_fit)
  dtrain <- lgb.Dataset(X_fit[keep, , drop = FALSE], label = y_fit[keep])

  best_rmse   <- Inf
  best_row    <- NULL
  best_rounds <- 100L

  for (i in seq_len(nrow(TUNE_GRID))) {
    params <- c(
      LGBM_FIXED,
      list(
        num_leaves       = TUNE_GRID$num_leaves[i],
        learning_rate    = TUNE_GRID$lr[i],
        min_data_in_leaf = TUNE_GRID$min_data_in_leaf[i]
      )
    )

    dval <- lgb.Dataset(X_val, label = y_val, reference = dtrain)

    mod <- lgb.train(
      params                = params,
      data                  = dtrain,
      nrounds               = INNER_MAX_ROUNDS,
      valids                = list(val = dval),
      early_stopping_rounds = INNER_EARLY_STOP,
      verbose               = -1L
    )

    preds    <- predict(mod, X_val)
    val_rmse <- sqrt(mean((y_val - preds)^2, na.rm = TRUE))
    n_rounds <- mod$best_iter

    if (val_rmse < best_rmse) {
      best_rmse   <- val_rmse
      best_rounds <- n_rounds
      best_row    <- TUNE_GRID[i, ]
    }
  }

  list(
    num_leaves       = best_row$num_leaves,
    lr               = best_row$lr,
    min_data_in_leaf = best_row$min_data_in_leaf,
    rounds           = best_rounds,
    inner_rmse       = best_rmse
  )
}

fit_lgbm_tuned <- function(X, y, params_list, n_rounds) {
  keep   <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(
    params  = c(LGBM_FIXED, params_list),
    data    = dtrain,
    nrounds = n_rounds,
    verbose = -1L
  )
}

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)
fmt_w  <- function(x) round(x, 3)

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 4b: WR Nested-Walk-Forward-Tuned LightGBM")
cli_alert_info("Grid: {nrow(TUNE_GRID)} combos per component per fold (eff and vol tuned separately)")
cli_alert_info("Inner split: last {CAL_FRAC*100}% season-weeks as RMSE holdout")
cli_alert_info("Construction: FROZEN Mechanism A power-law (identical to 3A/3A-v2)")
cli_alert_info("Rubric: FROZEN (unchanged veto, decision rule, fold_map)")

ft       <- readRDS("data/wr_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

# EXPECTED_TEST_N computed from WR feature table -- WR row count per fold differs from RB
EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()

cli_alert_success("WR feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

# Confirm all model features are present before the fold loop
missing_eff <- setdiff(EFF_FEATURES, names(ft))
missing_vol <- setdiff(VOL_FEATURES, names(ft))
if (length(missing_eff) > 0) cli::cli_abort("Missing EFF_FEATURES: {paste(missing_eff, collapse=', ')}")
if (length(missing_vol) > 0) cli::cli_abort("Missing VOL_FEATURES: {paste(missing_vol, collapse=', ')}")
cli_alert_success("All model features present in WR feature table")

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds x {nrow(TUNE_GRID)} combos x 2 components)")

fold_results  <- vector("list", nrow(fold_map))
tune_log      <- vector("list", nrow(fold_map))
alpha_log     <- numeric(nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {

  t0 <- proc.time()[["elapsed"]]

  test_season <- fold_map$test_season[f]
  test_week   <- fold_map$test_week[f]

  test_data  <- ft |> filter(season == test_season, week == test_week)
  train_data <- ft |> filter(
    season < test_season |
    (season == test_season & week < test_week)
  )

  overlap <- intersect(
    paste(train_data$season, train_data$week),
    paste(test_data$season,  test_data$week)
  )
  if (length(overlap) > 0L) cli_abort("Fold {f}: train/test overlap")

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)

  if (any(cal_sws$season == test_season & cal_sws$week == test_week)) {
    cli_abort("Fold {f}: test season-week leaked into cal set")
  }

  fit_sws  <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  X_fit_eff <- make_matrix(fit_data, EFF_FEATURES)
  X_cal_eff <- make_matrix(cal_data, EFF_FEATURES)
  X_fit_vol <- make_matrix(fit_data, VOL_FEATURES)
  X_cal_vol <- make_matrix(cal_data, VOL_FEATURES)

  # --- INNER TUNING: efficiency ---
  best_eff <- tune_lgbm_component(
    X_fit_eff, fit_data$epa_per_opp_obs,
    X_cal_eff, cal_data$epa_per_opp_obs,
    label = paste0("fold", f, "_eff")
  )

  # --- INNER TUNING: volume (opportunities = targets for WRs) ---
  best_vol <- tune_lgbm_component(
    X_fit_vol, as.numeric(fit_data$opportunities),
    X_cal_vol, as.numeric(cal_data$opportunities),
    label = paste0("fold", f, "_vol")
  )

  tune_log[[f]] <- tibble(
    fold              = f,
    eff_num_leaves    = best_eff$num_leaves,
    eff_lr            = best_eff$lr,
    eff_min_node      = best_eff$min_data_in_leaf,
    eff_rounds        = best_eff$rounds,
    eff_inner_rmse    = round(best_eff$inner_rmse, 4),
    vol_num_leaves    = best_vol$num_leaves,
    vol_lr            = best_vol$lr,
    vol_min_node      = best_vol$min_data_in_leaf,
    vol_rounds        = best_vol$rounds,
    vol_inner_rmse    = round(best_vol$inner_rmse, 4)
  )

  # --- FINAL REFIT ---
  eff_refit_rounds <- max(REFIT_ROUNDS_MIN, best_eff$rounds)
  vol_refit_rounds <- max(REFIT_ROUNDS_MIN, best_vol$rounds)

  X_test_eff <- make_matrix(test_data, EFF_FEATURES)
  X_test_vol <- make_matrix(test_data, VOL_FEATURES)

  mod_eff <- fit_lgbm_tuned(
    X_fit_eff, fit_data$epa_per_opp_obs,
    list(num_leaves = best_eff$num_leaves, learning_rate = best_eff$lr,
         min_data_in_leaf = best_eff$min_data_in_leaf),
    eff_refit_rounds
  )
  mod_vol <- fit_lgbm_tuned(
    X_fit_vol, as.numeric(fit_data$opportunities),
    list(num_leaves = best_vol$num_leaves, learning_rate = best_vol$lr,
         min_data_in_leaf = best_vol$min_data_in_leaf),
    vol_refit_rounds
  )

  pred_cal_eff  <- predict(mod_eff, X_cal_eff)
  pred_test_eff <- predict(mod_eff, X_test_eff)
  pred_cal_vol  <- predict(mod_vol, X_cal_vol)
  pred_test_vol <- predict(mod_vol, X_test_vol)

  # --- Component conformal intervals ---
  resid_eff <- abs(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_eff    <- c(conformal_q(resid_eff, 0.50),
                 conformal_q(resid_eff, 0.80),
                 conformal_q(resid_eff, 0.90))

  resid_vol <- abs(as.numeric(cal_data$opportunities) - pred_cal_vol)
  qs_vol    <- c(conformal_q(resid_vol, 0.50),
                 conformal_q(resid_vol, 0.80),
                 conformal_q(resid_vol, 0.90))

  # --- Combined: frozen Mechanism A power-law ---
  pred_cal_tot  <- pred_cal_eff  * pred_cal_vol
  pred_test_tot <- pred_test_eff * pred_test_vol
  raw_resid_cal <- abs(cal_data$total_epa - pred_cal_tot)
  cal_opp       <- as.numeric(cal_data$opportunities)
  test_opp      <- as.numeric(test_data$opportunities)

  alpha        <- fit_power_alpha(cal_opp, raw_resid_cal)
  alpha_log[f] <- alpha

  resid_norm <- raw_resid_cal / cal_opp^alpha
  q_norm     <- c(conformal_q(resid_norm, 0.50),
                  conformal_q(resid_norm, 0.80),
                  conformal_q(resid_norm, 0.90))

  hw50 <- q_norm[1] * test_opp^alpha
  hw80 <- q_norm[2] * test_opp^alpha
  hw90 <- q_norm[3] * test_opp^alpha

  fold_results[[f]] <- test_data |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(
      build_intervals(pred_test_eff, qs_eff, "eff"),
      build_intervals(pred_test_vol, qs_vol, "vol"),
      build_row_intervals(pred_test_tot, hw50, hw80, hw90, "tot")
    ) |>
    mutate(fold = f, alpha_fold = alpha)

  t1 <- proc.time()[["elapsed"]]
  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | eff: leaves={best_eff$num_leaves} minnode={best_eff$min_data_in_leaf} lr={best_eff$lr} rds={eff_refit_rounds} | vol: leaves={best_vol$num_leaves} minnode={best_vol$min_data_in_leaf} lr={best_vol$lr} rds={vol_refit_rounds} | {round(t1-t0)}s"
  )
}

results  <- bind_rows(fold_results)
tune_all <- bind_rows(tune_log)

# ===========================================================================
# HARNESS INTEGRITY REPORT
# ===========================================================================

cli_h1("Harness Integrity Report")

n_folds_ran <- n_distinct(results$fold)
if (n_folds_ran == nrow(fold_map)) {
  cli_alert_success("All {nrow(fold_map)} folds completed")
} else {
  cli_abort("Only {n_folds_ran} of {nrow(fold_map)} folds produced results")
}

n_scored <- nrow(results)
if (n_scored == EXPECTED_TEST_N) {
  cli_alert_success("Row count: {n_scored} / {EXPECTED_TEST_N}")
} else {
  cli_warn("Row count mismatch: scored {n_scored}, expected {EXPECTED_TEST_N}")
}

na_eff <- sum(is.na(results$pred_eff))
na_vol <- sum(is.na(results$pred_vol))
na_tot <- sum(is.na(results$pred_tot))
if (na_eff + na_vol + na_tot == 0L) {
  cli_alert_success("Zero NA predictions")
} else {
  cli_warn("NA predictions: eff={na_eff}, vol={na_vol}, tot={na_tot}")
}

cli_alert_success("Inner tuning: all {nrow(TUNE_GRID)} combos per component per fold scored on inner holdout only")
cli_alert_success("Outer test: each fold scored exactly once after param selection")
cli_alert_info(
  "alpha_fold range [{round(min(alpha_log),3)}, {round(max(alpha_log),3)}] | median {round(median(alpha_log),3)}"
)

# ===========================================================================
# SELECTED HYPERPARAMETER AUDIT
# ===========================================================================

cli_h1("Selected Hyperparameters (tuning audit)")
print(tune_all, n = Inf)

eff_refit_vec <- pmax(REFIT_ROUNDS_MIN, tune_all$eff_rounds)
vol_refit_vec <- pmax(REFIT_ROUNDS_MIN, tune_all$vol_rounds)

cli_h2("Summary across {nrow(fold_map)} folds")
cli_alert_info("--- EFFICIENCY ---")
cli_alert_info(
  "  num_leaves: mode={as.integer(names(sort(table(tune_all$eff_num_leaves), decreasing=T)[1]))} | counts: {paste(names(table(tune_all$eff_num_leaves)), table(tune_all$eff_num_leaves), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  min_data_in_leaf: mode={as.integer(names(sort(table(tune_all$eff_min_node), decreasing=T)[1]))} | counts: {paste(names(table(tune_all$eff_min_node)), table(tune_all$eff_min_node), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  lr: counts: {paste(names(table(tune_all$eff_lr)), table(tune_all$eff_lr), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  rounds (refit): median={round(median(eff_refit_vec))} | range [{min(eff_refit_vec)}, {max(eff_refit_vec)}]"
)
cli_alert_info("--- VOLUME ---")
cli_alert_info(
  "  num_leaves: mode={as.integer(names(sort(table(tune_all$vol_num_leaves), decreasing=T)[1]))} | counts: {paste(names(table(tune_all$vol_num_leaves)), table(tune_all$vol_num_leaves), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  min_data_in_leaf: mode={as.integer(names(sort(table(tune_all$vol_min_node), decreasing=T)[1]))} | counts: {paste(names(table(tune_all$vol_min_node)), table(tune_all$vol_min_node), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  lr: counts: {paste(names(table(tune_all$vol_lr)), table(tune_all$vol_lr), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  rounds (refit): median={round(median(vol_refit_vec))} | range [{min(vol_refit_vec)}, {max(vol_refit_vec)}]"
)

# ===========================================================================
# COVERAGE SCORING
# ===========================================================================

cli_h1("4b-WR Pooled Coverage (all {n_scored} test rows)")

pooled_wr <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_wr |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("4b-WR Low-Usage Bucket (targets {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low  <- nrow(res_lo)
cli_alert_info("Low-usage rows: {n_low}")

low_wr <- bind_rows(
  score_component(res_lo$epa_per_opp_obs,           res_lo, "eff", "efficiency"),
  score_component(as.numeric(res_lo$opportunities), res_lo, "vol", "volume"),
  score_component(res_lo$total_epa,                 res_lo, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_wr |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# Stratified combined coverage
res_strat <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
      opportunities <= 9L         ~ "mid  (6-9)",
      TRUE                        ~ "high (10+)"
    ) |> factor(levels = c(paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
                            "mid  (6-9)", "high (10+)"))
  )

strat_wr <- eval_calibration_stratified(
  res_strat$total_epa,
  pi_cols(res_strat, "tot"),
  strata = res_strat$opp_bucket
) |> mutate(component = "combined")

cli_h2("Stratified combined coverage at 80% (sharpness honesty check)")
print(strat_wr |> filter(nominal == 0.80) |>
      select(stratum, n, empirical, delta, sharpness) |>
      mutate(delta_pp = fmt_pp(delta)), n = Inf)

# ===========================================================================
# RUBRIC DECISION
# ===========================================================================

cli_h1("Rubric Decision: 4b-WR")

wr_pool80 <- pooled_wr |> filter(component == "combined", nominal == 0.80)
wr_low80  <- low_wr    |> filter(component == "combined", nominal == 0.80)

cli_alert_info("Pooled 80%: delta={fmt_pp(wr_pool80$delta)} | width={fmt_w(wr_pool80$sharpness)}")
cli_alert_info("Low-opp 80%: delta={fmt_pp(wr_low80$delta)} | width={fmt_w(wr_low80$sharpness)}")

if (abs(wr_low80$delta) > 0.10) {
  cli_warn("4b-WR VETOED: low-usage 80% delta = {fmt_pp(wr_low80$delta)} (threshold: +-10pp)")
} else {
  cli_alert_success("4b-WR veto passed: {fmt_pp(wr_low80$delta)}")
  if (abs(wr_pool80$delta) <= 0.02) {
    cli_alert_success("Pooled coverage within +-2pp -- WR model READY")
  } else {
    cli_alert_info("Pooled coverage {fmt_pp(wr_pool80$delta)} -- outside +-2pp; inspect stratified table")
  }
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,  "output/04b_wr_lgbm_fold_predictions.csv")
readr::write_csv(pooled_wr, "output/04b_wr_lgbm_pooled_coverage.csv")
readr::write_csv(low_wr,    "output/04b_wr_lgbm_low_usage_coverage.csv")
readr::write_csv(tune_all,  "output/04b_wr_lgbm_tune_log.csv")

cli_alert_success("output/04b_wr_lgbm_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/04b_wr_lgbm_pooled_coverage.csv")
cli_alert_success("output/04b_wr_lgbm_low_usage_coverage.csv")
cli_alert_success("output/04b_wr_lgbm_tune_log.csv  (full per-fold param audit)")

cli_h1("Step 4b complete")
