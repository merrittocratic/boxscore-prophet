# R/11c_rb_injury_ab.R
# Ablation ladder rung 1, arm B: the frozen 03a-v2 nested-walk-forward
# procedure with EX-ANTE INJURY STATE FEATURES (11b) added to the VOLUME
# model only. Efficiency features untouched -- the 11a/11b diagnostics
# located the usage-shock bias entirely in volume.
#
# CLONE DISCIPLINE: this file is 03a_v2_lgbm_tuned.R verbatim except for
# (1) this header, (2) the injury-state join + VOL_FEATURES extension,
# (3) 11c_ output paths, (4) FOLD_SUBSET env for smoke tests, and (5) the
# comparison tail, which now scores ARM A (shipped 03a-v2 predictions,
# unchanged on disk) vs ARM B (this run) under the FROZEN rubric:
# 80% coverage primary, sharpness tiebreak, low-usage veto intact,
# fold map unchanged -- plus the transition-state slice the family
# was built to fix.
#
# PRE-STATED EXPECTATIONS (2026-07-18, before the run):
#   - above_new_out rows: vol RMSE improves materially; mean vol residual
#     moves from +3.8 toward 0
#   - return_week rows: mean vol residual moves from -1.0 toward 0
#   - pooled 80% coverage unchanged within noise (+-1.5pp); low-usage veto
#     stays green; pooled sharpness within noise (transition rows are ~4%
#     of the table -- pooled metrics should barely move)
#   - Decision rule: ship if transition-state RMSE improves AND no rubric
#     regression. Flat transition-state result = rung 1 dies, publish null.
#
# LEAKAGE DISCIPLINE:
#   Tuning happens STRICTLY inside each fold's training rows. The outer test fold
#   is never seen until final scoring. Concretely:
#     - Inner split: the same last-20%-season-weeks holdout used for conformal
#       calibration in 3A. The same rows serve as inner validation for RMSE.
#       This is deliberate and clean: tuning selects params on inner RMSE;
#       conformal then runs fresh on those same cal rows with the chosen params.
#       No circularity because tuning uses RMSE (point accuracy) and conformal
#       uses residuals (uncertainty calibration) -- different statistics, same holdout.
#     - Outer test rows contribute ONLY to the evaluation table, never to
#       hyperparameter selection.
#
# TUNING OBJECTIVE:
#   Inner-holdout RMSE on efficiency (for eff model) and volume (for vol model),
#   tuned SEPARATELY. NOT coverage, NOT sharpness -- those are the bake-off
#   criteria. A sharpness gain must emerge from better point estimates, not be
#   the target. Efficiency and volume have different distributions and may prefer
#   different configs.
#
# GRID:
#   num_leaves x min_data_in_leaf x learning_rate
#   {7, 15, 31} x {20, 50, 100} x {0.02, 0.05} = 18 combos per component per fold
#   Centered on the corrections 3A needed:
#     - num_leaves DOWN: 31 over-carves a small training set
#     - min_data_in_leaf UP: at 20, a 432-row low-opp tail can form a leaf off
#       ~5% of the data (memorizing Jaylen Warren weeks, not learning committee backs)
#     - Early stopping per combo: let the data choose num_rounds, not a fixed count
#
# CONSTRUCTION: FROZEN. Same power-law Mechanism A as 3A/3B.
# RUBRIC: FROZEN. Same veto, decision rule, folds.
#
# DO NOT: tune to sharpness or coverage. DO NOT edit 3A's numbers.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS -- pre-committed; the grid is locked before any fold runs
# ===========================================================================

CAL_FRAC   <- 0.20
LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

# Tuning grid (32 combos per component per fold)
# Expanded vs original 18-combo grid: added num_leaves=63 and min_data_in_leaf=10
# to let the data ask for deeper trees and smaller leaf floors now that the
# training corpus is ~6x larger than when the original grid was designed.
TUNE_GRID <- expand.grid(
  num_leaves       = c(7L, 15L, 31L, 63L),
  min_data_in_leaf = c(10L, 20L, 50L, 100L),
  lr               = c(0.02, 0.05)
)

# Inner early stopping settings
INNER_MAX_ROUNDS  <- 500L
INNER_EARLY_STOP  <- 20L   # patience: stop if no improvement in 20 rounds

# Refit uses the SAME fit_data as the inner tuning (not a larger set, because cal_data
# is reserved for conformal calibration). So the correct refit round count is exactly
# best_iter, not scaled. Floor at 10 to prevent degenerate single-round models in
# the earliest folds where inner holdouts are tiny and early stopping fires immediately.
REFIT_ROUNDS_MIN  <- 10L

# Fixed params shared across all combos (not on the grid -- these are not the bottleneck)
LGBM_FIXED <- list(
  objective          = "regression",
  metric             = "rmse",
  feature_fraction   = 0.8,
  bagging_fraction   = 0.8,
  bagging_freq       = 5L,
  seed               = 42L,
  verbose            = -1L,
  num_threads        = 1L,
  # Disable feature pre-filtering so min_data_in_leaf can vary across grid combos
  # without the Dataset caching stale filter decisions from the first combo seen.
  feature_pre_filter = FALSE
)

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

# Mechanism A power-law constants (frozen from 3A)
ALPHA_LO       <- 0.20
ALPHA_HI       <- 0.90
ALPHA_FALLBACK <- 0.50

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
# Fits each param combo on inner_fit, scores RMSE on inner_val.
# Returns the winning params and round count.
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

    # lgb.Dataset for validation must reference dtrain so feature binning is shared
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

# Fit final model with selected params. rounds = best_inner_rounds * REFIT_ROUNDS_MIN
# to account for larger training set (no early stopping on final fit).
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

cli_h1("Step 3A-v2: Nested-Walk-Forward-Tuned LightGBM")
cli_alert_info("Grid: {nrow(TUNE_GRID)} combos per component per fold (eff and vol tuned separately)")
cli_alert_info("Inner split: last {CAL_FRAC*100}% season-weeks as RMSE holdout (same as cal split)")
cli_alert_info("Construction: FROZEN Mechanism A power-law (identical to 3A/3B)")
cli_alert_info("Rubric: FROZEN (unchanged veto, decision rule, folds)")

ft       <- readRDS("data/rb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- sum(fold_map$n_test_rows)
cli_alert_success("Feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

# --- ARM B DELTA: ex-ante injury states (11b) into the volume model ---
INJURY_FEATURES <- c(
  "own_q_int", "own_practice_int", "weeks_missed", "return_from_absence",
  "above_new_out_share", "above_q_share", "above_long_out_share"
)
inj_states <- readRDS("data/injury_states_rb.rds")
ft <- ft |> left_join(inj_states, by = c("player_id", "season", "week"))
stopifnot(!any(is.na(ft$own_practice_int)))
VOL_FEATURES <- c(VOL_FEATURES, INJURY_FEATURES)
cli_alert_info("ARM B: volume features extended with {length(INJURY_FEATURES)} injury-state features")

if (nzchar(Sys.getenv("FOLD_SUBSET"))) {
  fold_map <- tail(fold_map, as.integer(Sys.getenv("FOLD_SUBSET")))
  cli_alert_warning("FOLD_SUBSET={nrow(fold_map)} folds (smoke test only)")
}

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

  # Build matrices for inner tuning
  X_fit_eff <- make_matrix(fit_data, EFF_FEATURES)
  X_cal_eff <- make_matrix(cal_data, EFF_FEATURES)
  X_fit_vol <- make_matrix(fit_data, VOL_FEATURES)
  X_cal_vol <- make_matrix(cal_data, VOL_FEATURES)

  # --- INNER TUNING: efficiency ---
  # Tune point-prediction RMSE on cal_data rows. cal_data is in-sample for the
  # outer fold, so no outer test leakage. Tuning objective is RMSE ONLY.
  best_eff <- tune_lgbm_component(
    X_fit_eff, fit_data$epa_per_opp_obs,
    X_cal_eff, cal_data$epa_per_opp_obs,
    label = paste0("fold", f, "_eff")
  )

  # --- INNER TUNING: volume ---
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

  # --- FINAL REFIT with selected params ---
  # Refit uses the same fit_data as the inner tuning, so the optimal round count
  # is exactly best_iter (not scaled). Floor at REFIT_ROUNDS_MIN for early folds
  # where inner holdouts are too small for early stopping to converge properly.
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

  # --- Component conformal intervals (unchanged from 3A) ---
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

  resid_norm  <- raw_resid_cal / cal_opp^alpha
  q_norm      <- c(conformal_q(resid_norm, 0.50),
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

cli_h2("Summary across 31 folds")
cli_alert_info("--- EFFICIENCY ---")
cli_alert_info(
  "  num_leaves: mode={as.integer(names(sort(table(tune_all$eff_num_leaves), decreasing=T)[1])) } | counts: {paste(names(table(tune_all$eff_num_leaves)), table(tune_all$eff_num_leaves), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  min_data_in_leaf: mode={as.integer(names(sort(table(tune_all$eff_min_node), decreasing=T)[1]))} | counts: {paste(names(table(tune_all$eff_min_node)), table(tune_all$eff_min_node), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  lr: counts: {paste(names(table(tune_all$eff_lr)), table(tune_all$eff_lr), sep='=', collapse=' | ')}"
)
cli_alert_info(
  "  rounds (refit): median={round(median(ceiling(tune_all$eff_rounds * REFIT_ROUNDS_MIN)))} | range [{min(ceiling(tune_all$eff_rounds*REFIT_ROUNDS_MIN))}, {max(ceiling(tune_all$eff_rounds*REFIT_ROUNDS_MIN))}]"
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
  "  rounds (refit): median={round(median(ceiling(tune_all$vol_rounds * REFIT_ROUNDS_MIN)))} | range [{min(ceiling(tune_all$vol_rounds*REFIT_ROUNDS_MIN))}, {max(ceiling(tune_all$vol_rounds*REFIT_ROUNDS_MIN))}]"
)

# Default vs tuned comparison (what the data asked for vs what 3A had)
cli_h2("Default (3A) vs data-chosen (3A-v2)")
cli_alert_info("3A used: num_leaves=31, min_data_in_leaf=20, lr=0.05, rounds=200 (fixed)")
cli_alert_info("3A-v2 efficiency modal: leaves={as.integer(names(sort(table(tune_all$eff_num_leaves), decreasing=T)[1]))} | minnode={as.integer(names(sort(table(tune_all$eff_min_node), decreasing=T)[1]))} | lr_mode={names(sort(table(tune_all$eff_lr), decreasing=T)[1])}")
cli_alert_info("3A-v2 volume modal:     leaves={as.integer(names(sort(table(tune_all$vol_num_leaves), decreasing=T)[1]))} | minnode={as.integer(names(sort(table(tune_all$vol_min_node), decreasing=T)[1]))} | lr_mode={names(sort(table(tune_all$vol_lr), decreasing=T)[1])}")

# ===========================================================================
# COVERAGE SCORING -- 3A-v2
# ===========================================================================

cli_h1("3A-v2 Pooled Coverage (all {n_scored} test rows)")

pooled_v2 <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_v2 |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("3A-v2 Low-Usage Bucket (opp {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo_v2 <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low     <- nrow(res_lo_v2)
cli_alert_info("Low-usage rows: {n_low}")

low_v2 <- bind_rows(
  score_component(res_lo_v2$epa_per_opp_obs,           res_lo_v2, "eff", "efficiency"),
  score_component(as.numeric(res_lo_v2$opportunities), res_lo_v2, "vol", "volume"),
  score_component(res_lo_v2$total_epa,                 res_lo_v2, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_v2 |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# Stratified combined coverage (sharpness honesty check per 3B standard)
res_strat_v2 <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
      opportunities <= 13L        ~ "mid  (9-13)",
      TRUE                        ~ "high (14+)"
    ) |> factor(levels = c(paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
                            "mid  (9-13)", "high (14+)"))
  )

strat_v2 <- eval_calibration_stratified(
  res_strat_v2$total_epa,
  pi_cols(res_strat_v2, "tot"),
  strata = res_strat_v2$opp_bucket
) |> mutate(component = "combined")

cli_h2("Stratified combined coverage at 80% (sharpness honesty check)")
print(strat_v2 |> filter(nominal == 0.80) |>
      select(stratum, n, empirical, delta, sharpness) |>
      mutate(delta_pp = fmt_pp(delta)), n = Inf)


# ===========================================================================
# SAVE ARM B OUTPUTS
# ===========================================================================

cli_h1("Save arm B outputs")

readr::write_csv(results,   "output/11c_rb_injury_fold_predictions.csv")
readr::write_csv(pooled_v2, "output/11c_rb_injury_pooled_coverage.csv")
readr::write_csv(low_v2,    "output/11c_rb_injury_low_usage_coverage.csv")
readr::write_csv(tune_all,  "output/11c_rb_injury_tune_log.csv")
cli_alert_success("output/11c_rb_injury_* written")

# ===========================================================================
# FROZEN-RUBRIC COMPARISON: ARM A (shipped 03a-v2) vs ARM B (+injury)
# ===========================================================================

cli_h1("Frozen rubric: ARM A (shipped) vs ARM B (+injury states)")

preds_a <- readr::read_csv("output/03a_v2_lgbm_fold_predictions.csv",
                           show_col_types = FALSE) |>
  filter(!is.na(player_id)) |>
  semi_join(results, by = c("season", "week"))   # identical fold coverage
stopifnot(nrow(preds_a) == nrow(results))

score_arm <- function(preds, label) {
  lo <- preds |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
  bind_rows(
    score_component(preds$total_epa, preds, "tot", "combined") |>
      mutate(stratum = "pooled", n = nrow(preds)),
    score_component(lo$total_epa, lo, "tot", "combined") |>
      mutate(stratum = "low_usage", n = nrow(lo)),
    score_component(as.numeric(preds$opportunities), preds, "vol", "volume") |>
      mutate(stratum = "pooled", n = nrow(preds))
  ) |> mutate(arm = label)
}

rubric <- bind_rows(score_arm(preds_a, "A_shipped"),
                    score_arm(results, "B_injury")) |>
  filter(nominal == 0.80) |>
  select(arm, component, stratum, n, empirical, delta, sharpness)

cli_h2("80% coverage + width (delta pp / width) -- veto: low-usage |delta| > 10pp")
print(rubric |> mutate(delta_pp = fmt_pp(delta), width = fmt_w(sharpness),
                       veto = ifelse(stratum == "low_usage" & abs(delta) > 0.10,
                                     "TRIGGER", "pass")), n = Inf)

# ===========================================================================
# TRANSITION-STATE SLICE (the population the family targets)
# ===========================================================================

cli_h1("Transition-state slice: vol residual + RMSE + combined 80% coverage")

slice_arm <- function(preds, label) {
  preds |>
    left_join(inj_states, by = c("player_id", "season", "week")) |>
    mutate(
      vol_resid = as.numeric(opportunities) - pred_vol,
      cov80 = total_epa >= lo_80_tot & total_epa <= hi_80_tot,
      state = case_when(
        return_from_absence == 1 & above_new_out_share > 0 ~ "return+above_out",
        return_from_absence == 1                            ~ "return_week",
        above_new_out_share > 0                             ~ "above_new_out",
        above_long_out_share > 0                            ~ "above_long_out",
        above_q_share > 0                                   ~ "above_q_only",
        .default                                            = "steady"
      )
    ) |>
    group_by(state) |>
    summarise(n = n(),
              mean_vol_resid = mean(vol_resid),
              vol_rmse = sqrt(mean(vol_resid^2)),
              cov80 = mean(cov80),
              width80 = mean(hi_80_tot - lo_80_tot),
              .groups = "drop") |>
    mutate(arm = label)
}

trans <- bind_rows(slice_arm(preds_a, "A_shipped"), slice_arm(results, "B_injury"))

trans_wide <- trans |>
  pivot_wider(names_from = arm,
              values_from = c(mean_vol_resid, vol_rmse, cov80, width80)) |>
  mutate(
    d_resid  = mean_vol_resid_B_injury - mean_vol_resid_A_shipped,
    d_rmse   = vol_rmse_B_injury - vol_rmse_A_shipped,
    d_cov_pp = 100 * (cov80_B_injury - cov80_A_shipped)
  ) |>
  arrange(desc(abs(mean_vol_resid_A_shipped)))

print(trans_wide |> mutate(across(where(is.numeric) & !c(n), ~ round(.x, 2))),
      n = Inf, width = Inf)

readr::write_csv(rubric,     "output/11c_rubric_table.csv")
readr::write_csv(trans_wide, "output/11c_transition_slice.csv")
cli_alert_success("output/11c_rubric_table.csv + output/11c_transition_slice.csv")

cli_h1("11c arm B complete -- judge per pre-stated rule in the header")
