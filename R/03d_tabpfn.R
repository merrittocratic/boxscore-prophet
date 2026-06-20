# R/03d_tabpfn.R
# Step 3D: TabPFN efficiency + volume point predictors, frozen conformal construction.
#
# TabPFN (v2, "prior-fitted network") is a foundation model pre-trained on millions of
# synthetic tabular datasets. It learns an in-context posterior over regression functions
# and produces predictions via a single forward pass -- no training loop, no tuning surface.
# That is exactly why it belongs in this run: it sidesteps the tuning dispute entirely.
# A TabPFN prediction for a committee back just IS the foundation model's read of the
# feature vector -- no gradient path to game.
#
# CONSTRUCTION: FROZEN power-law Mechanism A (identical to 3A/3B/3A-v2).
# We are wrapping TabPFN's point predictions in the same conformal shell, not using
# TabPFN's native predictive distribution. That is a future thread (separate paradigm
# comparison). Mixing it here would confound the sharpness re-run with a paradigm change.
#
# PREREQUISITES:
#   - Python 3 with tabpfn >= 8.0.8 installed (pip install tabpfn)
#   - TABPFN_TOKEN environment variable set (register at https://ux.priorlabs.ai,
#     accept license, copy API key from Account page)
#   - reticulate R package installed
#
# FEATURE COUNT / ROW COUNT:
#   EFF: 12 features, VOL: 8 features -- both well within TabPFN v2 limits (2000 features,
#   50000 rows). Training rows range from 284 (fold 1) to 1908 (fold 31). We report the
#   limit check explicitly and abort rather than truncating.
#
# DO NOT: use TabPFN's native predictive intervals. DO NOT modify frozen artifacts.

suppressPackageStartupMessages({
  library(tidyverse)
  library(reticulate)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# TABPFN PREREQUISITES CHECK
# ===========================================================================

cli_h1("Step 3D: TabPFN + Conformal (sharpness re-run)")

# Confirm TABPFN_TOKEN is set before trying to import
token <- Sys.getenv("TABPFN_TOKEN", unset = "")
if (nchar(token) == 0L) {
  cli_abort(
    c(
      "TABPFN_TOKEN environment variable not set.",
      "i" = "1. Register at https://ux.priorlabs.ai and accept the license",
      "i" = "2. Copy your API key from https://ux.priorlabs.ai/account",
      "i" = "3. Set the variable: Sys.setenv(TABPFN_TOKEN = '<your-key>')",
      "i" = "   or add it to ~/.Renviron and restart R"
    )
  )
}
cli_alert_success("TABPFN_TOKEN set (length {nchar(token)})")

# Point reticulate at the Python that has tabpfn installed
PYTHON_PATH <- Sys.getenv("RETICULATE_PYTHON", unset = "/opt/homebrew/bin/python3")
use_python(PYTHON_PATH, required = TRUE)
cli_alert_info("Python: {py_config()$python}")

# Import tabpfn -- abort cleanly rather than propagating a cryptic Python error
if (!py_module_available("tabpfn")) {
  cli_abort("tabpfn Python package not found. Install with: pip3 install tabpfn --break-system-packages")
}
tabpfn  <- import("tabpfn")
np      <- import("numpy")

# TabPFN's license check fires on the first .fit() call and reads os.environ.
# Reticulate does not automatically forward R's env vars into Python's os.environ,
# so we pass the token via py$ (R -> Python bridge) and set it explicitly.
py$tabpfn_token_val <- token
py_run_string("import os; os.environ['TABPFN_TOKEN'] = tabpfn_token_val")
cli_alert_success("TABPFN_TOKEN injected into Python os.environ")

cli_alert_success("tabpfn version: {tabpfn$`__version__`}")

# TabPFN v2 documented limits (pre-training range)
TABPFN_MAX_ROWS     <- 50000L
TABPFN_MAX_FEATURES <- 2000L

# ===========================================================================
# PARAMETERS
# ===========================================================================

CAL_FRAC        <- 0.20
LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L
TABPFN_N_EST    <- 16L   # ensemble size; 16 is a good default for this feature count

# Mechanism A constants (frozen)
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

# TabPFN requires complete cases; impute NA features to training median.
# Same approach as 3C (ensures same rows scored as 3A/3B).
impute_to_train_median <- function(train_df, test_df, feature_cols) {
  medians <- sapply(feature_cols, function(c) median(train_df[[c]], na.rm = TRUE))
  apply_impute <- function(df) {
    for (col in feature_cols) {
      na_idx <- is.na(df[[col]])
      if (any(na_idx)) df[[col]][na_idx] <- medians[[col]]
    }
    df
  }
  list(train = apply_impute(train_df), test = apply_impute(test_df))
}

to_numpy <- function(df, features) {
  np$array(as.matrix(df |> select(all_of(features))), dtype = "float64")
}

# Fit TabPFN regressor on training data, predict on test.
# Aborts if row or feature count exceeds TabPFN limits.
tabpfn_fit_predict <- function(X_train, y_train, X_test, label, fold_id) {
  n_train <- nrow(X_train)
  n_feat  <- ncol(X_train)

  if (n_train > TABPFN_MAX_ROWS || n_feat > TABPFN_MAX_FEATURES) {
    cli_abort(
      "Fold {fold_id} [{label}]: data exceeds TabPFN limits (rows={n_train}/{TABPFN_MAX_ROWS}, features={n_feat}/{TABPFN_MAX_FEATURES}). Stop per protocol -- do not silently truncate."
    )
  }

  keep     <- !is.na(y_train)
  X_tr_np  <- np$array(X_train[keep, , drop = FALSE], dtype = "float64")
  y_tr_np  <- np$array(as.numeric(y_train[keep]),     dtype = "float64")
  X_te_np  <- np$array(X_test,                        dtype = "float64")

  reg <- tabpfn$TabPFNRegressor(n_estimators = TABPFN_N_EST, random_state = 42L)
  reg$fit(X_tr_np, y_tr_np)
  preds <- reg$predict(X_te_np)

  as.numeric(preds)
}

# TabPFN also predicts on calibration rows (needed for conformal residuals)
tabpfn_predict_cal <- function(X_train, y_train, X_cal, label, fold_id) {
  keep     <- !is.na(y_train)
  X_tr_np  <- np$array(X_train[keep, , drop = FALSE], dtype = "float64")
  y_tr_np  <- np$array(as.numeric(y_train[keep]),     dtype = "float64")
  X_ca_np  <- np$array(X_cal, dtype = "float64")

  reg <- tabpfn$TabPFNRegressor(n_estimators = TABPFN_N_EST, random_state = 42L)
  reg$fit(X_tr_np, y_tr_np)
  as.numeric(reg$predict(X_ca_np))
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
  out <- tibble(p=pred, lo50=pred-qs[1], hi50=pred+qs[1],
                lo80=pred-qs[2], hi80=pred+qs[2], lo90=pred-qs[3], hi90=pred+qs[3])
  names(out) <- c(paste0("pred_",suffix),
                  paste0("lo_50_",suffix), paste0("hi_50_",suffix),
                  paste0("lo_80_",suffix), paste0("hi_80_",suffix),
                  paste0("lo_90_",suffix), paste0("hi_90_",suffix))
  out
}

build_row_intervals <- function(pred, hw50, hw80, hw90, suffix) {
  out <- tibble(p=pred, lo50=pred-hw50, hi50=pred+hw50,
                lo80=pred-hw80, hi80=pred+hw80, lo90=pred-hw90, hi90=pred+hw90)
  names(out) <- c(paste0("pred_",suffix),
                  paste0("lo_50_",suffix), paste0("hi_50_",suffix),
                  paste0("lo_80_",suffix), paste0("hi_80_",suffix),
                  paste0("lo_90_",suffix), paste0("hi_90_",suffix))
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
fmt_w  <- function(x) round(x, 3)

score_lo <- function(preds_lo, label) {
  bind_rows(
    score_component(preds_lo$epa_per_opp_obs,           preds_lo, "eff", "efficiency"),
    score_component(as.numeric(preds_lo$opportunities), preds_lo, "vol", "volume"),
    score_component(preds_lo$total_epa,                 preds_lo, "tot", "combined")
  ) |>
    mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI), model = label)
}

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_alert_info("TabPFN n_estimators={TABPFN_N_EST} | limits: rows<={TABPFN_MAX_ROWS}, features<={TABPFN_MAX_FEATURES}")
cli_alert_info("EFF features: {length(EFF_FEATURES)} | VOL features: {length(VOL_FEATURES)} -- within limits")
cli_alert_info("Construction: FROZEN power-law Mechanism A (identical to 3A/3B/3A-v2)")
cli_alert_info("Native TabPFN distribution: NOT USED (future thread -- do not build now)")

ft       <- readRDS("data/rb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- sum(fold_map$n_test_rows)
cli_alert_success("Feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds -- TabPFN forward pass per fold)")
cli_alert_info("Note: two TabPFN.fit() calls per fold (cal + test prediction sets handled separately)")

fold_results <- vector("list", nrow(fold_map))
alpha_log    <- numeric(nrow(fold_map))
pred_count   <- 0L   # running count for non-NA check

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
    cli_abort("Fold {f}: test leaked into cal")
  }

  fit_sws  <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # Impute NA features before extracting matrices
  imp_eff <- impute_to_train_median(fit_data, list(cal=cal_data, test=test_data),
                                    intersect(EFF_FEATURES, names(fit_data)))
  # impute_to_train_median only takes two dfs; handle three:
  imp_eff_cal  <- impute_to_train_median(fit_data, cal_data,  intersect(EFF_FEATURES, names(fit_data)))
  imp_eff_test <- impute_to_train_median(fit_data, test_data, intersect(EFF_FEATURES, names(fit_data)))
  fit_eff  <- imp_eff_cal$train;  cal_eff  <- imp_eff_cal$test
  imp_eff_test2 <- impute_to_train_median(fit_data, test_data, intersect(EFF_FEATURES, names(fit_data)))
  test_eff <- imp_eff_test2$test

  imp_vol_cal  <- impute_to_train_median(fit_data, cal_data,  intersect(VOL_FEATURES, names(fit_data)))
  imp_vol_test <- impute_to_train_median(fit_data, test_data, intersect(VOL_FEATURES, names(fit_data)))
  fit_vol  <- imp_vol_cal$train;  cal_vol  <- imp_vol_cal$test;  test_vol <- imp_vol_test$test

  X_fit_eff  <- as.matrix(fit_eff  |> select(all_of(EFF_FEATURES)))
  X_cal_eff  <- as.matrix(cal_eff  |> select(all_of(EFF_FEATURES)))
  X_test_eff <- as.matrix(test_eff |> select(all_of(EFF_FEATURES)))
  X_fit_vol  <- as.matrix(fit_vol  |> select(all_of(VOL_FEATURES)))
  X_cal_vol  <- as.matrix(cal_vol  |> select(all_of(VOL_FEATURES)))
  X_test_vol <- as.matrix(test_vol |> select(all_of(VOL_FEATURES)))

  # Row/feature limit check (abort, never truncate)
  for (lbl in c("eff", "vol")) {
    X_tr <- if (lbl == "eff") X_fit_eff else X_fit_vol
    if (nrow(X_tr) > TABPFN_MAX_ROWS || ncol(X_tr) > TABPFN_MAX_FEATURES) {
      cli_abort(
        "Fold {f} [{lbl}]: rows={nrow(X_tr)} features={ncol(X_tr)} exceeds TabPFN limits. Stop."
      )
    }
  }

  # -- Efficiency: fit on fit_data, predict cal + test --
  # Two separate fit() calls because TabPFN cannot reuse a fitted model for
  # different test sets in all versions. This doubles inference time but keeps
  # the interface clean and avoids version-specific workarounds.
  pred_cal_eff  <- tabpfn_predict_cal(X_fit_eff, fit_data$epa_per_opp_obs, X_cal_eff,  "eff", f)
  pred_test_eff <- tabpfn_fit_predict(X_fit_eff, fit_data$epa_per_opp_obs, X_test_eff, "eff", f)

  # -- Volume --
  pred_cal_vol  <- tabpfn_predict_cal(X_fit_vol, as.numeric(fit_data$opportunities), X_cal_vol,  "vol", f)
  pred_test_vol <- tabpfn_fit_predict(X_fit_vol, as.numeric(fit_data$opportunities), X_test_vol, "vol", f)

  # -- Component conformal intervals --
  resid_eff <- abs(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_eff    <- c(conformal_q(resid_eff, 0.50),
                 conformal_q(resid_eff, 0.80),
                 conformal_q(resid_eff, 0.90))

  resid_vol <- abs(as.numeric(cal_data$opportunities) - pred_cal_vol)
  qs_vol    <- c(conformal_q(resid_vol, 0.50),
                 conformal_q(resid_vol, 0.80),
                 conformal_q(resid_vol, 0.90))

  # -- Combined: frozen Mechanism A power-law --
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

  pred_count <- pred_count + nrow(test_data)

  t1 <- proc.time()[["elapsed"]]
  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | alpha={round(alpha, 3)} | {round(t1-t0)}s"
  )
}

results <- bind_rows(fold_results)

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
  cli_warn("Row count mismatch: {n_scored} scored, {EXPECTED_TEST_N} expected")
}

na_eff <- sum(is.na(results$pred_eff))
na_vol <- sum(is.na(results$pred_vol))
na_tot <- sum(is.na(results$pred_tot))
if (na_eff + na_vol + na_tot == 0L) {
  cli_alert_success("Zero NA predictions -- no silent fallback to mean")
} else {
  cli_warn("NA predictions: eff={na_eff}, vol={na_vol}, tot={na_tot}")
}

cli_alert_success("Conformal calibration: training-side only (fit_data, verified per fold)")
cli_alert_info("alpha_fold range [{round(min(alpha_log),3)}, {round(max(alpha_log),3)}] | median {round(median(alpha_log),3)}")

# ===========================================================================
# 3D COVERAGE SCORING
# ===========================================================================

cli_h1("3D Pooled Coverage (all {n_scored} test rows)")

pooled_3d <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_3d |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("3D Low-Usage Bucket (opp {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo_3d <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low     <- nrow(res_lo_3d)
cli_alert_info("Low-usage rows: {n_low}")

low_3d <- bind_rows(
  score_component(res_lo_3d$epa_per_opp_obs,           res_lo_3d, "eff", "efficiency"),
  score_component(as.numeric(res_lo_3d$opportunities), res_lo_3d, "vol", "volume"),
  score_component(res_lo_3d$total_epa,                 res_lo_3d, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_3d |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# ===========================================================================
# FULL FIVE-WAY COMPARISON
# ===========================================================================

cli_h1("Five-Way Comparison: 3A / 3A-v2 / 3B / 3C / 3D")

pooled_3a  <- readr::read_csv("output/03a_lgbm_pooled_coverage.csv",    show_col_types = FALSE)
preds_3a   <- readr::read_csv("output/03a_lgbm_fold_predictions.csv",   show_col_types = FALSE)
pooled_3b  <- readr::read_csv("output/03b_rf_pooled_coverage.csv",      show_col_types = FALSE)
preds_3b   <- readr::read_csv("output/03b_rf_fold_predictions.csv",     show_col_types = FALSE)
pooled_3c  <- readr::read_csv("output/03c_hier_pooled_coverage.csv",    show_col_types = FALSE)
preds_3c   <- readr::read_csv("output/03c_hier_fold_predictions.csv",   show_col_types = FALSE)
pooled_v2  <- readr::read_csv("output/03a_v2_lgbm_pooled_coverage.csv", show_col_types = FALSE)
preds_v2   <- readr::read_csv("output/03a_v2_lgbm_fold_predictions.csv", show_col_types = FALSE)

# Low-usage scores from fold predictions
res_lo <- function(preds) preds |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)

lo_all <- bind_rows(
  score_lo(res_lo(preds_3a), "3A-LightGBM(default)"),
  score_lo(res_lo(preds_v2), "3A-v2-LightGBM(tuned)"),
  score_lo(res_lo(preds_3b), "3B-RF              "),
  score_lo(res_lo(preds_3c), "3C-HierBayes       "),
  score_lo(res_lo_3d,        "3D-TabPFN          ")
)

pool_comb <- bind_rows(
  pooled_3a |> filter(component=="combined") |> mutate(model="3A-LightGBM(default)",  stratum="pooled"),
  pooled_v2 |> filter(component=="combined") |> mutate(model="3A-v2-LightGBM(tuned)", stratum="pooled"),
  pooled_3b |> filter(component=="combined") |> mutate(model="3B-RF              ",   stratum="pooled"),
  pooled_3c |> filter(component=="combined") |> mutate(model="3C-HierBayes       ",   stratum="pooled"),
  pooled_3d |> filter(component=="combined") |> mutate(model="3D-TabPFN          ",   stratum="pooled")
)

lo_comb <- lo_all |> filter(component == "combined")
all_comb <- bind_rows(pool_comb, lo_comb)

cli_h2("1. Rubric Table (all five models)")
cli_alert_info("model                    | pooled 80% | low 80%  | veto  | pooled width")
all_comb |>
  filter(nominal == 0.80) |>
  select(model, stratum, delta, sharpness) |>
  pivot_wider(names_from = stratum, values_from = c(delta, sharpness)) |>
  mutate(
    veto    = ifelse(abs(.data[[paste0("delta_low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI)]]) > 0.10,
                     "TRIGGER", "pass"),
    row     = paste0(
      str_pad(model, 24), " | ",
      str_pad(fmt_pp(.data[["delta_pooled"]]), 10), " | ",
      str_pad(fmt_pp(.data[[paste0("delta_low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI)]]), 9), " | ",
      str_pad(veto, 7), " | ",
      fmt_w(.data[["sharpness_pooled"]])
    )
  ) |>
  arrange(abs(.data[["delta_pooled"]])) |>
  pull(row) |>
  walk(cli_alert_info)

cli_h2("2. Sharpness Head-to-Head (conformal entrants: 3A / 3A-v2 / 3B / 3D)")
cli_alert_info("Stratified combined width at 80% (coverage must hold per bucket)")

strat_table <- map_dfr(
  list(
    list(preds = preds_3a, model = "3A-LightGBM(default)"),
    list(preds = preds_v2, model = "3A-v2-LightGBM(tuned)"),
    list(preds = preds_3b, model = "3B-RF              "),
    list(preds = results,  model = "3D-TabPFN          ")
  ),
  function(x) {
    p <- x$preds
    eval_calibration_stratified(
      p$total_epa,
      pi_cols(p, "tot"),
      strata = case_when(
        p$opportunities <= LOW_OPP_HI ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
        p$opportunities <= 13L        ~ "mid  (9-13)",
        TRUE                          ~ "high (14+)"
      )
    ) |> mutate(model = x$model)
  }
)

cli_alert_info("model                    | low delta | low wid | mid delta | mid wid | hi delta | hi wid")
strat_table |>
  filter(nominal == 0.80) |>
  select(model, stratum, delta, sharpness) |>
  pivot_wider(names_from = stratum, values_from = c(delta, sharpness)) |>
  mutate(row = paste0(
    str_pad(model, 24), " | ",
    str_pad(fmt_pp(.data[[paste0("delta_low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")")]]), 10), " | ",
    str_pad(fmt_w(.data[[paste0("sharpness_low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")")]]), 8), " | ",
    str_pad(fmt_pp(.data[["delta_mid  (9-13)"]]),    10), " | ",
    str_pad(fmt_w(.data[["sharpness_mid  (9-13)"]]), 8), " | ",
    str_pad(fmt_pp(.data[["delta_high (14+)"]]),     9),  " | ",
    fmt_w(.data[["sharpness_high (14+)"]])
  )) |>
  pull(row) |> walk(cli_alert_info)

cli_h2("3. Low-Opp Efficiency 80% (how much of 3A's -8.7pp was tuning vs family?)")
lo_all |>
  filter(component == "efficiency", nominal == 0.80) |>
  mutate(row = paste0(str_pad(model, 24), ": delta=", fmt_pp(delta), " | width=", fmt_w(sharpness))) |>
  pull(row) |> walk(cli_alert_info)

# ===========================================================================
# DECISION RULE -- FINAL VERDICT
# ===========================================================================

cli_h1("Final Rubric Decision (all five models)")
cli_alert_info("PRIMARY: pooled combined 80% closest to nominal")
cli_alert_info("TIEBREAK: sharpest (narrowest combined 80% width) among models within +-2pp")
cli_alert_info("VETO: low-usage combined 80% > +-10pp disqualifies")

rubric <- all_comb |>
  filter(nominal == 0.80) |>
  select(model, stratum, delta, sharpness) |>
  pivot_wider(names_from = stratum, values_from = c(delta, sharpness))

lo_col <- paste0("delta_low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI)

vetoed   <- rubric |> filter(abs(.data[[lo_col]]) > 0.10) |> pull(model)
eligible <- rubric |> filter(!model %in% vetoed)

if (length(vetoed) > 0L) {
  cli_warn("Vetoed: {paste(trimws(vetoed), collapse=', ')}")
}

within_2pp <- eligible |> filter(abs(delta_pooled) <= 0.02)

if (nrow(within_2pp) == 0L) {
  winner <- eligible |> slice_min(abs(delta_pooled)) |> pull(model)
  cli_alert_success("Winner (primary, no model within +-2pp): {trimws(winner[1])}")
} else if (nrow(within_2pp) == 1L) {
  winner <- within_2pp$model
  cli_alert_success("Winner (only model within +-2pp): {trimws(winner)}")
} else {
  winner <- within_2pp |> slice_min(sharpness_pooled) |> pull(model)
  cli_alert_success("Tiebreak (all within +-2pp, sharpest wins): {trimws(winner[1])}")
  within_2pp |>
    arrange(sharpness_pooled) |>
    mutate(row = paste0(str_pad(model, 24), ": delta=", fmt_pp(delta_pooled),
                        " | width=", fmt_w(sharpness_pooled))) |>
    pull(row) |> walk(cli_alert_info)
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,   "output/03d_tabpfn_fold_predictions.csv")
readr::write_csv(pooled_3d, "output/03d_tabpfn_pooled_coverage.csv")
readr::write_csv(low_3d,    "output/03d_tabpfn_low_usage_coverage.csv")

cli_alert_success("output/03d_tabpfn_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/03d_tabpfn_pooled_coverage.csv")
cli_alert_success("output/03d_tabpfn_low_usage_coverage.csv")

cli_h1("Step 3D complete -- five-way bake-off verdict above")
