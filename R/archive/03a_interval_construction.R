# R/03a_interval_construction.R
# Selects the combined-interval construction for 3A-baseline and 3B.
# Judge: flatness of normalized residuals across opp buckets (5-8 / 9-13 / 14+)
#        on the FROZEN 1,682-row test set. Coverage is reported for the winner only.
#
# Mechanism A: per-fold power-law normalization
#   -- fit raw_resid ~ c * opp^alpha via log-log OLS on each fold's cal set
#   -- conformalize on the normalized (resid / opp^alpha) scale -> one quantile per fold
#   -- rescale per test row: half_width_i = q_norm * opp_i^alpha
#
# Mechanism B: locally-weighted conformal
#   -- for each test row at opp = k, conformal quantile from cal rows within +-BW_B of k
#   -- no parametric form; width tracks local volatility directly
#   -- fallback to global quantile when fewer than MIN_NEIGHBORS_B cal rows in window
#
# Frozen: rb_feature_table.rds, fold_map.rds, metrics.R
# Frozen: LightGBM model, hyperparams, efficiency/volume interval construction
# DO NOT modify frozen inputs or tune toward passing.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS -- identical to 03a_lgbm_control.R (model must stay frozen)
# ===========================================================================

CAL_FRAC <- 0.20

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

LOW_OPP_LO      <- 5L
LOW_OPP_HI      <- 8L
EXPECTED_TEST_N <- 1682L

# --- Mechanism A parameters ---
# Run 2 (linear, alpha=1) undercovered low-opp; run 1 (alpha=0) overcovered.
# Log-log regression of raw_resid ~ opp across prior test runs gave alpha ~ 0.48,
# so the true exponent is between 0 and 1 and sub-linear. Clamp range set wide enough
# to let the data speak, narrow enough to exclude degenerate solutions.
ALPHA_LO       <- 0.20   # below this is near-constant (no scaling worth modeling)
ALPHA_HI       <- 0.90   # above this approaches linear, already shown to overcorrect
ALPHA_FALLBACK <- 0.50   # used if log-log fit is numerically unreliable

# --- Mechanism B parameters ---
# Window of +-BW_B opp units around each test row. BW_B=5 gives an 11-unit window;
# sufficient for typical cal sizes (>=46 rows) while still local enough to track
# the low/mid/high opp gradient.
BW_B           <- 5L
MIN_NEIGHBORS_B <- 10L   # fall back to global quantile below this

# ===========================================================================
# HELPERS
# ===========================================================================

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |> mutate(
    draft_tier_int        = TIER_ORDER[draft_tier],
    is_cold_start_int     = as.integer(is_cold_start),
    def_used_fallback_int = as.integer(def_used_fallback)
  )
}

make_lgbm_matrix <- function(df, features) {
  df |> select(all_of(features)) |> as.matrix()
}

fit_lgbm <- function(X, y) {
  keep   <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = LGBM_PARAMS, data = dtrain, nrounds = N_ROUNDS, verbose = -1L)
}

conformal_q <- function(abs_resid, alpha) {
  n    <- length(abs_resid)
  prob <- (1 + 1 / n) * alpha
  if (prob >= 1.0) return(Inf)
  quantile(abs_resid, prob, names = FALSE)
}

# Mechanism A: log-log OLS gives the exponent of raw_resid ~ c * opp^alpha.
# Clamped to [ALPHA_LO, ALPHA_HI]; fallback on numerical failure.
fit_power_alpha <- function(opp, raw_resid) {
  df  <- data.frame(log_opp = log(opp), log_resid = log(raw_resid + 1e-8))
  fit <- tryCatch(lm(log_resid ~ log_opp, data = df), error = function(e) NULL)
  if (is.null(fit)) return(ALPHA_FALLBACK)
  alpha <- unname(coef(fit)["log_opp"])
  if (!is.finite(alpha)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, alpha))
}

# Mechanism B: local conformal quantile for one test opp value.
# Uses cal rows within +-BW_B of test_opp_val; falls back to global when sparse.
local_conf_q <- function(cal_opp, cal_resid, test_opp_val, alpha) {
  idx <- abs(cal_opp - test_opp_val) <= BW_B
  if (sum(idx) < MIN_NEIGHBORS_B) idx <- rep(TRUE, length(cal_opp))
  conformal_q(cal_resid[idx], alpha)
}

# Build intervals from a per-row half-width VECTOR (unlike build_intervals which takes a scalar)
build_row_intervals <- function(pred, hw50, hw80, hw90, suffix) {
  out <- tibble(
    p    = pred,
    lo50 = pred - hw50, hi50 = pred + hw50,
    lo80 = pred - hw80, hi80 = pred + hw80,
    lo90 = pred - hw90, hi90 = pred + hw90
  )
  names(out) <- c(
    paste0("pred_",   suffix),
    paste0("lo_50_",  suffix), paste0("hi_50_", suffix),
    paste0("lo_80_",  suffix), paste0("hi_80_", suffix),
    paste0("lo_90_",  suffix), paste0("hi_90_", suffix)
  )
  out
}

# Scalar half-width version (efficiency/volume -- unchanged from prior 3A runs)
build_intervals <- function(pred, qs, suffix) {
  out <- tibble(
    p    = pred,
    lo50 = pred - qs[1], hi50 = pred + qs[1],
    lo80 = pred - qs[2], hi80 = pred + qs[2],
    lo90 = pred - qs[3], hi90 = pred + qs[3]
  )
  names(out) <- c(
    paste0("pred_",   suffix),
    paste0("lo_50_",  suffix), paste0("hi_50_", suffix),
    paste0("lo_80_",  suffix), paste0("hi_80_", suffix),
    paste0("lo_90_",  suffix), paste0("hi_90_", suffix)
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
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 3A: Combined-Interval Construction Test")
cli_alert_info("A: power-law (log-log OLS exponent per fold)")
cli_alert_info("B: locally-weighted conformal (bw = +-{BW_B} opp, min_n = {MIN_NEIGHBORS_B})")
cli_alert_info("Judge: flatness high/low ratio on 1,682-row test set -- coverage reported for winner only")

ft       <- readRDS("data/rb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

cli_alert_success("Feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds")
if (nrow(fold_map) != 31L) cli_abort("Expected 31 folds, got {nrow(fold_map)}")

ft <- encode_features(ft)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop")

fold_results <- vector("list", nrow(fold_map))
alpha_log    <- numeric(nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {

  test_season <- fold_map$test_season[f]
  test_week   <- fold_map$test_week[f]

  # Exact split logic from rubric Step 2d -- no variation permitted
  test_data  <- ft |> filter(season == test_season, week == test_week)
  train_data <- ft |> filter(
    season < test_season |
    (season == test_season & week < test_week)
  )

  overlap <- intersect(
    paste(train_data$season, train_data$week),
    paste(test_data$season,  test_data$week)
  )
  if (length(overlap) > 0L) cli_abort("Fold {f}: train/test overlap: {overlap}")

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)

  if (any(cal_sws$season == test_season & cal_sws$week == test_week)) {
    cli_abort("Fold {f}: test season-week in calibration set")
  }

  fit_data <- train_data |> semi_join(head(train_sws, nrow(train_sws) - n_cal_sw), by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # --- Efficiency model (frozen -- identical to 03a_lgbm_control.R) ---
  X_fit_eff  <- make_lgbm_matrix(fit_data,  EFF_FEATURES)
  X_cal_eff  <- make_lgbm_matrix(cal_data,  EFF_FEATURES)
  X_test_eff <- make_lgbm_matrix(test_data, EFF_FEATURES)

  mod_eff       <- fit_lgbm(X_fit_eff, fit_data$epa_per_opp_obs)
  pred_cal_eff  <- predict(mod_eff, X_cal_eff)
  pred_test_eff <- predict(mod_eff, X_test_eff)

  resid_eff <- abs(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_eff    <- c(conformal_q(resid_eff, 0.50),
                 conformal_q(resid_eff, 0.80),
                 conformal_q(resid_eff, 0.90))

  # --- Volume model (frozen) ---
  X_fit_vol  <- make_lgbm_matrix(fit_data,  VOL_FEATURES)
  X_cal_vol  <- make_lgbm_matrix(cal_data,  VOL_FEATURES)
  X_test_vol <- make_lgbm_matrix(test_data, VOL_FEATURES)

  mod_vol       <- fit_lgbm(X_fit_vol, as.numeric(fit_data$opportunities))
  pred_cal_vol  <- predict(mod_vol, X_cal_vol)
  pred_test_vol <- predict(mod_vol, X_test_vol)

  resid_vol <- abs(as.numeric(cal_data$opportunities) - pred_cal_vol)
  qs_vol    <- c(conformal_q(resid_vol, 0.50),
                 conformal_q(resid_vol, 0.80),
                 conformal_q(resid_vol, 0.90))

  # --- Combined point prediction (same for both mechanisms) ---
  pred_cal_tot  <- pred_cal_eff * pred_cal_vol
  pred_test_tot <- pred_test_eff * pred_test_vol
  raw_resid_cal <- abs(cal_data$total_epa - pred_cal_tot)

  cal_opp  <- as.numeric(cal_data$opportunities)
  test_opp <- as.numeric(test_data$opportunities)

  # --- Mechanism A: power-law normalization ---
  # Fit alpha so that raw_resid / opp^alpha is homoscedastic on the cal set.
  alpha        <- fit_power_alpha(cal_opp, raw_resid_cal)
  alpha_log[f] <- alpha

  resid_norm_A  <- raw_resid_cal / cal_opp^alpha
  q_norm_A      <- c(conformal_q(resid_norm_A, 0.50),
                     conformal_q(resid_norm_A, 0.80),
                     conformal_q(resid_norm_A, 0.90))

  hw_A_50 <- q_norm_A[1] * test_opp^alpha
  hw_A_80 <- q_norm_A[2] * test_opp^alpha
  hw_A_90 <- q_norm_A[3] * test_opp^alpha

  # --- Mechanism B: locally-weighted conformal ---
  # Each test row gets the quantile from cal rows within +-BW_B of its opp count.
  # No parametric assumption; adapts to the actual residual distribution at each opp level.
  hw_B_50 <- vapply(test_opp,
                    function(k) local_conf_q(cal_opp, raw_resid_cal, k, 0.50),
                    numeric(1))
  hw_B_80 <- vapply(test_opp,
                    function(k) local_conf_q(cal_opp, raw_resid_cal, k, 0.80),
                    numeric(1))
  hw_B_90 <- vapply(test_opp,
                    function(k) local_conf_q(cal_opp, raw_resid_cal, k, 0.90),
                    numeric(1))

  fold_results[[f]] <- test_data |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(
      build_intervals(pred_test_eff, qs_eff, "eff"),
      build_intervals(pred_test_vol, qs_vol, "vol"),
      build_row_intervals(pred_test_tot, hw_A_50, hw_A_80, hw_A_90, "tot_A"),
      build_row_intervals(pred_test_tot, hw_B_50, hw_B_80, hw_B_90, "tot_B")
    ) |>
    mutate(
      fold          = f,
      alpha_fold    = alpha,
      raw_resid_tot = abs(total_epa - pred_test_tot),
      hw_A_80       = hw_A_80,
      hw_B_80       = hw_B_80
    )

  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | alpha_A={round(alpha,3)}"
  )
}

results <- bind_rows(fold_results)

# ===========================================================================
# HARNESS INTEGRITY REPORT
# ===========================================================================

cli_h1("Harness Integrity Report")

n_folds_ran <- n_distinct(results$fold)
if (n_folds_ran == nrow(fold_map)) {
  cli_alert_success("All {nrow(fold_map)} folds completed -- no silently skipped folds")
} else {
  cli_abort("{n_folds_ran} of {nrow(fold_map)} folds produced results")
}

n_scored <- nrow(results)
if (n_scored == EXPECTED_TEST_N) {
  cli_alert_success("Row count: {n_scored} / {EXPECTED_TEST_N}")
} else {
  cli_warn("Row count mismatch: {n_scored} scored, {EXPECTED_TEST_N} expected")
}

na_tot <- sum(is.na(results$pred_tot_A)) + sum(is.na(results$pred_tot_B))
if (na_tot == 0L) {
  cli_alert_success("Zero NA predictions")
} else {
  cli_warn("{na_tot} NA predictions detected")
}

cli_alert_success("Conformal calibration: training-side only (verified per fold)")

cli_alert_info("Mechanism A -- fitted alpha across {nrow(fold_map)} folds:")
cli_alert_info(
  "  range [{round(min(alpha_log),3)}, {round(max(alpha_log),3)}] | median {round(median(alpha_log),3)} | mean {round(mean(alpha_log),3)}"
)

# ===========================================================================
# FLATNESS CHECK -- THE JUDGE
# ===========================================================================

cli_h1("Flatness Check (judge -- test set, n=1682)")
cli_alert_info("Normalized residual = raw_resid / mechanism_80pct_half_width for that row")
cli_alert_info("Flat = mean normalized residual constant across opp buckets")
cli_alert_info("Pass criterion: high/low ratio in [0.67, 1.50]")

flat_data <- results |>
  mutate(
    norm_A = raw_resid_tot / hw_A_80,
    norm_B = raw_resid_tot / hw_B_80,
    opp_bucket = case_when(
      opportunities <= 8L  ~ "low  (5-8)",
      opportunities <= 13L ~ "mid  (9-13)",
      TRUE                 ~ "high (14+)"
    ) |> factor(levels = c("low  (5-8)", "mid  (9-13)", "high (14+)"))
  )

make_flatness_table <- function(df, norm_col) {
  df |>
    group_by(opp_bucket) |>
    summarise(
      n               = n(),
      mean_raw_resid  = round(mean(raw_resid_tot), 3),
      mean_norm_resid = round(mean(.data[[norm_col]]), 3),
      sd_norm_resid   = round(sd(.data[[norm_col]]),   3),
      .groups = "drop"
    )
}

flatness_A <- make_flatness_table(flat_data, "norm_A")
flatness_B <- make_flatness_table(flat_data, "norm_B")

ratio <- function(ft) {
  ft$mean_norm_resid[ft$opp_bucket == "high (14+)"] /
  ft$mean_norm_resid[ft$opp_bucket == "low  (5-8)"]
}
ratio_A <- ratio(flatness_A)
ratio_B <- ratio(flatness_B)

cli_h2("Mechanism A -- power-law (per-fold fitted alpha)")
print(flatness_A, n = Inf)
cli_alert_info("A high/low ratio: {round(ratio_A, 3)}")

cli_h2("Mechanism B -- locally-weighted conformal (bw=+-{BW_B})")
print(flatness_B, n = Inf)
cli_alert_info("B high/low ratio: {round(ratio_B, 3)}")

A_passes <- ratio_A >= 0.67 & ratio_A <= 1.50
B_passes <- ratio_B >= 0.67 & ratio_B <= 1.50

if (A_passes)  cli_alert_success("A: FLAT") else cli_warn("A: NOT FLAT")
if (B_passes)  cli_alert_success("B: FLAT") else cli_warn("B: NOT FLAT")

# ===========================================================================
# WINNER SELECTION
# ===========================================================================

cli_h1("Winner Selection")

if (!A_passes && !B_passes) {
  cli_abort(
    "STOP: neither mechanism flattened the heteroscedasticity. Continuous-width is insufficient. Reconsider stratified/group-conditional construction before proceeding to 3B."
  )
}

if (A_passes && !B_passes) {
  winner <- "A"
} else if (B_passes && !A_passes) {
  winner <- "B"
} else {
  # Both pass: prefer A (simpler parametric form, easier to describe to 3B)
  # unless B is materially closer to 1.0 (>0.05 improvement in absolute distance)
  dist_A <- abs(ratio_A - 1.0)
  dist_B <- abs(ratio_B - 1.0)
  winner <- if (dist_B < dist_A - 0.05) "B" else "A"
}

cli_alert_success(
  "Winner: Mechanism {winner} | ratio_A={round(ratio_A,3)}, ratio_B={round(ratio_B,3)}"
)

winner_suffix <- paste0("tot_", winner)

# ===========================================================================
# COVERAGE REPORT -- WINNER ONLY
# ===========================================================================

cli_h1("Coverage: Mechanism {winner} (pooled + low-usage)")

score_component <- function(y, df, suffix, label) {
  eval_calibration(y, pi_cols(df, suffix)) |>
    mutate(component = label, stratum = "pooled", .before = 1)
}

pooled <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff",          "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol",          "volume"),
  score_component(results$total_epa,                 results, winner_suffix,  "combined")
)

print(pooled |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

res_lo    <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low     <- nrow(res_lo)
cli_alert_info("Low-usage bucket: {n_low} rows")

low_usage <- bind_rows(
  score_component(res_lo$epa_per_opp_obs,           res_lo, "eff",         "efficiency"),
  score_component(as.numeric(res_lo$opportunities), res_lo, "vol",         "volume"),
  score_component(res_lo$total_epa,                 res_lo, winner_suffix, "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))

print(low_usage |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# Decision rule
cli_h1("Decision Rule Evaluation")

comb_pooled_80 <- pooled    |> filter(component == "combined", nominal == 0.80)
comb_low_80    <- low_usage |> filter(component == "combined", nominal == 0.80)

delta_pooled <- comb_pooled_80$delta
delta_low    <- comb_low_80$delta

cli_alert_info(
  "Pooled combined 80%: empirical={round(comb_pooled_80$empirical,3)} delta={sprintf('%+.3f', delta_pooled)}"
)
cli_alert_info(
  "Low-usage combined 80%: empirical={round(comb_low_80$empirical,3)} delta={sprintf('%+.3f', delta_low)}"
)

if (abs(delta_low) > 0.10) {
  cli_warn(
    "VETO TRIGGERED: low-usage 80% delta = {sprintf('%+.1f', delta_low*100)}pp"
  )
} else {
  cli_alert_success(
    "Veto check passed: low-usage 80% delta = {sprintf('%+.1f', delta_low*100)}pp"
  )
}

cli_alert_info("Sharpness (combined, pooled):")
pooled |>
  filter(component == "combined") |>
  mutate(lbl = paste0(as.integer(nominal * 100), "%: ", round(sharpness, 3))) |>
  pull(lbl) |>
  paste(collapse = " | ") |>
  cli_alert_info()

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# Flatness comparison (both mechanisms)
flatness_out <- bind_rows(
  flatness_A |> mutate(mechanism = "A_power_law",       high_low_ratio = round(ratio_A, 3)),
  flatness_B |> mutate(mechanism = "B_local_conformal", high_low_ratio = round(ratio_B, 3))
)
readr::write_csv(flatness_out, "output/03a_construction_flatness.csv")

# Winner results -- rename winner columns to canonical names (tot, not tot_A/tot_B)
# so downstream scripts (3B comparison) use a consistent column schema.
winner_cols <- results |>
  select(matches(paste0("_tot_", winner, "$"))) |>
  rename_with(~ sub(paste0("_", winner, "$"), "", .x))

results_final <- results |>
  select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa, fold, alpha_fold,
         pred_eff, lo_50_eff, hi_50_eff, lo_80_eff, hi_80_eff, lo_90_eff, hi_90_eff,
         pred_vol, lo_50_vol, hi_50_vol, lo_80_vol, hi_80_vol, lo_90_vol, hi_90_vol) |>
  bind_cols(winner_cols)

readr::write_csv(results_final, "output/03a_lgbm_fold_predictions.csv")
readr::write_csv(pooled,        "output/03a_lgbm_pooled_coverage.csv")
readr::write_csv(low_usage,     "output/03a_lgbm_low_usage_coverage.csv")

cli_alert_success("output/03a_construction_flatness.csv  (both mechanisms)")
cli_alert_success("output/03a_lgbm_fold_predictions.csv  (winner: {winner})")
cli_alert_success("output/03a_lgbm_pooled_coverage.csv")
cli_alert_success("output/03a_lgbm_low_usage_coverage.csv")
cli_alert_info("Construction locked: Mechanism {winner} is 3A-baseline and 3B standard")

cli_h1("Step 3A interval construction complete")
