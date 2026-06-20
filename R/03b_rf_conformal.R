# R/03b_rf_conformal.R
# Step 3B: Random Forest efficiency + conformal (learner comparison vs 3A LightGBM).
#
# THE ONLY MOVING PART: efficiency point predictor is Random Forest (ranger) instead
# of LightGBM. Volume model stays as LightGBM -- it was calibrated in 3A and swapping
# it would confound the learner comparison with a volume-model change.
#
# WHY Random Forest over LightGBM for efficiency:
#   Boosting pursues the conditional mean via gradient steps. For a committee back with
#   5-7 touches, the training signal is sparse and the gradient path averages over
#   boom/bust outcomes -- the result is a confident near-mean prediction even when the
#   true distribution is heavy-tailed. Random Forest bags diverse trees, each of which
#   overfits a different subset of fringe-starter outcomes; the ensemble residuals on
#   the calibration set are wider for high-variance low-opp players, which feeds the
#   conformal quantile a more honest tail estimate.
#
# CONSTRUCTION FROZEN: power-law Mechanism A, per-fold alpha, identical to 3A.
#   1. pred_cal_tot = pred_cal_eff * pred_cal_vol
#   2. log-log OLS: log(raw_resid) ~ log(opp) on cal set -> alpha, clamped [0.20, 0.90]
#   3. resid_norm = raw_resid / opp^alpha
#   4. conformal quantile on resid_norm
#   5. at test time: half_width_i = q_norm * opp_i^alpha
#
# DO NOT modify frozen inputs. DO NOT tune toward passing the veto.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(ranger)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS -- pre-committed; shared constants match 3A exactly
# ===========================================================================

CAL_FRAC <- 0.20

# Volume model -- frozen LightGBM (identical to 3A so volume side is identical)
LGBM_PARAMS_VOL <- list(
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
N_ROUNDS_VOL <- 200L

# Efficiency model -- Random Forest (ranger)
# mtry = floor(sqrt(p)) is the standard regression default; gives each split a
# subset of features so trees differ meaningfully rather than all picking the
# same dominant signal (prior_epa_per_opp).
# min.node.size = 10: allows finer splits than LightGBM's min_data_in_leaf=20,
# letting early folds with small training sets still split at the tails.
# num.trees = 500: stable variance at this n; beyond this gives diminishing returns.
RF_NUM_TREES    <- 500L
RF_MIN_NODE     <- 10L
RF_SEED         <- 42L

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

# Mechanism A power-law constants (same as 03a_interval_construction.R)
ALPHA_LO       <- 0.20
ALPHA_HI       <- 0.90
ALPHA_FALLBACK <- 0.50

LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

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

make_lgbm_matrix <- function(df, features) {
  df |> select(all_of(features)) |> as.matrix()
}

fit_lgbm_vol <- function(X, y) {
  keep   <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = LGBM_PARAMS_VOL, data = dtrain, nrounds = N_ROUNDS_VOL, verbose = -1L)
}

# RF efficiency model: predicts conditional mean of epa_per_opp_obs.
# Returns a fitted ranger object; predict() with type="response" gives means.
fit_rf_eff <- function(df, features) {
  p <- sum(!is.na(df$epa_per_opp_obs))
  ranger::ranger(
    formula       = epa_per_opp_obs ~ .,
    data          = df |> select(all_of(c(features, "epa_per_opp_obs"))) |> drop_na(),
    num.trees     = RF_NUM_TREES,
    mtry          = max(1L, floor(sqrt(length(features)))),
    min.node.size = RF_MIN_NODE,
    seed          = RF_SEED,
    num.threads   = 1L,
    verbose       = FALSE
  )
}

conformal_q <- function(abs_resid, alpha) {
  n    <- length(abs_resid)
  prob <- (1 + 1 / n) * alpha
  if (prob >= 1.0) return(Inf)
  quantile(abs_resid, prob, names = FALSE)
}

# Mechanism A: log-log OLS to estimate the heteroscedasticity exponent.
fit_power_alpha <- function(opp, raw_resid) {
  df  <- data.frame(log_opp = log(opp), log_resid = log(raw_resid + 1e-8))
  fit <- tryCatch(lm(log_resid ~ log_opp, data = df), error = function(e) NULL)
  if (is.null(fit)) return(ALPHA_FALLBACK)
  alpha <- unname(coef(fit)["log_opp"])
  if (!is.finite(alpha)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, alpha))
}

# Scalar conformal interval (efficiency, volume)
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

# Per-row half-width (combined power-law)
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

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 3B: Random Forest + Conformal (learner comparison)")
cli_alert_info("Efficiency: Random Forest (ranger, {RF_NUM_TREES} trees, mtry=sqrt(p)={max(1L, floor(sqrt(length(EFF_FEATURES))))})")
cli_alert_info("Volume: LightGBM (frozen -- identical to 3A)")
cli_alert_info("Combined: Mechanism A power-law conformal (frozen construction)")

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

fold_results <- vector("list", nrow(fold_map))
alpha_log    <- numeric(nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {

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
  if (length(overlap) > 0L) cli_abort("Fold {f}: train/test season-week overlap: {overlap}")

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)

  if (any(cal_sws$season == test_season & cal_sws$week == test_week)) {
    cli_abort("Fold {f}: test season-week leaked into conformal calibration set")
  }

  fit_sws  <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # -- Efficiency model: Random Forest --
  # Fit on rows used for model fitting (not calibration rows), same split as 3A.
  mod_eff       <- fit_rf_eff(fit_data, EFF_FEATURES)
  pred_cal_eff  <- predict(mod_eff, data = cal_data  |> select(all_of(EFF_FEATURES)))$predictions
  pred_test_eff <- predict(mod_eff, data = test_data |> select(all_of(EFF_FEATURES)))$predictions

  resid_eff <- abs(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_eff    <- c(
    conformal_q(resid_eff, 0.50),
    conformal_q(resid_eff, 0.80),
    conformal_q(resid_eff, 0.90)
  )

  # -- Volume model: LightGBM (frozen, identical to 3A) --
  X_fit_vol  <- make_lgbm_matrix(fit_data,  VOL_FEATURES)
  X_cal_vol  <- make_lgbm_matrix(cal_data,  VOL_FEATURES)
  X_test_vol <- make_lgbm_matrix(test_data, VOL_FEATURES)

  mod_vol       <- fit_lgbm_vol(X_fit_vol, as.numeric(fit_data$opportunities))
  pred_cal_vol  <- predict(mod_vol, X_cal_vol)
  pred_test_vol <- predict(mod_vol, X_test_vol)

  resid_vol <- abs(as.numeric(cal_data$opportunities) - pred_cal_vol)
  qs_vol    <- c(
    conformal_q(resid_vol, 0.50),
    conformal_q(resid_vol, 0.80),
    conformal_q(resid_vol, 0.90)
  )

  # -- Combined: Mechanism A power-law (FROZEN CONSTRUCTION) --
  # Identical steps as 03a_interval_construction.R winner path.
  pred_cal_tot  <- pred_cal_eff  * pred_cal_vol
  pred_test_tot <- pred_test_eff * pred_test_vol

  raw_resid_cal <- abs(cal_data$total_epa - pred_cal_tot)
  cal_opp       <- as.numeric(cal_data$opportunities)
  test_opp      <- as.numeric(test_data$opportunities)

  alpha        <- fit_power_alpha(cal_opp, raw_resid_cal)
  alpha_log[f] <- alpha

  resid_norm <- raw_resid_cal / cal_opp^alpha
  q_norm     <- c(
    conformal_q(resid_norm, 0.50),
    conformal_q(resid_norm, 0.80),
    conformal_q(resid_norm, 0.90)
  )

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

  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | alpha={round(alpha, 3)} | cal_sw={n_cal_sw} ({nrow(cal_data)} rows)"
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
cli_alert_info("Test rows scored: {n_scored} | Expected: {EXPECTED_TEST_N}")
if (n_scored == EXPECTED_TEST_N) {
  cli_alert_success("Row count matches expected {EXPECTED_TEST_N}")
} else {
  cli_warn("Row count mismatch: scored {n_scored}, expected {EXPECTED_TEST_N}")
}

na_eff <- sum(is.na(results$pred_eff))
na_vol <- sum(is.na(results$pred_vol))
na_tot <- sum(is.na(results$pred_tot))
if (na_eff + na_vol + na_tot == 0L) {
  cli_alert_success("Zero NA predictions across all folds and components")
} else {
  cli_warn("NA predictions: efficiency={na_eff}, volume={na_vol}, combined={na_tot}")
}

cli_alert_success("Conformal calibration: training-side only (loop assertion verified each fold)")

cli_alert_info(
  "alpha_fold range [{round(min(alpha_log), 3)}, {round(max(alpha_log), 3)}] | median {round(median(alpha_log), 3)}"
)

# ===========================================================================
# COVERAGE SCORING -- 3B
# ===========================================================================

cli_h1("3B Pooled Coverage (all {n_scored} test rows)")

pooled_3b <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_3b |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("3B Low-Usage Bucket (opp {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo_3b <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low     <- nrow(res_lo_3b)
cli_alert_info("Low-usage rows: {n_low}")

low_3b <- bind_rows(
  score_component(res_lo_3b$epa_per_opp_obs,           res_lo_3b, "eff", "efficiency"),
  score_component(as.numeric(res_lo_3b$opportunities), res_lo_3b, "vol", "volume"),
  score_component(res_lo_3b$total_epa,                 res_lo_3b, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))

print(low_3b |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# ===========================================================================
# LEARNER COMPARISON: 3B vs 3A
# ===========================================================================

cli_h1("Learner Comparison: 3B (RF) vs 3A (LightGBM)")

# Load 3A frozen outputs
pooled_3a   <- readr::read_csv("output/03a_lgbm_pooled_coverage.csv",    show_col_types = FALSE)
low_3a      <- readr::read_csv("output/03a_lgbm_low_usage_coverage.csv", show_col_types = FALSE)
preds_3a    <- readr::read_csv("output/03a_lgbm_fold_predictions.csv",   show_col_types = FALSE)

# Pre-compute 3A low-usage sharpness (not stored in pooled CSV -- derive from preds)
res_lo_3a  <- preds_3a |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)

low_3a_sharp <- bind_rows(
  score_component(res_lo_3a$epa_per_opp_obs,           res_lo_3a, "eff", "efficiency"),
  score_component(as.numeric(res_lo_3a$opportunities), res_lo_3a, "vol", "volume"),
  score_component(res_lo_3a$total_epa,                 res_lo_3a, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))

fmt_pp  <- function(x) sprintf("%+.1fpp", x * 100)
fmt_pct <- function(x) sprintf("%.1f%%",  x * 100)
fmt_w   <- function(x) round(x, 3)

# --- PRIMARY DELIVERABLE 1: efficiency coverage in low-usage bucket ---
cli_h2("1. Low-Usage Efficiency Coverage (the -8.7pp target)")

eff_comp <- function(low_df, tag) {
  low_df |>
    filter(component == "efficiency") |>
    mutate(
      model = tag,
      delta_pp = fmt_pp(delta),
      width    = fmt_w(sharpness)
    ) |>
    select(model, nominal, delta_pp, width)
}

eff_lo_3a <- eff_comp(low_3a_sharp, "3A-LightGBM")
eff_lo_3b <- eff_comp(low_3b,       "3B-RF       ")

cli_alert_info("model          | nominal | coverage_delta | width")
bind_rows(eff_lo_3a, eff_lo_3b) |>
  arrange(nominal, model) |>
  mutate(row = paste0(model, " | ", as.integer(nominal * 100), "% | ", delta_pp, " | ", width)) |>
  pull(row) |>
  walk(cli_alert_info)

# --- PRIMARY DELIVERABLE 2: combined coverage, pooled and low-usage ---
cli_h2("2. Combined Coverage -- Pooled and Low-Usage")

comb_comp <- function(pooled_df, low_df, tag) {
  bind_rows(
    pooled_df |> filter(component == "combined") |> mutate(stratum = "pooled"),
    low_df    |> filter(component == "combined") |> mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
  ) |>
    mutate(model = tag, delta_pp = fmt_pp(delta), width = fmt_w(sharpness)) |>
    select(model, stratum, nominal, delta_pp, width)
}

comb_3a <- comb_comp(pooled_3a, low_3a_sharp, "3A-LightGBM")
comb_3b <- comb_comp(pooled_3b, low_3b,       "3B-RF       ")

cli_alert_info("model          | stratum       | nominal | delta   | width")
bind_rows(comb_3a, comb_3b) |>
  arrange(stratum, nominal, model) |>
  mutate(row = paste0(model, " | ", str_pad(stratum, 14), " | ",
                      as.integer(nominal * 100), "% | ", delta_pp, " | ", width)) |>
  pull(row) |>
  walk(cli_alert_info)

# --- PRIMARY DELIVERABLE 3: volume coverage, low-usage -- should be ~identical ---
cli_h2("3. Low-Usage Volume Coverage (should match 3A -- same model)")

vol_comp <- function(low_df, tag) {
  low_df |>
    filter(component == "volume") |>
    mutate(model = tag, delta_pp = fmt_pp(delta), width = fmt_w(sharpness)) |>
    select(model, nominal, delta_pp, width)
}

vol_lo_3a <- vol_comp(low_3a_sharp, "3A-LightGBM")
vol_lo_3b <- vol_comp(low_3b,       "3B-RF       ")

cli_alert_info("model          | nominal | coverage_delta | width")
bind_rows(vol_lo_3a, vol_lo_3b) |>
  arrange(nominal, model) |>
  mutate(row = paste0(model, " | ", as.integer(nominal * 100), "% | ", delta_pp, " | ", width)) |>
  pull(row) |>
  walk(cli_alert_info)

vol_delta_3a <- low_3a_sharp |> filter(component == "volume", nominal == 0.80) |> pull(delta)
vol_delta_3b <- low_3b       |> filter(component == "volume", nominal == 0.80) |> pull(delta)
vol_shift    <- abs(vol_delta_3b - vol_delta_3a)
if (vol_shift > 0.05) {
  cli_warn(
    "Volume low-usage 80% shifted by {fmt_pp(vol_shift)}: volume models are DIVERGING -- investigate before continuing"
  )
} else {
  cli_alert_success(
    "Volume low-usage 80% shift: {fmt_pp(vol_shift)} (within 5pp -- same model confirmed)"
  )
}

# --- PRIMARY DELIVERABLE 4: efficiency-vs-combined direction check ---
cli_h2("4. Pre-Registered Direction Check (efficiency and combined should move together in 5-8 bucket)")

eff_80_3a  <- low_3a_sharp |> filter(component == "efficiency", nominal == 0.80) |> pull(delta)
comb_80_3a <- low_3a_sharp |> filter(component == "combined",   nominal == 0.80) |> pull(delta)
eff_80_3b  <- low_3b       |> filter(component == "efficiency", nominal == 0.80) |> pull(delta)
comb_80_3b <- low_3b       |> filter(component == "combined",   nominal == 0.80) |> pull(delta)

d_eff  <- eff_80_3b  - eff_80_3a    # positive = 3B reduced undercoverage
d_comb <- comb_80_3b - comb_80_3a   # positive = 3B improved combined delta

cli_alert_info("3A: efficiency 80% delta = {fmt_pp(eff_80_3a)} | combined 80% delta = {fmt_pp(comb_80_3a)}")
cli_alert_info("3B: efficiency 80% delta = {fmt_pp(eff_80_3b)} | combined 80% delta = {fmt_pp(comb_80_3b)}")
cli_alert_info("Shift: efficiency {fmt_pp(d_eff)} | combined {fmt_pp(d_comb)}")

if (sign(d_eff) == sign(d_comb) || abs(d_eff) < 0.005) {
  cli_alert_success("Direction consistent: efficiency and combined move together in 5-8 bucket")
} else {
  cli_warn(
    "DIVERGENCE FINDING: efficiency shifted {fmt_pp(d_eff)} but combined shifted {fmt_pp(d_comb)} (opposite direction)"
  )
  cli_alert_info(
    "Pre-registered interpretation: volume multiplier is damping the efficiency signal in this stratum. Surface as a finding -- do not re-tune."
  )
}

# ===========================================================================
# DECISION RULE
# ===========================================================================

cli_h1("Decision Rule Evaluation")
cli_alert_info("PRIMARY: pooled combined 80% closest to nominal")
cli_alert_info("TIEBREAK: sharpest among models within +-2pp of nominal")
cli_alert_info("VETO: low-usage combined 80% delta > +-10pp disqualifies")

comb_pool_80_3b <- pooled_3b |> filter(component == "combined", nominal == 0.80)
comb_low_80_3b  <- low_3b    |> filter(component == "combined", nominal == 0.80)
comb_pool_80_3a <- pooled_3a |> filter(component == "combined", nominal == 0.80)
comb_low_80_3a  <- low_3a    |> filter(component == "combined", nominal == 0.80)

delta_pool_3b <- comb_pool_80_3b$delta
delta_low_3b  <- comb_low_80_3b$delta
delta_pool_3a <- comb_pool_80_3a$delta
delta_low_3a  <- comb_low_80_3a$delta

cli_alert_info("3A pooled combined 80%: delta={fmt_pp(delta_pool_3a)} | 3B: delta={fmt_pp(delta_pool_3b)}")
cli_alert_info("3A low combined 80%:    delta={fmt_pp(delta_low_3a)} | 3B: delta={fmt_pp(delta_low_3b)}")

if (abs(delta_low_3b) > 0.10) {
  cli_warn(
    "3B VETO TRIGGERED: low-usage 80% delta = {fmt_pp(delta_low_3b)} (threshold +-10pp)"
  )
} else {
  cli_alert_success(
    "3B veto check passed: low-usage 80% delta = {fmt_pp(delta_low_3b)}"
  )
}

sharp_pool_3b <- pooled_3b |> filter(component == "combined", nominal == 0.80) |> pull(sharpness)
sharp_pool_3a <- pooled_3a |> filter(component == "combined", nominal == 0.80) |> pull(sharpness)

cli_alert_info("Sharpness (pooled combined 80%): 3A={round(sharp_pool_3a, 3)} | 3B={round(sharp_pool_3b, 3)}")

within_2pp_3a <- abs(delta_pool_3a) <= 0.02
within_2pp_3b <- abs(delta_pool_3b) <= 0.02

cli_h2("Scoring Summary")
if (!within_2pp_3a && !within_2pp_3b) {
  if (abs(delta_pool_3b) < abs(delta_pool_3a)) {
    cli_alert_success("3B closer to nominal pooled combined 80%: 3B leads on primary")
  } else {
    cli_alert_info("3A closer to nominal pooled combined 80%: 3A leads on primary")
  }
} else if (within_2pp_3a && within_2pp_3b) {
  if (sharp_pool_3b < sharp_pool_3a) {
    cli_alert_success("Both within +-2pp; 3B sharper: 3B wins tiebreak")
  } else {
    cli_alert_info("Both within +-2pp; 3A sharper: 3A wins tiebreak")
  }
} else if (within_2pp_3b && !within_2pp_3a) {
  cli_alert_success("3B within +-2pp of nominal; 3A is not: 3B leads on primary")
} else {
  cli_alert_info("3A within +-2pp of nominal; 3B is not: 3A leads on primary")
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,   "output/03b_rf_fold_predictions.csv")
readr::write_csv(pooled_3b, "output/03b_rf_pooled_coverage.csv")
readr::write_csv(low_3b,    "output/03b_rf_low_usage_coverage.csv")

cli_alert_success("output/03b_rf_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/03b_rf_pooled_coverage.csv")
cli_alert_success("output/03b_rf_low_usage_coverage.csv")

cli_h1("Step 3B complete")
