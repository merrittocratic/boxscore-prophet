# R/12c_te_asymmetric_conformal_volfix.R
# Step 12c VOLFIX CLONE: TE asymmetric-conformal volume retrain with the
# baseline_* prior-season carryforward features (90df36a) added to the
# volume model only.
#
# CLONE DISCIPLINE: this file is 12c_te_asymmetric_conformal.R verbatim
# except for (1) this header, (2) VOL_FEATURES gains
# baseline_target_share, baseline_air_yards_share, baseline_snap_share,
# baseline_tgt_per_snap, baseline_team_total_plays (the TE set has one more
# member than WR's -- baseline_tgt_per_snap -- mirroring TE's extra
# wt_tgt_per_snap role feature), and (3) output paths get a _volfix suffix
# so the frozen 12c outputs are never overwritten. EFF_FEATURES, the fold
# map, the fit/cal split logic, the reused 12b hyperparameters, and the
# asymmetric conformal construction are all untouched. This is the TE
# sibling of R/04c_wr_asymmetric_conformal_volfix.R (WR) and
# R/11c_rb_injury_volfix.R (RB) -- same three-position retrain set.
#
# WHY: te_feature_table.rds volume features were NA at every player's
# Week 1 pre-fix (rookie or veteran alike), same gap as WR/RB (90df36a
# confirmed TE inherited it). The 12c model currently deployed
# (TE_VOL_FEATURES in R/10a_deployment_models.R) was trained before that
# fix landed. This retrain answers whether restoring real Week 1 volume
# signal changes the model's raw score distribution for known high-usage
# TEs -- see the companion before/after check run after this script
# completes.
#
# --- Original 12c header below, unchanged ---
#
# Step 12c: TE intervals with ASYMMETRIC conformal construction.
# TE clone of 04c (VOL_FEATURES adds wt_tgt_per_snap, matching 12b).
#
# MOTIVATION: same structural argument as WR -- symmetric pred +/- q(|resid|)
# intervals cannot pass right skew to the FP layer. TE feasibility showed
# per-game FP skew 1.26 (higher than WR 1.13), so the asym construction is
# at least as necessary here.
#
# ONLY CHANGING FACTOR vs 12b: the conformal quantile step uses SIGNED
# residuals with separate upper and lower quantiles. Everything else frozen:
#   - Same feature table, fold map, fit/cal split logic (CAL_FRAC tail)
#   - Same per-fold tuned hyperparameters, read from 12b_te_lgbm_tune_log.csv
#     (no re-tuning -- the learner is identical, seed 42 reproduces the fits)
#   - Same Mechanism A power-law alpha, fitted on |resid| exactly as in 04b
#     (alpha is the variance-scaling law; only the quantile step changes)
#
# For coverage c, bounds are signed-residual quantiles at (1-c)/2 and (1+c)/2
# with the (1 + 1/n) split-conformal finite-sample correction on each tail.
# A med_* column (pred + signed 50% quantile) is emitted so the downstream
# simulation can use the true conditional median instead of assuming pred is
# the 50th percentile (with skewed residuals, mean prediction != median).
#
# ACCEPTANCE (stated before run): pooled combined 80% within +-2pp, low-usage
# veto passes, and hi - pred > pred - lo on average (right skew captured).

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS (identical to 12b except no tuning grid)
# ===========================================================================

CAL_FRAC   <- 0.20
LOW_OPP_LO <- 3L
LOW_OPP_HI <- 5L

REFIT_ROUNDS_MIN <- 10L

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

EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_air_yards_per_target",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

VOL_FEATURES <- c(
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_tgt_per_snap",
  "wt_team_total_plays",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far",
  # VOLFIX: prior-season carryforward (90df36a), fixes NA volume features at
  # Week 1 for every player (rookie or veteran) -- see script header.
  "baseline_target_share", "baseline_air_yards_share", "baseline_snap_share",
  "baseline_tgt_per_snap", "baseline_team_total_plays"
)

ALPHA_LO       <- 0.20
ALPHA_HI       <- 0.90
ALPHA_FALLBACK <- 0.50

COVERAGES <- c(0.50, 0.80, 0.90)

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

# Two-sided conformal quantile on SIGNED residuals.
# prob > 0.5 = upper tail, prob < 0.5 = lower tail; each tail gets the
# (1 + 1/n) finite-sample correction pushing it conservatively outward.
conformal_q_signed <- function(resid_signed, prob) {
  n <- length(resid_signed)
  if (prob >= 0.5) {
    p_adj <- (1 + 1 / n) * prob
    if (p_adj >= 1) return(Inf)
  } else {
    p_adj <- 1 - (1 + 1 / n) * (1 - prob)
    if (p_adj <= 0) return(-Inf)
  }
  quantile(resid_signed, p_adj, names = FALSE)
}

# Signed lower/median/upper quantiles for the three coverage levels.
signed_quantile_set <- function(resid_signed) {
  list(
    lo  = vapply(COVERAGES, function(c) conformal_q_signed(resid_signed, (1 - c) / 2), numeric(1)),
    hi  = vapply(COVERAGES, function(c) conformal_q_signed(resid_signed, (1 + c) / 2), numeric(1)),
    med = quantile(resid_signed, 0.50, names = FALSE)
  )
}

# alpha fit unchanged from 04b (uses |resid|; the scaling law is frozen)
fit_power_alpha <- function(opp, raw_resid) {
  df  <- data.frame(log_opp = log(opp), log_resid = log(raw_resid + 1e-8))
  fit <- tryCatch(lm(log_resid ~ log_opp, data = df), error = function(e) NULL)
  if (is.null(fit)) return(ALPHA_FALLBACK)
  alpha <- unname(coef(fit)["log_opp"])
  if (!is.finite(alpha)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, alpha))
}

# Asymmetric intervals: bound_i = pred_i + q * scale_i (q_lo negative for
# lower bounds). scale = 1 for components, opp^alpha for combined.
build_asym_intervals <- function(pred, qset, suffix, scale = 1) {
  out <- tibble(
    p    = pred,
    m    = pred + qset$med   * scale,
    lo50 = pred + qset$lo[1] * scale, hi50 = pred + qset$hi[1] * scale,
    lo80 = pred + qset$lo[2] * scale, hi80 = pred + qset$hi[2] * scale,
    lo90 = pred + qset$lo[3] * scale, hi90 = pred + qset$hi[3] * scale
  )
  names(out) <- c(
    paste0("pred_", suffix), paste0("med_", suffix),
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
# LOAD FROZEN INPUTS + 12B TUNED PARAMS
# ===========================================================================

cli_h1("Step 12c VOLFIX: TE Asymmetric Conformal (12b learner, signed-residual intervals)")

ft       <- readRDS("data/te_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")
tune_log <- readr::read_csv("output/12b_te_lgbm_tune_log.csv", show_col_types = FALSE)

if (nrow(tune_log) != nrow(fold_map)) {
  cli_abort("Tune log has {nrow(tune_log)} rows; fold map has {nrow(fold_map)}")
}

EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()

cli_alert_success("TE feature table: {nrow(ft)} rows | {nrow(fold_map)} folds | expected test rows: {EXPECTED_TEST_N}")
cli_alert_info("Hyperparameters: per-fold tuned values reused from 12b tune log (no re-tuning)")

ft <- encode_features(ft)

# ===========================================================================
# WALK-FORWARD LOOP (refit + asymmetric conformal only)
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds, refit-only)")

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

  fit_sws   <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data  <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data  <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  X_fit_eff  <- make_matrix(fit_data,  EFF_FEATURES)
  X_cal_eff  <- make_matrix(cal_data,  EFF_FEATURES)
  X_test_eff <- make_matrix(test_data, EFF_FEATURES)
  X_fit_vol  <- make_matrix(fit_data,  VOL_FEATURES)
  X_cal_vol  <- make_matrix(cal_data,  VOL_FEATURES)
  X_test_vol <- make_matrix(test_data, VOL_FEATURES)

  tl <- tune_log[f, ]

  mod_eff <- fit_lgbm_tuned(
    X_fit_eff, fit_data$epa_per_opp_obs,
    list(num_leaves = tl$eff_num_leaves, learning_rate = tl$eff_lr,
         min_data_in_leaf = tl$eff_min_node),
    max(REFIT_ROUNDS_MIN, tl$eff_rounds)
  )
  mod_vol <- fit_lgbm_tuned(
    X_fit_vol, as.numeric(fit_data$opportunities),
    list(num_leaves = tl$vol_num_leaves, learning_rate = tl$vol_lr,
         min_data_in_leaf = tl$vol_min_node),
    max(REFIT_ROUNDS_MIN, tl$vol_rounds)
  )

  pred_cal_eff  <- predict(mod_eff, X_cal_eff)
  pred_test_eff <- predict(mod_eff, X_test_eff)
  pred_cal_vol  <- predict(mod_vol, X_cal_vol)
  pred_test_vol <- predict(mod_vol, X_test_vol)

  # --- Component intervals: signed-residual asymmetric conformal ---
  qs_eff <- signed_quantile_set(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_vol <- signed_quantile_set(as.numeric(cal_data$opportunities) - pred_cal_vol)

  # --- Combined: Mechanism A scaling, asymmetric quantiles ---
  pred_cal_tot  <- pred_cal_eff  * pred_cal_vol
  pred_test_tot <- pred_test_eff * pred_test_vol
  resid_cal_tot <- cal_data$total_epa - pred_cal_tot
  cal_opp       <- as.numeric(cal_data$opportunities)
  test_opp      <- as.numeric(test_data$opportunities)

  alpha        <- fit_power_alpha(cal_opp, abs(resid_cal_tot))
  alpha_log[f] <- alpha

  qs_tot <- signed_quantile_set(resid_cal_tot / cal_opp^alpha)

  fold_results[[f]] <- test_data |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(
      build_asym_intervals(pred_test_eff, qs_eff, "eff"),
      build_asym_intervals(pred_test_vol, qs_vol, "vol"),
      build_asym_intervals(pred_test_tot, qs_tot, "tot", scale = test_opp^alpha)
    ) |>
    mutate(fold = f, alpha_fold = alpha)

  t1 <- proc.time()[["elapsed"]]
  if (f %% 25 == 0 || f == nrow(fold_map)) {
    cli_alert_info("Fold {sprintf('%03d', f)} [{test_season}-W{sprintf('%02d', test_week)}] done ({round(t1 - t0, 1)}s/fold)")
  }
}

results <- bind_rows(fold_results)

# ===========================================================================
# INTEGRITY + ASYMMETRY DIAGNOSTIC
# ===========================================================================

cli_h1("Integrity + asymmetry diagnostics")

if (n_distinct(results$fold) != nrow(fold_map)) cli_abort("Missing folds")
if (nrow(results) != EXPECTED_TEST_N) {
  cli_warn("Row count: {nrow(results)} vs expected {EXPECTED_TEST_N}")
} else {
  cli_alert_success("Row count: {nrow(results)} / {EXPECTED_TEST_N}")
}

na_eff <- sum(is.na(results$pred_eff))
na_vol <- sum(is.na(results$pred_vol))
na_tot <- sum(is.na(results$pred_tot))
if (na_eff + na_vol + na_tot == 0L) {
  cli_alert_success("Zero NA predictions")
} else {
  cli_warn("NA predictions: eff={na_eff}, vol={na_vol}, tot={na_tot}")
}

na_baseline <- sum(is.na(ft$baseline_target_share), is.na(ft$baseline_air_yards_share),
                    is.na(ft$baseline_snap_share), is.na(ft$baseline_tgt_per_snap),
                    is.na(ft$baseline_team_total_plays))
if (na_baseline == 0L) {
  cli_alert_success("Zero NA on baseline_* carryforward columns ({nrow(ft)} rows x 5 cols)")
} else {
  cli_warn("NA in baseline_* carryforward columns: {na_baseline}")
}

cli_alert_info("alpha range [{round(min(alpha_log), 3)}, {round(max(alpha_log), 3)}] | median {round(median(alpha_log), 3)}")

asym <- results |>
  summarise(
    up_80_tot   = mean(hi_80_tot - pred_tot),
    down_80_tot = mean(pred_tot - lo_80_tot),
    up_80_eff   = mean(hi_80_eff - pred_eff),
    down_80_eff = mean(pred_eff - lo_80_eff),
    med_shift   = mean(med_tot - pred_tot)
  )
cli_alert_info("Combined 80%: mean up-arm {round(asym$up_80_tot, 2)} vs down-arm {round(asym$down_80_tot, 2)} (up > down = right skew captured)")
cli_alert_info("Efficiency 80%: up {round(asym$up_80_eff, 3)} vs down {round(asym$down_80_eff, 3)}")
cli_alert_info("Median shift (med_tot - pred_tot): {round(asym$med_shift, 3)} EPA")

# ===========================================================================
# COVERAGE SCORING (same rubric as 12b)
# ===========================================================================

cli_h1("12c-TE VOLFIX Pooled Coverage")

pooled_te <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_te |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("12c-TE VOLFIX Low-Usage Bucket (targets {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
low_te <- bind_rows(
  score_component(res_lo$epa_per_opp_obs,           res_lo, "eff", "efficiency"),
  score_component(as.numeric(res_lo$opportunities), res_lo, "vol", "volume"),
  score_component(res_lo$total_epa,                 res_lo, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_te |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

res_strat <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI ~ "low  (3-5)",
      opportunities <= 9L         ~ "mid  (6-9)",
      TRUE                        ~ "high (10+)"
    ) |> factor(levels = c("low  (3-5)", "mid  (6-9)", "high (10+)"))
  )

strat_te <- eval_calibration_stratified(
  res_strat$total_epa,
  pi_cols(res_strat, "tot"),
  strata = res_strat$opp_bucket
) |> mutate(component = "combined")

cli_h2("Stratified combined coverage at 80%")
print(strat_te |> filter(nominal == 0.80) |>
      select(stratum, n, empirical, delta, sharpness) |>
      mutate(delta_pp = fmt_pp(delta)), n = Inf)

# ===========================================================================
# RUBRIC DECISION + COMPARISON TO 12C (immediate predecessor, same
# construction and hyperparameters, missing only baseline_* carryforward)
# ===========================================================================

cli_h1("Rubric Decision: 12c VOLFIX vs 12c (shipped)")

te_pool80 <- pooled_te |> filter(component == "combined", nominal == 0.80)
te_low80  <- low_te    |> filter(component == "combined", nominal == 0.80)

b_pool <- readr::read_csv("output/12c_te_asym_pooled_coverage.csv", show_col_types = FALSE) |>
  filter(component == "combined", nominal == 0.80)
b_low  <- readr::read_csv("output/12c_te_asym_low_usage_coverage.csv", show_col_types = FALSE) |>
  filter(component == "combined", nominal == 0.80)

cli_alert_info("12c (shipped, no baseline_*):  pooled {fmt_pp(b_pool$delta)} w={fmt_w(b_pool$sharpness)} | low {fmt_pp(b_low$delta)}")
cli_alert_info("12c VOLFIX (+baseline_*):      pooled {fmt_pp(te_pool80$delta)} w={fmt_w(te_pool80$sharpness)} | low {fmt_pp(te_low80$delta)}")

if (abs(te_low80$delta) > 0.10) {
  cli_warn("12c VOLFIX VETOED: low-usage 80% delta = {fmt_pp(te_low80$delta)}")
} else if (abs(te_pool80$delta) > 0.02) {
  cli_warn("12c VOLFIX pooled 80% outside +-2pp: {fmt_pp(te_pool80$delta)}")
} else {
  cli_alert_success("12c VOLFIX passes rubric: pooled {fmt_pp(te_pool80$delta)}, low {fmt_pp(te_low80$delta)}")
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs (VOLFIX -- new files, originals untouched)")

readr::write_csv(results,   "output/12c_te_asym_fold_predictions_volfix.csv")
readr::write_csv(pooled_te, "output/12c_te_asym_pooled_coverage_volfix.csv")
readr::write_csv(low_te,    "output/12c_te_asym_low_usage_coverage_volfix.csv")

cli_alert_success("output/12c_te_asym_fold_predictions_volfix.csv ({nrow(results)} rows)")
cli_alert_success("output/12c_te_asym_pooled_coverage_volfix.csv")
cli_alert_success("output/12c_te_asym_low_usage_coverage_volfix.csv")

cli_h1("Step 12c VOLFIX complete")
