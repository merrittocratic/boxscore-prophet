# R/11c_rb_injury_volfix.R
# RB VOLUME RETRAIN (VOLFIX): the deployed RB procedure (11c_rb_injury_ab.R,
# whose VOL_FEATURES matches RB_VOL_FEATURES in R/10a_deployment_models.R
# exactly) with the baseline_* prior-season carryforward features (6713b0e)
# added to the volume model only.
#
# CLONE-DISCIPLINE NOTE (judgment call, flagged per task instructions):
# 11c is NOT structured like 04c/04b for WR. WR splits tuning (04b, full
# per-fold grid search, writes a tune log) from scoring (04c, refit-only,
# reads that tune log, changes only the conformal construction). RB has no
# such split -- 03a_v2_lgbm_tuned.R and its 11c descendant both tune AND
# score inside the same walk-forward loop; there is no separate "RB 04b".
#
# The task asked to reuse already-tuned hyperparameters and NOT retune, so
# this script does NOT clone 11c's tuning loop verbatim. Instead it is
# structured like 04c: refit-only, reusing the per-fold hyperparameters
# already written to output/11c_rb_injury_tune_log.csv (11c's own tuning
# output -- the RB equivalent of 04b_wr_lgbm_tune_log.csv). This keeps the
# comparison clean (identical hyperparameters per fold, only VOL_FEATURES
# differs) and avoids an unrequested full re-tune. If a byte-identical clone
# of 11c's nested-tuning loop is wanted instead, that is a straightforward
# follow-up (just add baseline_* to VOL_FEATURES in 11c_rb_injury_ab.R and
# rerun the tuning loop) -- flagging so this choice can be revisited.
#
# CLONE DISCIPLINE (relative to 11c_rb_injury_ab.R): this file keeps 11c's
# feature sets, injury-state join, fold map, fit/cal split logic, and
# symmetric Mechanism A conformal construction. It differs from 11c in:
#   (1) this header: (2) VOL_FEATURES gains baseline_carry_share,
#   baseline_target_share, baseline_snap_share, baseline_team_total_plays;
#   (3) the walk-forward loop is refit-only, reusing per-fold hyperparameters
#   read from output/11c_rb_injury_tune_log.csv instead of re-running the
#   32-combo grid search; (4) output paths get a _volfix suffix so 11c's
#   frozen outputs are never overwritten; (5) the transition-state injury
#   slice (11c's own ablation diagnostic, not relevant to this fix) is
#   dropped, but the ARM comparison (this run vs the immediate predecessor,
#   11c's shipped injury-arm predictions) is kept.
#
# WHY: rb_feature_table.rds volume features were NA at every player's
# Week 1 pre-fix (rookie or veteran alike). The 11c model currently
# deployed (RB_VOL_FEATURES in 10a) was trained before that fix landed.
# This retrain answers whether restoring real Week 1 volume signal changes
# the model's raw score distribution for known high-usage backs -- see the
# companion before/after check run after this script completes.
#
# LEAKAGE DISCIPLINE: unchanged from 11c/03a-v2 -- same walk-forward folds
# (data/fold_map.rds), same fit/cal split (last CAL_FRAC season-weeks of
# train as calibration), test fold never touches parameter selection
# (parameters here are reused from a prior run, not selected on this run's
# test data at all).
#
# CONSTRUCTION: FROZEN. Same power-law Mechanism A as 3A/3A-v2/11c.
# RUBRIC: FROZEN. Same veto, decision rule, folds.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS (identical to 11c except no tuning grid)
# ===========================================================================

CAL_FRAC   <- 0.20
LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

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
  "def_rush_epa_adj", "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

VOL_FEATURES <- c(
  "wt_carry_share", "wt_target_share", "wt_snap_share", "wt_team_total_plays",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far",
  # VOLFIX: prior-season carryforward (6713b0e), fixes NA volume features at
  # Week 1 for every player (rookie or veteran) -- see script header.
  "baseline_carry_share", "baseline_target_share", "baseline_snap_share",
  "baseline_team_total_plays"
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
# LOAD FROZEN INPUTS + 11C TUNED PARAMS
# ===========================================================================

cli_h1("RB VOLFIX: baseline carryforward retrain (11c learner, refit-only)")

ft       <- readRDS("data/rb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")
tune_log <- readr::read_csv("output/11c_rb_injury_tune_log.csv", show_col_types = FALSE)

if (nrow(tune_log) != nrow(fold_map)) {
  cli_abort("Tune log has {nrow(tune_log)} rows; fold map has {nrow(fold_map)}")
}

EXPECTED_TEST_N <- sum(fold_map$n_test_rows)
cli_alert_success("RB feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")
cli_alert_info("Hyperparameters: per-fold tuned values reused from 11c_rb_injury_tune_log.csv (no re-tuning)")

ft <- encode_features(ft)

# --- Ex-ante injury states (11b), same join as 11c ---
INJURY_FEATURES <- c(
  "own_q_int", "own_practice_int", "weeks_missed", "return_from_absence",
  "above_new_out_share", "above_q_share", "above_long_out_share"
)
inj_states <- readRDS("data/injury_states_rb.rds")
ft <- ft |> left_join(inj_states, by = c("player_id", "season", "week"))
stopifnot(!any(is.na(ft$own_practice_int)))
VOL_FEATURES <- c(VOL_FEATURES, INJURY_FEATURES)
cli_alert_info("Volume features: 11c base + baseline_* carryforward + {length(INJURY_FEATURES)} injury-state features ({length(VOL_FEATURES)} total)")

if (nzchar(Sys.getenv("FOLD_SUBSET"))) {
  fold_map <- tail(fold_map, as.integer(Sys.getenv("FOLD_SUBSET")))
  tune_log <- tail(tune_log, as.integer(Sys.getenv("FOLD_SUBSET")))
  cli_alert_warning("FOLD_SUBSET={nrow(fold_map)} folds (smoke test only)")
}

# ===========================================================================
# WALK-FORWARD LOOP (refit + symmetric conformal only)
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

  fit_sws  <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

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

  # --- Component conformal intervals (symmetric, unchanged from 11c/3A) ---
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
  if (f %% 25 == 0 || f == nrow(fold_map)) {
    cli_alert_info("Fold {sprintf('%03d', f)} [{test_season}-W{sprintf('%02d', test_week)}] done ({round(t1 - t0, 1)}s/fold)")
  }
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

cli_alert_info("alpha_fold range [{round(min(alpha_log),3)}, {round(max(alpha_log),3)}] | median {round(median(alpha_log),3)}")

# ===========================================================================
# COVERAGE SCORING
# ===========================================================================

cli_h1("RB VOLFIX Pooled Coverage (all {n_scored} test rows)")

pooled_rb <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_rb |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("RB VOLFIX Low-Usage Bucket (opp {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo_rb <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low     <- nrow(res_lo_rb)
cli_alert_info("Low-usage rows: {n_low}")

low_rb <- bind_rows(
  score_component(res_lo_rb$epa_per_opp_obs,           res_lo_rb, "eff", "efficiency"),
  score_component(as.numeric(res_lo_rb$opportunities), res_lo_rb, "vol", "volume"),
  score_component(res_lo_rb$total_epa,                 res_lo_rb, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_rb |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

res_strat_rb <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
      opportunities <= 13L        ~ "mid  (9-13)",
      TRUE                        ~ "high (14+)"
    ) |> factor(levels = c(paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
                            "mid  (9-13)", "high (14+)"))
  )

strat_rb <- eval_calibration_stratified(
  res_strat_rb$total_epa,
  pi_cols(res_strat_rb, "tot"),
  strata = res_strat_rb$opp_bucket
) |> mutate(component = "combined")

cli_h2("Stratified combined coverage at 80% (sharpness honesty check)")
print(strat_rb |> filter(nominal == 0.80) |>
      select(stratum, n, empirical, delta, sharpness) |>
      mutate(delta_pp = fmt_pp(delta)), n = Inf)

# ===========================================================================
# RUBRIC DECISION + COMPARISON TO 11C (immediate predecessor, same
# construction and hyperparameters, missing only baseline_* carryforward)
# ===========================================================================

cli_h1("Rubric Decision: RB VOLFIX vs 11c (shipped)")

rb_pool80 <- pooled_rb |> filter(component == "combined", nominal == 0.80)
rb_low80  <- low_rb    |> filter(component == "combined", nominal == 0.80)

b_pool <- readr::read_csv("output/11c_rb_injury_pooled_coverage.csv", show_col_types = FALSE) |>
  filter(component == "combined", nominal == 0.80)
b_low  <- readr::read_csv("output/11c_rb_injury_low_usage_coverage.csv", show_col_types = FALSE) |>
  filter(component == "combined", nominal == 0.80)

cli_alert_info("11c (shipped, no baseline_*):  pooled {fmt_pp(b_pool$delta)} w={fmt_w(b_pool$sharpness)} | low {fmt_pp(b_low$delta)}")
cli_alert_info("VOLFIX (+baseline_*):          pooled {fmt_pp(rb_pool80$delta)} w={fmt_w(rb_pool80$sharpness)} | low {fmt_pp(rb_low80$delta)}")

if (abs(rb_low80$delta) > 0.10) {
  cli_warn("VOLFIX VETOED: low-usage 80% delta = {fmt_pp(rb_low80$delta)}")
} else if (abs(rb_pool80$delta) > 0.02) {
  cli_warn("VOLFIX pooled 80% outside +-2pp: {fmt_pp(rb_pool80$delta)}")
} else {
  cli_alert_success("VOLFIX passes rubric: pooled {fmt_pp(rb_pool80$delta)}, low {fmt_pp(rb_low80$delta)}")
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs (VOLFIX -- new files, originals untouched)")

readr::write_csv(results,   "output/11c_rb_injury_fold_predictions_volfix.csv")
readr::write_csv(pooled_rb, "output/11c_rb_injury_pooled_coverage_volfix.csv")
readr::write_csv(low_rb,    "output/11c_rb_injury_low_usage_coverage_volfix.csv")

cli_alert_success("output/11c_rb_injury_fold_predictions_volfix.csv ({nrow(results)} rows)")
cli_alert_success("output/11c_rb_injury_pooled_coverage_volfix.csv")
cli_alert_success("output/11c_rb_injury_low_usage_coverage_volfix.csv")

cli_h1("RB VOLFIX complete")
