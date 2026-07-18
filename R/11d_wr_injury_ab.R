# R/11d_wr_injury_ab.R
# Ablation ladder rung 1, arm B for WR: frozen 04c procedure (04b per-fold
# tuned hyperparameters reused, refit-only) with EX-ANTE INJURY STATE
# FEATURES (11b) added to the VOLUME model. Clone of
# 04c_wr_asymmetric_conformal.R except: this header, the injury join +
# VOL_FEATURES extension, FOLD_SUBSET env seam, 11d_ output paths, and an
# ARM A (shipped 04c) vs ARM B comparison tail under the frozen rubric.
#
# NOTE: hyperparameters are the 04b-tuned values, chosen WITHOUT the
# injury features -- a fixed-hyperparameter feature ablation. If WR
# surprised (diagnostic says it will not), a re-tuned run would follow.
#
# PRE-STATED EXPECTATIONS (2026-07-18): the 11a diagnostic found WR
# transition states FLAT (all |mean vol resid| <= 0.26 targets). Expected
# result: no material change anywhere -- this run exists to publish the
# null with a receipt. Decision rule: same frozen rubric; ship only if
# transition-state RMSE improves AND no rubric regression.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS (identical to 04b except no tuning grid)
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
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_team_total_plays",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far"
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
# LOAD FROZEN INPUTS + 04B TUNED PARAMS
# ===========================================================================

cli_h1("Step 4c: WR Asymmetric Conformal (04b learner, signed-residual intervals)")

ft       <- readRDS("data/wr_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")
tune_log <- readr::read_csv("output/04b_wr_lgbm_tune_log.csv", show_col_types = FALSE)

if (nrow(tune_log) != nrow(fold_map)) {
  cli_abort("Tune log has {nrow(tune_log)} rows; fold map has {nrow(fold_map)}")
}

EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()

cli_alert_success("WR feature table: {nrow(ft)} rows | {nrow(fold_map)} folds | expected test rows: {EXPECTED_TEST_N}")
cli_alert_info("Hyperparameters: per-fold tuned values reused from 04b tune log (no re-tuning)")

ft <- encode_features(ft)

# --- ARM B DELTA: ex-ante injury states (11b) into the volume model ---
INJURY_FEATURES <- c(
  "own_q_int", "own_practice_int", "weeks_missed", "return_from_absence",
  "above_new_out_share", "above_q_share", "above_long_out_share"
)
inj_states <- readRDS("data/injury_states_wr.rds")
ft <- ft |> left_join(inj_states, by = c("player_id", "season", "week"))
stopifnot(!any(is.na(ft$own_practice_int[!is.na(ft$player_id)])))
VOL_FEATURES <- c(VOL_FEATURES, INJURY_FEATURES)
cli_alert_info("ARM B: volume features extended with {length(INJURY_FEATURES)} injury-state features")

if (nzchar(Sys.getenv("FOLD_SUBSET"))) {
  keep <- seq(nrow(fold_map) - as.integer(Sys.getenv("FOLD_SUBSET")) + 1L, nrow(fold_map))
  fold_map <- fold_map[keep, ]
  tune_log <- tune_log[keep, ]
  EXPECTED_TEST_N <- ft |>
    semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
    nrow()
  cli_alert_warning("FOLD_SUBSET={nrow(fold_map)} folds (smoke test only)")
}

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

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)
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
# COVERAGE SCORING (same rubric as 04b)
# ===========================================================================

cli_h1("4c-WR Pooled Coverage")

pooled_wr <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_wr |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("4c-WR Low-Usage Bucket (targets {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
low_wr <- bind_rows(
  score_component(res_lo$epa_per_opp_obs,           res_lo, "eff", "efficiency"),
  score_component(as.numeric(res_lo$opportunities), res_lo, "vol", "volume"),
  score_component(res_lo$total_epa,                 res_lo, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_wr |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

res_strat <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI ~ "low  (3-5)",
      opportunities <= 9L         ~ "mid  (6-9)",
      TRUE                        ~ "high (10+)"
    ) |> factor(levels = c("low  (3-5)", "mid  (6-9)", "high (10+)"))
  )

strat_wr <- eval_calibration_stratified(
  res_strat$total_epa,
  pi_cols(res_strat, "tot"),
  strata = res_strat$opp_bucket
) |> mutate(component = "combined")

cli_h2("Stratified combined coverage at 80%")
print(strat_wr |> filter(nominal == 0.80) |>
      select(stratum, n, empirical, delta, sharpness) |>
      mutate(delta_pp = fmt_pp(delta)), n = Inf)

# ===========================================================================
# SAVE ARM B OUTPUTS + FROZEN-RUBRIC COMPARISON vs SHIPPED 04c
# ===========================================================================

cli_h1("Save arm B outputs")
readr::write_csv(results,   "output/11d_wr_injury_fold_predictions.csv")
readr::write_csv(pooled_wr, "output/11d_wr_injury_pooled_coverage.csv")
readr::write_csv(low_wr,    "output/11d_wr_injury_low_usage_coverage.csv")
cli_alert_success("output/11d_wr_injury_* written")

cli_h1("Frozen rubric: ARM A (shipped 04c) vs ARM B (+injury states)")

preds_a <- readr::read_csv("output/04c_wr_asym_fold_predictions.csv",
                           show_col_types = FALSE) |>
  filter(!is.na(player_id)) |>
  semi_join(results, by = c("season", "week"))
results_r <- results |> filter(!is.na(player_id))
stopifnot(nrow(preds_a) == nrow(results_r))

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
                    score_arm(results_r, "B_injury")) |>
  filter(nominal == 0.80) |>
  select(arm, component, stratum, n, empirical, delta, sharpness)

cli_h2("80% coverage + width -- veto: low-usage |delta| > 10pp")
print(rubric |> mutate(delta_pp = fmt_pp(delta),
                       veto = ifelse(stratum == "low_usage" & abs(delta) > 0.10,
                                     "TRIGGER", "pass")), n = Inf)

cli_h1("Transition-state slice")

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
    summarise(n = n(), mean_vol_resid = mean(vol_resid),
              vol_rmse = sqrt(mean(vol_resid^2)),
              cov80 = mean(cov80), .groups = "drop") |>
    mutate(arm = label)
}

trans_wide <- bind_rows(slice_arm(preds_a, "A_shipped"),
                        slice_arm(results_r, "B_injury")) |>
  pivot_wider(names_from = arm, values_from = c(mean_vol_resid, vol_rmse, cov80)) |>
  mutate(d_rmse = vol_rmse_B_injury - vol_rmse_A_shipped,
         d_cov_pp = 100 * (cov80_B_injury - cov80_A_shipped)) |>
  arrange(desc(abs(mean_vol_resid_A_shipped)))
print(trans_wide |> mutate(across(where(is.numeric) & !c(n), ~ round(.x, 2))),
      n = Inf, width = Inf)

readr::write_csv(rubric,     "output/11d_wr_rubric_table.csv")
readr::write_csv(trans_wide, "output/11d_wr_transition_slice.csv")
cli_alert_success("output/11d_wr_rubric_table.csv + output/11d_wr_transition_slice.csv")

cli_h1("11d WR arm B complete -- expected null; judge per header rule")
