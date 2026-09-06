# R/04d_wr_nona_ab.R
# Step 4d: A/B sizing of the WR NA-player training contamination.
#
# FINDING (2026-07-18, caught by the 10b3 slate gate): the frozen WR
# feature table carries one NA-player pseudo-row per team-game (3,734
# rows, ~17%) -- unattributed targets pass the play filter because
# wr_ids includes an NA gsis_id. These rows were part of 04b/04c
# training and conformal calibration; the published FP chain filtered
# them before scoring (06b), so only the LEARNED objects are in question.
#
# DESIGN (pre-committed before this run):
#   B arm = exact 04c construction (per-fold hyperparams FROZEN from the
#   shipped 04b tune log, seed 42, same folds, same conformal) with ONE
#   change: filter(!is.na(player_id)) on the feature table. Freezing the
#   params isolates the contamination effect on fits + calibration
#   without confounding from grid re-selection.
#   A arm = shipped 04c fold predictions.
#   Both arms graded on the IDENTICAL real-player row set (inner join).
#
# DECISION RULE (pre-committed): contamination is IMMATERIAL if
#   (1) |80% combined coverage difference| <= 1pp, pooled AND low-usage
#   (2) 80% combined width change <= 2%
#   -> keep shipped chain, record receipt, fix filter in next feature
#      table version. Otherwise -> schedule WR chain rebuild pre-launch.
# EXPECTED: immaterial (NA rows are plausible anonymous low-target
# aggregates; LightGBM routes their NA player-history features).

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

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
COVERAGES      <- c(0.50, 0.80, 0.90)

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

signed_quantile_set <- function(resid_signed) {
  list(
    lo  = vapply(COVERAGES, function(c) conformal_q_signed(resid_signed, (1 - c) / 2), numeric(1)),
    hi  = vapply(COVERAGES, function(c) conformal_q_signed(resid_signed, (1 + c) / 2), numeric(1)),
    med = quantile(resid_signed, 0.50, names = FALSE)
  )
}

fit_power_alpha <- function(opp, raw_resid) {
  df  <- data.frame(log_opp = log(opp), log_resid = log(raw_resid + 1e-8))
  fit <- tryCatch(lm(log_resid ~ log_opp, data = df), error = function(e) NULL)
  if (is.null(fit)) return(ALPHA_FALLBACK)
  alpha <- unname(coef(fit)["log_opp"])
  if (!is.finite(alpha)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, alpha))
}

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

fmt_pp <- function(x) sprintf("%+.2fpp", x * 100)

# ===========================================================================
# LOAD (B arm: NA players filtered -- THE single changed factor)
# ===========================================================================

cli_h1("Step 4d: WR NA-contamination A/B (B arm: filtered refit)")

ft       <- readRDS("data/wr_feature_table.rds") |> filter(!is.na(player_id))
fold_map <- readRDS("data/fold_map.rds")
tune_log <- readr::read_csv("output/04b_wr_lgbm_tune_log.csv", show_col_types = FALSE)

cli_alert_success("Filtered WR table: {nrow(ft)} rows (NA-player rows removed)")

ft <- encode_features(ft)

# ===========================================================================
# WALK-FORWARD LOOP (identical to 04c, refit-only)
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds, refit-only)")

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

  qs_eff <- signed_quantile_set(cal_data$epa_per_opp_obs - pred_cal_eff)
  qs_vol <- signed_quantile_set(as.numeric(cal_data$opportunities) - pred_cal_vol)

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

  if (f %% 50 == 0 || f == nrow(fold_map)) {
    cli_alert_info("Fold {sprintf('%03d', f)} done")
  }
}

results_b <- bind_rows(fold_results)

# ===========================================================================
# A/B COMPARISON ON IDENTICAL REAL-PLAYER ROWS
# ===========================================================================

cli_h1("A/B comparison (identical real-player rows)")

results_a <- readr::read_csv("output/04c_wr_asym_fold_predictions.csv",
                             show_col_types = FALSE) |>
  filter(!is.na(player_id))

joined <- results_a |>
  select(player_id, season, week, opportunities, total_epa,
         a_pred_tot = pred_tot,
         a_lo_50 = lo_50_tot, a_hi_50 = hi_50_tot,
         a_lo_80 = lo_80_tot, a_hi_80 = hi_80_tot,
         a_lo_90 = lo_90_tot, a_hi_90 = hi_90_tot) |>
  inner_join(
    results_b |>
      select(player_id, season, week,
             b_pred_tot = pred_tot,
             b_lo_50 = lo_50_tot, b_hi_50 = hi_50_tot,
             b_lo_80 = lo_80_tot, b_hi_80 = hi_80_tot,
             b_lo_90 = lo_90_tot, b_hi_90 = hi_90_tot),
    by = c("player_id", "season", "week")
  )

cli_alert_info("A rows: {nrow(results_a)} | B rows: {nrow(results_b)} | joined: {nrow(joined)}")
if (nrow(joined) != nrow(results_a) || nrow(joined) != nrow(results_b)) {
  cli_warn("Row sets differ between arms -- inspect before trusting the comparison")
}

cover <- function(y, lo, hi) mean(y >= lo & y <= hi, na.rm = TRUE)
width <- function(lo, hi) mean(hi - lo, na.rm = TRUE)

ab_table <- function(df, label) {
  map_dfr(c("50", "80", "90"), function(lv) {
    tibble(
      stratum = label, nominal = as.numeric(lv) / 100,
      cov_a   = cover(df$total_epa, df[[paste0("a_lo_", lv)]], df[[paste0("a_hi_", lv)]]),
      cov_b   = cover(df$total_epa, df[[paste0("b_lo_", lv)]], df[[paste0("b_hi_", lv)]]),
      wid_a   = width(df[[paste0("a_lo_", lv)]], df[[paste0("a_hi_", lv)]]),
      wid_b   = width(df[[paste0("b_lo_", lv)]], df[[paste0("b_hi_", lv)]])
    )
  })
}

low <- joined |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
ab  <- bind_rows(ab_table(joined, "pooled"),
                 ab_table(low, sprintf("low_opp_%d_%d", LOW_OPP_LO, LOW_OPP_HI))) |>
  mutate(cov_diff_pp = 100 * (cov_b - cov_a),
         wid_pct     = 100 * (wid_b / wid_a - 1))

cli_h2("Coverage and width, A (shipped) vs B (filtered), same rows")
print(ab |>
        mutate(across(c(cov_a, cov_b), ~ sprintf("%.3f", .x)),
               across(c(wid_a, wid_b), ~ sprintf("%.2f", .x)),
               cov_diff_pp = sprintf("%+.2f", cov_diff_pp),
               wid_pct     = sprintf("%+.2f%%", wid_pct)) |>
        as.data.frame())

pred_move <- abs(joined$b_pred_tot - joined$a_pred_tot)
cli_h2("Prediction movement (pred_tot, |B - A|, EPA)")
cli_alert_info("median = {round(median(pred_move), 3)} | p90 = {round(quantile(pred_move, 0.9), 3)} | max = {round(max(pred_move), 3)}")
cli_alert_info("RMSE vs observed: A = {round(sqrt(mean((joined$total_epa - joined$a_pred_tot)^2)), 4)} | B = {round(sqrt(mean((joined$total_epa - joined$b_pred_tot)^2)), 4)}")

# ===========================================================================
# PRE-COMMITTED DECISION RULE
# ===========================================================================

cli_h1("Decision (pre-committed rule)")

chk <- ab |> filter(nominal == 0.80)
cov_ok <- all(abs(chk$cov_diff_pp) <= 1.0)
wid_ok <- all(abs(chk$wid_pct) <= 2.0)

for (i in seq_len(nrow(chk))) {
  cli_alert_info("{chk$stratum[i]} 80%: coverage diff {sprintf('%+.2f', chk$cov_diff_pp[i])}pp | width {sprintf('%+.2f', chk$wid_pct[i])}%")
}

if (cov_ok && wid_ok) {
  cli_alert_success("IMMATERIAL: both gates within bounds -- keep shipped chain; fix filter in next feature-table version")
} else {
  cli_alert_danger("MATERIAL: outside pre-committed bounds -- schedule WR chain rebuild before launch")
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
readr::write_csv(results_b, "output/04d_wr_nona_fold_predictions.csv")
readr::write_csv(ab,        "output/04d_wr_nona_ab_table.csv")
cli_alert_success("output/04d_wr_nona_fold_predictions.csv ({nrow(results_b)} rows)")
cli_alert_success("output/04d_wr_nona_ab_table.csv")

cli_h1("Step 4d complete")
