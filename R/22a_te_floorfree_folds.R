# R/22a_te_floorfree_folds.R
# TE floor-free volume retrain: walk-forward fold predictions (D32).
#
# WHY (found 2026-10-06 chasing the AJ Barner W4 movers miss): the TE VOL
# component has always trained on data/te_feature_table.rds, which keeps
# only weeks with 3+ targets (12a MIN_OPPORTUNITIES floor). The floor exists
# because epa_per_opp is undefined at zero opportunities -- the EFF
# component needs it, the VOL component never did. A volume model that never
# sees a 1-2 target week over-projects every low-volume TE:
#   - out-of-fit 2023 W16 -> 2026 W3, all 1+ target weeks: actual 3.53 vs
#     pred 4.98 targets (bias -1.45); 3+ weeks only: -0.11
#   - live 2026 W1-4 ledger: pred 4.89 vs actual 2.89; mean P(start) 24.5%
#     vs hit rate 13.4% (pred <5 tgts: 17.9% vs 4.2%)
# The rejected first hypothesis (one spike week over-inflates the
# projection) is NOT what this fixes: post-spike reaction was 39% of the
# way to the spike vs 34% in reality, CI spans 0.
#
# WHAT CHANGES vs the canonical TE chain (13e_te_fold_predictions_volfix):
#   - VOL trains on the floor-free table (MIN_OPP=1, all 1+ target weeks),
#     same features/column order (12c volfix VOL set), same per-fold 12b
#     hyperparameters (no re-tune -- frozen-procedure convention, same as
#     the volfix retrain).
#   - EFF is UNCHANGED: trains on the floored table exactly as 13b arm B
#     (opener Vegas features), refit from the 13b volfix-open per-fold tune
#     log. It does NOT bit-reproduce the Aug-31 canonical 13e file (max
#     |diff| ~0.08-0.15): those 13b outputs postdate the last committed
#     feature table, and the weekly table has since picked up a constant
#     +0.001 def_*_epa_adj centering shift (anchor rollover). So this file
#     rebuilds BOTH arms on today's tables with the SAME eff model object:
#       OLD arm = floored VOL, floored cal, floored test rows (the current
#                 production procedure, re-run) -> 22a_te_floored_*
#       NEW arm = floor-free VOL, floor-free cal/test rows -> 22a_te_floorfree_*
#     Every bar compares NEW against OLD, like for like.
#   - Test rows = every floor-free test row (1+ targets), so the FP chain
#     (12d0 -> 12d -> 12e) and the recal maps are refit on the population
#     the live board actually scores. te_outcomes.rds (the 12d translation
#     + residual pools) was ALREADY floor-free -- untouched.
#   - Vol/tot conformal residuals use floor-free cal rows; eff conformal
#     uses floored cal rows (eff is undefined below its floor anyway).
#   - 0-target weeks are still absent (the outcome table is built from
#     plays); the floor-free fix narrows the bias, it cannot remove that part.
#
# PRE-REGISTERED BARS (stated 2026-10-06 before this ran; Steve approved
# the retrain, these bars are the ship gate):
#   B1 (volume bias, primary): walk-forward test rows, all 1+ target weeks,
#      mean(actual - pred_vol) within +-0.30 targets (old chain on the same
#      rows, scored by this file's floored refit, for comparison).
#   B2 (no harm where the old model was fine): on 3+ target rows, |bias|
#      no worse than 0.30 and vol RMSE no worse than +5% vs canonical.
#   B3 (calibration, after 12d0/12d/12e refit on these folds): TE P(12+)
#      reliability on the full 1+ population -- mean predicted within 2pp of
#      hit rate; Brier on the 3+ subset no worse than canonical +0.002.
#   B4 (live hindcast, 2026 W1-4 real slates): candidate TE mean P(start)
#      within 3pp of realized hit rate, and Brier better than the live
#      ledger's on the same rows.
# Fail any bar -> do not ship; report the receipts.
#
# OUTPUT: output/22a_te_floorfree_fold_predictions.csv (canonical 13e
# column layout, consumed by 12d0 via TE0_IN/TE0_OUT).

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

set.seed(42)

CAL_FRAC         <- 0.20
REFIT_ROUNDS_MIN <- 10L
COVERAGES        <- c(0.50, 0.80, 0.90)
ALPHA_LO <- 0.20; ALPHA_HI <- 0.90; ALPHA_FALLBACK <- 0.50

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

# Feature sets verbatim from 13b_vegas_ab_volfix.R TE config (column order
# matters: feature_fraction samples columns by index under a fixed seed)
EFF_FEATURES <- c("prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp",
                  "form_residual", "is_cold_start_int", "draft_tier_int",
                  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
                  "wt_air_yards_per_target",
                  "wt_snap_share", "games_played_so_far", "def_used_fallback_int",
                  "team_spread", "implied_total")
VOL_FEATURES <- c("wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_tgt_per_snap",
                  "wt_team_total_plays", "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
                  "draft_tier_int", "is_cold_start_int", "games_played_so_far",
                  "baseline_target_share", "baseline_air_yards_share", "baseline_snap_share",
                  "baseline_tgt_per_snap", "baseline_team_total_plays")

FLOORED_TABLE   <- Sys.getenv("TE_FLOORED_TABLE", "data/te_feature_table.rds")
FLOORFREE_TABLE <- Sys.getenv("TE_FLOORFREE_TABLE", "data/te_feature_table_floorfree.rds")
CANON_PREDS     <- "output/13e_te_fold_predictions_volfix.csv"
EFF_TUNE_LOG    <- "output/13b_te_vegas_volfix_open_tune_log.csv"
VOL_TUNE_LOG    <- "output/12b_te_lgbm_tune_log.csv"
OUT_PATH        <- Sys.getenv("TE_FF_FOLDS_OUT", "output/22a_te_floorfree_fold_predictions.csv")
OLD_OUT_PATH    <- Sys.getenv("TE_FL_FOLDS_OUT", "output/22a_te_floored_fold_predictions.csv")

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |>
    mutate(
      draft_tier_int        = TIER_ORDER[draft_tier],
      is_cold_start_int     = as.integer(is_cold_start),
      def_used_fallback_int = as.integer(def_used_fallback)
    )
}

make_matrix <- function(df, features) df |> select(all_of(features)) |> as.matrix()

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
  a <- unname(coef(fit)["log_opp"])
  if (!is.finite(a)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, a))
}

fit_lgbm <- function(X, y, num_leaves, lr, min_node, n_rounds) {
  keep <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = c(LGBM_FIXED, list(num_leaves = num_leaves,
                                        learning_rate = lr,
                                        min_data_in_leaf = min_node)),
            data = dtrain, nrounds = n_rounds, verbose = -1L)
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

# ===========================================================================
# 1. LOAD
# ===========================================================================

cli_h1("22a: TE floor-free VOL walk-forward (EFF unchanged)")

open_lines <- readRDS("data/vegas_open_lines.rds")
prep <- function(path) readRDS(path) |> encode_features() |>
  left_join(open_lines, by = c("game_id", "posteam"))

ft_fl <- prep(FLOORED_TABLE)
ft_ff <- prep(FLOORFREE_TABLE)
fold_map <- readRDS("data/fold_map.rds")
tl_eff   <- readr::read_csv(EFF_TUNE_LOG, show_col_types = FALSE)
tl_vol   <- readr::read_csv(VOL_TUNE_LOG, show_col_types = FALSE)
canon    <- readr::read_csv(CANON_PREDS, show_col_types = FALSE) |> filter(!is.na(player_id))
stopifnot(nrow(tl_eff) == nrow(fold_map), nrow(tl_vol) == nrow(fold_map))

missing <- setdiff(c(EFF_FEATURES, VOL_FEATURES), intersect(names(ft_fl), names(ft_ff)))
if (length(missing)) cli_abort("Missing features: {paste(missing, collapse = ', ')}")

cli_alert_success("Floored rows {nrow(ft_fl)} | floor-free rows {nrow(ft_ff)} | folds {nrow(fold_map)}")

# ===========================================================================
# 2. WALK-FORWARD
# ===========================================================================

fold_results <- vector("list", nrow(fold_map))
vol_old_rows <- vector("list", nrow(fold_map))
old_results  <- vector("list", nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {
  ts <- fold_map$test_season[f]; tw <- fold_map$test_week[f]
  before <- function(d) d |> filter(season < ts | (season == ts & week < tw))

  # Season-week split from the floored table (identical to 13b) so EFF
  # reproduces exactly; the floor-free fit/cal use the same season-weeks.
  train_fl  <- before(ft_fl)
  train_sws <- train_fl |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)
  fit_sws   <- head(train_sws, nrow(train_sws) - n_cal_sw)

  fit_fl <- train_fl |> semi_join(fit_sws, by = c("season", "week"))
  cal_fl <- train_fl |> semi_join(cal_sws, by = c("season", "week"))
  train_ff <- before(ft_ff)
  fit_ff <- train_ff |> semi_join(fit_sws, by = c("season", "week"))
  cal_ff <- train_ff |> semi_join(cal_sws, by = c("season", "week"))
  test_ff <- ft_ff |> filter(season == ts, week == tw)
  test_fl <- ft_fl |> filter(season == ts, week == tw)

  te <- tl_eff[f, ]; tv <- tl_vol[f, ]
  mod_eff <- fit_lgbm(make_matrix(fit_fl, EFF_FEATURES), fit_fl$epa_per_opp_obs,
                      te$eff_num_leaves, te$eff_lr, te$eff_min_node,
                      max(REFIT_ROUNDS_MIN, te$eff_rounds))
  mod_vol <- fit_lgbm(make_matrix(fit_ff, VOL_FEATURES), as.numeric(fit_ff$opportunities),
                      tv$vol_num_leaves, tv$vol_lr, tv$vol_min_node,
                      max(REFIT_ROUNDS_MIN, tv$vol_rounds))
  # Floored VOL refit (canonical procedure) scored on the SAME floor-free
  # test rows -- the like-for-like "old chain" comparison for B1.
  mod_vol_old <- fit_lgbm(make_matrix(fit_fl, VOL_FEATURES), as.numeric(fit_fl$opportunities),
                          tv$vol_num_leaves, tv$vol_lr, tv$vol_min_node,
                          max(REFIT_ROUNDS_MIN, tv$vol_rounds))

  qs_eff <- signed_quantile_set(cal_fl$epa_per_opp_obs -
                                  predict(mod_eff, make_matrix(cal_fl, EFF_FEATURES)))
  pred_cal_vol <- predict(mod_vol, make_matrix(cal_ff, VOL_FEATURES))
  qs_vol <- signed_quantile_set(as.numeric(cal_ff$opportunities) - pred_cal_vol)

  pred_cal_tot <- predict(mod_eff, make_matrix(cal_ff, EFF_FEATURES)) * pred_cal_vol
  resid_cal_tot <- cal_ff$total_epa - pred_cal_tot
  cal_opp <- as.numeric(cal_ff$opportunities)
  alpha   <- fit_power_alpha(cal_opp, abs(resid_cal_tot))
  qs_tot  <- signed_quantile_set(resid_cal_tot / cal_opp^alpha)

  pred_test_eff <- predict(mod_eff, make_matrix(test_ff, EFF_FEATURES))
  pred_test_vol <- predict(mod_vol, make_matrix(test_ff, VOL_FEATURES))
  test_opp      <- as.numeric(test_ff$opportunities)

  fold_results[[f]] <- test_ff |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(
      build_asym_intervals(pred_test_eff, qs_eff, "eff"),
      build_asym_intervals(pred_test_vol, qs_vol, "vol"),
      tibble(fold = f, alpha_fold = alpha),
      build_asym_intervals(pred_test_eff * pred_test_vol, qs_tot, "tot",
                           scale = test_opp^alpha)
    )
  vol_old_rows[[f]] <- test_ff |> select(player_id, season, week) |>
    mutate(pred_vol_old = predict(mod_vol_old, make_matrix(test_ff, VOL_FEATURES)))

  # OLD arm: the production procedure as-is (floored everywhere)
  pred_cal_vol_o <- predict(mod_vol_old, make_matrix(cal_fl, VOL_FEATURES))
  qs_vol_o  <- signed_quantile_set(as.numeric(cal_fl$opportunities) - pred_cal_vol_o)
  pred_cal_tot_o <- predict(mod_eff, make_matrix(cal_fl, EFF_FEATURES)) * pred_cal_vol_o
  resid_o   <- cal_fl$total_epa - pred_cal_tot_o
  cal_opp_o <- as.numeric(cal_fl$opportunities)
  alpha_o   <- fit_power_alpha(cal_opp_o, abs(resid_o))
  qs_tot_o  <- signed_quantile_set(resid_o / cal_opp_o^alpha_o)
  pe_o <- predict(mod_eff, make_matrix(test_fl, EFF_FEATURES))
  pv_o <- predict(mod_vol_old, make_matrix(test_fl, VOL_FEATURES))
  old_results[[f]] <- test_fl |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(
      build_asym_intervals(pe_o, qs_eff, "eff"),
      build_asym_intervals(pv_o, qs_vol_o, "vol"),
      tibble(fold = f, alpha_fold = alpha_o),
      build_asym_intervals(pe_o * pv_o, qs_tot_o, "tot",
                           scale = as.numeric(test_fl$opportunities)^alpha_o)
    )

  if (f %% 25 == 0 || f == nrow(fold_map)) cli_alert_info("Fold {f}/{nrow(fold_map)} [{ts}-W{tw}]")
}

results <- bind_rows(fold_results)
vol_old <- bind_rows(vol_old_rows)
old_res <- bind_rows(old_results)

dup_keys <- results |> count(player_id, season, week) |> filter(n > 1) |> select(-n)
if (nrow(dup_keys)) {
  cli_alert_warning("Dropping {nrow(dup_keys)} duplicated player-week key{?s} (feature-table dup, same exclusion as 13e)")
  results <- results |> anti_join(dup_keys, by = c("player_id", "season", "week"))
  vol_old <- vol_old |> anti_join(dup_keys, by = c("player_id", "season", "week"))
  old_res <- old_res |> anti_join(dup_keys, by = c("player_id", "season", "week"))
}

# ===========================================================================
# 3. INTEGRITY: arms share EFF; drift vs the Aug-31 canonical (report only)
# ===========================================================================

cli_h1("Integrity")
eff_arms <- results |> select(player_id, season, week, e_new = pred_eff) |>
  inner_join(old_res |> select(player_id, season, week, e_old = pred_eff), by = c("player_id", "season", "week"))
if (max(abs(eff_arms$e_new - eff_arms$e_old)) > 1e-12) cli_abort("Arms disagree on EFF -- they must share one model.")
if (nrow(old_res) != nrow(canon)) cli_alert_warning("OLD arm rows {nrow(old_res)} vs canonical {nrow(canon)}")
eff_chk <- old_res |> select(player_id, season, week, e = pred_eff, v = pred_vol) |>
  inner_join(canon |> select(player_id, season, week, ec = pred_eff, vc = pred_vol), by = c("player_id", "season", "week"))
cli_alert_info("Drift vs Aug-31 canonical ({nrow(eff_chk)} rows): EFF mean |diff| {signif(mean(abs(eff_chk$e - eff_chk$ec)), 3)}, max {signif(max(abs(eff_chk$e - eff_chk$ec)), 3)} | VOL mean |diff| {signif(mean(abs(eff_chk$v - eff_chk$vc)), 3)}, max {signif(max(abs(eff_chk$v - eff_chk$vc)), 3)}")

# ===========================================================================
# 4. BARS B1/B2 (volume)
# ===========================================================================

cli_h1("Volume bars")
vb <- results |> select(player_id, season, week, opportunities, pred_vol) |>
  inner_join(vol_old, by = c("player_id", "season", "week"))

summ <- function(d, set) d |> summarise(set = set, n = n(),
  actual = mean(opportunities), bias_new = mean(opportunities - pred_vol),
  bias_old = mean(opportunities - pred_vol_old),
  rmse_new = sqrt(mean((opportunities - pred_vol)^2)),
  rmse_old = sqrt(mean((opportunities - pred_vol_old)^2)))
bars <- bind_rows(summ(vb, "all 1+ tgt (B1)"),
                  summ(vb |> filter(opportunities >= 3), "3+ tgt (B2)"),
                  summ(vb |> filter(opportunities < 3), "1-2 tgt"))
print(bars |> mutate(across(where(is.double), \(x) round(x, 3))))

# Post-hoc receipt (added after B2 ran, NOT a replacement bar): B2 subsets
# on the REALIZED outcome (3+ targets), which favors any model fit to that
# truncated sample. Same comparison split on ex-ante role instead -- the
# player's in-season trailing targets/game before the test week.
trail <- ft_ff |> arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  mutate(n_prior = row_number() - 1L, trail_tgt = lag(cummean(targets))) |>
  ungroup() |>
  distinct(player_id, season, week, .keep_all = TRUE) |>
  select(player_id, season, week, n_prior, trail_tgt)
exante <- vb |> left_join(trail, by = c("player_id", "season", "week")) |>
  mutate(role = case_when(n_prior < 2 ~ "<2 prior games",
                          trail_tgt >= 5 ~ "trailing 5+ tgt/g",
                          trail_tgt >= 3 ~ "trailing 3-5 tgt/g",
                          TRUE ~ "trailing <3 tgt/g")) |>
  group_by(role) |>
  summarise(n = n(), actual = mean(opportunities),
            bias_new = mean(opportunities - pred_vol), bias_old = mean(opportunities - pred_vol_old),
            rmse_new = sqrt(mean((opportunities - pred_vol)^2)),
            rmse_old = sqrt(mean((opportunities - pred_vol_old)^2)), .groups = "drop")
cli_h2("Post-hoc receipt: split on EX-ANTE role (all realized outcomes kept)")
print(exante |> mutate(across(where(is.double), \(x) round(x, 3))))
readr::write_csv(exante, "output/22a_te_floorfree_vol_exante.csv")

b1 <- abs(bars$bias_new[1]) <= 0.30
b2 <- abs(bars$bias_new[2]) <= 0.30 && bars$rmse_new[2] <= 1.05 * bars$rmse_old[2]
cli_alert_info("B1 (|bias| <= 0.30 on all 1+ rows): {if (b1) 'PASS' else 'FAIL'}")
cli_alert_info("B2 (3+ rows |bias| <= 0.30 and RMSE <= +5% vs OLD arm): {if (b2) 'PASS' else 'FAIL'}")

readr::write_csv(results, OUT_PATH)
readr::write_csv(old_res, OLD_OUT_PATH)
cli_alert_success("{OLD_OUT_PATH} ({nrow(old_res)} rows)")
readr::write_csv(bars, "output/22a_te_floorfree_vol_bars.csv")
cli_alert_success("{OUT_PATH} ({nrow(results)} rows)")
