# R/21d_fp_single_stage_backtest.R
# Stage C, step 1 of the D29 single-stage rebuild: the 204-fold walk-forward
# that replaces "predict efficiency, predict volume, multiply" with one
# LightGBM predicting fantasy_points_ppr directly, plus an AUXILIARY volume
# head (unchanged target, unchanged features) that is trained and predicted
# but NEVER multiplied into the point estimate -- it exists only to scale
# uncertainty and feed the recal strata/streamer board downstream.
#
# Fold loop, split contract, leakage assertions, tuning grid, and refit
# logic are lifted verbatim from R/archive/03a_v2_lgbm_tuned.R:290-330 (the
# RB bake-off winner) -- same 32-combo grid, same CAL_FRAC=0.20 inner
# holdout, same two leakage cli_abort()s. What's different from 03a_v2:
#   - target is fantasy_points_ppr, not epa_per_opp_obs -- no multiplication
#   - point-head feature set is the UNION of the deployed EFF and VOL
#     feature vectors (R/10a_deployment_models.R) for that position, since
#     a single-stage model isn't forced to isolate a per-opportunity
#     efficiency estimate -- it can use everything at once
#   - uncertainty scaling fits alpha on the auxiliary head's OWN cal-fold
#     prediction (pred_vol), not observed cal opportunities -- this closes
#     the documented 10a:15-21 train/serve seam by construction rather than
#     retrofitting it after the fact (R/06b0_predvol_rescale.R existed only
#     because the old pipeline fit on observed volume and served on
#     predicted volume; fitting on predicted volume here removes the reason
#     that seam existed)
#   - probability chain is DIRECT CONFORMAL CDF INVERSION, not Monte Carlo:
#     R/06b_fp_simulation.R's copula existed to couple EPA-error to
#     volume-error, which only made sense when FP was a function of TWO
#     correlated random components (EPA x volume). A single-stage model has
#     ONE random component (the FP residual itself), so that reason is
#     gone. A K=11 signed conformal quantile grid (vs the old 7-point
#     symmetric-then-asymmetric split) is inverted directly via linear
#     interpolation (extrapolating the outermost segment's slope beyond the
#     grid, same spirit as 06b's rule=2) to get p_start/p_boom -- zero
#     sampling noise, deterministic reruns.
#
# DEFERRED to a later increment (not silently skipped -- noted so a future
# session doesn't assume this is finished):
#   - tier-conditional quantile sets (the old residual-pool mechanism, which
#     varied SHAPE not just width by volume tier) -- this version uses one
#     pooled signed quantile set per fold, not tier-conditional
#   - the alpha-on-pred-vol vs alpha-on-observed-opp A/B itself -- this
#     version adopts pred-vol directly as the recommended fix rather than
#     running both and comparing; if the resulting coverage looks wrong,
#     revisit before trusting downstream calibration numbers
#   - recalibration (R/21e) -- this script's p_start/p_boom are RAW,
#     pre-recalibration probabilities, comparable to the "raw" columns
#     3A-era scripts always reported before Platt/isotonic fitting
#
# Usage: Rscript R/21d_fp_single_stage_backtest.R <RB|WR|TE> [base|floorfree]
#   Env FOLD_LIMIT: run only the last N folds (smoke-test before a full run)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else cli_abort("Usage: Rscript R/21d_fp_single_stage_backtest.R <RB|WR|TE> [base|floorfree]")
ARM      <- if (length(args) >= 2) args[2] else "base"
stopifnot(POSITION %in% c("RB", "WR", "TE"), ARM %in% c("base", "floorfree"))
FOLD_LIMIT <- as.integer(Sys.getenv("FOLD_LIMIT", NA))

# ===========================================================================
# PARAMETERS -- identical to 03a_v2 (grid, split, fixed params frozen)
# ===========================================================================

CAL_FRAC <- 0.20

TUNE_GRID <- expand.grid(
  num_leaves       = c(7L, 15L, 31L, 63L),
  min_data_in_leaf = c(10L, 20L, 50L, 100L),
  lr               = c(0.02, 0.05)
)

INNER_MAX_ROUNDS <- 500L
INNER_EARLY_STOP <- 20L
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

ALPHA_LO       <- 0.20
ALPHA_HI       <- 0.90
ALPHA_FALLBACK <- 0.50

QLEVELS <- c(0.02, 0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.80, 0.90, 0.95, 0.98)

THRESH <- list(RB = c(start = 15, boom = 20),
               WR = c(start = 15, boom = 20),
               TE = c(start = 12, boom = 17))

# ===========================================================================
# Feature sets -- the deployed EFF/VOL vectors from R/10a_deployment_models.R,
# copied verbatim (each stage script stays self-contained, repo convention).
# Point head trains on the UNION -- no eff/vol split.
# ===========================================================================

VEGAS_FEATURES <- c("team_spread", "implied_total")

RB_EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_rush_epa_adj", "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int",
  VEGAS_FEATURES
)
RB_INJURY_FEATURES <- c(
  "own_q_int", "own_practice_int", "weeks_missed", "return_from_absence",
  "above_new_out_share", "above_q_share", "above_long_out_share"
)
RB_VOL_FEATURES <- c(
  "wt_carry_share", "wt_target_share", "wt_snap_share", "wt_team_total_plays",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far",
  "baseline_carry_share", "baseline_target_share", "baseline_snap_share",
  "baseline_team_total_plays",
  RB_INJURY_FEATURES
)
WR_EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_air_yards_per_target",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int",
  VEGAS_FEATURES
)
WR_VOL_FEATURES <- c(
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_team_total_plays",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far",
  "baseline_target_share", "baseline_air_yards_share", "baseline_snap_share",
  "baseline_team_total_plays"
)
TE_EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_air_yards_per_target",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int",
  VEGAS_FEATURES
)
TE_VOL_FEATURES <- c(
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_tgt_per_snap",
  "wt_team_total_plays",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far",
  "baseline_target_share", "baseline_air_yards_share", "baseline_snap_share",
  "baseline_tgt_per_snap", "baseline_team_total_plays"
)

VOL_FEATURES_MAP <- list(RB = RB_VOL_FEATURES, WR = WR_VOL_FEATURES, TE = TE_VOL_FEATURES)
EFF_FEATURES_MAP <- list(RB = RB_EFF_FEATURES, WR = WR_EFF_FEATURES, TE = TE_EFF_FEATURES)
POINT_FEATURES   <- lapply(names(VOL_FEATURES_MAP), function(p) {
  union(EFF_FEATURES_MAP[[p]], VOL_FEATURES_MAP[[p]])
}) |> setNames(names(VOL_FEATURES_MAP))

# ===========================================================================
# HELPERS -- conformal machinery, generalized to a K=11 signed grid
# ===========================================================================

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

signed_quantile_grid <- function(resid_signed, probs) {
  vapply(probs, function(p) conformal_q_signed(resid_signed, p), numeric(1))
}

fit_power_alpha <- function(vol, raw_resid) {
  df  <- data.frame(log_vol = log(pmax(vol, 1)), log_resid = log(raw_resid + 1e-8))
  fit <- tryCatch(lm(log_resid ~ log_vol, data = df), error = function(e) NULL)
  if (is.null(fit)) return(ALPHA_FALLBACK)
  alpha <- unname(coef(fit)["log_vol"])
  if (!is.finite(alpha)) return(ALPHA_FALLBACK)
  max(ALPHA_LO, min(ALPHA_HI, alpha))
}

# Row-wise survival function P(Y >= t) from a monotone per-row quantile grid,
# via linear interpolation between the two bracketing quantile columns and
# linear extrapolation of the outermost segment's slope beyond the grid
# (never a flat clamp -- same spirit as R/06b_fp_simulation.R's rule=2, but
# a real slope instead of a boundary constant).
p_at_least <- function(Qmat, probs, t) {
  n <- nrow(Qmat)
  out <- numeric(n)
  k <- length(probs)
  for (i in seq_len(n)) {
    q <- Qmat[i, ]
    if (t <= q[1]) {
      slope <- (probs[2] - probs[1]) / (q[2] - q[1])
      cdf <- probs[1] + slope * (t - q[1])
    } else if (t >= q[k]) {
      slope <- (probs[k] - probs[k - 1]) / (q[k] - q[k - 1])
      cdf <- probs[k] + slope * (t - q[k])
    } else {
      cdf <- approx(q, probs, xout = t)$y
    }
    out[i] <- 1 - cdf
  }
  pmin(pmax(out, 0), 1)
}

make_matrix <- function(df, features) df |> select(all_of(features)) |> as.matrix()

tune_lgbm_component <- function(X_fit, y_fit, X_val, y_val) {
  keep   <- !is.na(y_fit)
  dtrain <- lgb.Dataset(X_fit[keep, , drop = FALSE], label = y_fit[keep])
  best_rmse <- Inf; best_row <- NULL; best_rounds <- 100L
  for (i in seq_len(nrow(TUNE_GRID))) {
    params <- c(LGBM_FIXED, list(
      num_leaves       = TUNE_GRID$num_leaves[i],
      learning_rate    = TUNE_GRID$lr[i],
      min_data_in_leaf = TUNE_GRID$min_data_in_leaf[i]
    ))
    dval <- lgb.Dataset(X_val, label = y_val, reference = dtrain)
    mod <- lgb.train(params = params, data = dtrain, nrounds = INNER_MAX_ROUNDS,
                     valids = list(val = dval), early_stopping_rounds = INNER_EARLY_STOP,
                     verbose = -1L)
    val_rmse <- sqrt(mean((y_val - predict(mod, X_val))^2, na.rm = TRUE))
    if (val_rmse < best_rmse) {
      best_rmse <- val_rmse; best_rounds <- mod$best_iter; best_row <- TUNE_GRID[i, ]
    }
  }
  list(num_leaves = best_row$num_leaves, lr = best_row$lr,
       min_data_in_leaf = best_row$min_data_in_leaf,
       rounds = best_rounds, inner_rmse = best_rmse)
}

fit_lgbm_tuned <- function(X, y, params_list, n_rounds) {
  keep   <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = c(LGBM_FIXED, params_list), data = dtrain, nrounds = n_rounds, verbose = -1L)
}

# ===========================================================================
# LOAD
# ===========================================================================

train_path <- sprintf("data/fp_train_%s%s.rds", tolower(POSITION),
                      if (ARM == "floorfree") "_floorfree" else "")
cli_h1("21d: single-stage FP backtest -- {POSITION} [{ARM}] ({train_path})")

ft       <- readRDS(train_path)
fold_map <- readRDS("data/fold_map.rds")
if (!is.na(FOLD_LIMIT)) {
  fold_map <- tail(fold_map, FOLD_LIMIT)
  cli_alert_warning("FOLD_LIMIT set -- running only the last {FOLD_LIMIT} folds (smoke test, not a full backtest)")
}

point_feats <- POINT_FEATURES[[POSITION]]
vol_feats   <- VOL_FEATURES_MAP[[POSITION]]
thresh      <- THRESH[[POSITION]]

cli_alert_info("Train table: {nrow(ft)} rows | point features: {length(point_feats)} | vol features: {length(vol_feats)} | folds: {nrow(fold_map)}")

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

fold_results <- vector("list", nrow(fold_map))
tune_log     <- vector("list", nrow(fold_map))
skipped      <- 0L

for (f in seq_len(nrow(fold_map))) {
  t0 <- proc.time()[["elapsed"]]

  test_season <- fold_map$test_season[f]
  test_week   <- fold_map$test_week[f]

  test_data  <- ft |> filter(season == test_season, week == test_week)
  if (nrow(test_data) == 0L) { skipped <- skipped + 1L; next }

  train_data <- ft |> filter(season < test_season | (season == test_season & week < test_week))
  if (nrow(train_data) < 50L) { skipped <- skipped + 1L; next }

  overlap <- intersect(paste(train_data$season, train_data$week),
                       paste(test_data$season, test_data$week))
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
  if (nrow(fit_data) < 20L || nrow(cal_data) < 10L) { skipped <- skipped + 1L; next }

  X_fit_pt <- make_matrix(fit_data, point_feats)
  X_cal_pt <- make_matrix(cal_data, point_feats)
  X_fit_vl <- make_matrix(fit_data, vol_feats)
  X_cal_vl <- make_matrix(cal_data, vol_feats)

  best_pt <- tune_lgbm_component(X_fit_pt, fit_data$fantasy_points_ppr, X_cal_pt, cal_data$fantasy_points_ppr)
  best_vl <- tune_lgbm_component(X_fit_vl, as.numeric(fit_data$opportunities), X_cal_vl, as.numeric(cal_data$opportunities))

  tune_log[[f]] <- tibble(
    fold = f, pt_num_leaves = best_pt$num_leaves, pt_lr = best_pt$lr,
    pt_min_node = best_pt$min_data_in_leaf, pt_rounds = best_pt$rounds,
    pt_inner_rmse = round(best_pt$inner_rmse, 4),
    vl_num_leaves = best_vl$num_leaves, vl_lr = best_vl$lr,
    vl_min_node = best_vl$min_data_in_leaf, vl_rounds = best_vl$rounds,
    vl_inner_rmse = round(best_vl$inner_rmse, 4)
  )

  pt_rounds <- max(REFIT_ROUNDS_MIN, best_pt$rounds)
  vl_rounds <- max(REFIT_ROUNDS_MIN, best_vl$rounds)

  mod_pt <- fit_lgbm_tuned(X_fit_pt, fit_data$fantasy_points_ppr,
                           list(num_leaves = best_pt$num_leaves, learning_rate = best_pt$lr,
                                min_data_in_leaf = best_pt$min_data_in_leaf), pt_rounds)
  mod_vl <- fit_lgbm_tuned(X_fit_vl, as.numeric(fit_data$opportunities),
                           list(num_leaves = best_vl$num_leaves, learning_rate = best_vl$lr,
                                min_data_in_leaf = best_vl$min_data_in_leaf), vl_rounds)

  pred_cal_pt  <- predict(mod_pt, X_cal_pt)
  pred_cal_vl  <- predict(mod_vl, X_cal_vl)
  X_test_pt    <- make_matrix(test_data, point_feats)
  X_test_vl    <- make_matrix(test_data, vol_feats)
  pred_test_pt <- predict(mod_pt, X_test_pt)
  pred_test_vl <- predict(mod_vl, X_test_vl)

  # Uncertainty scaling: alpha fit on the AUXILIARY head's own cal-fold
  # prediction (pred_cal_vl), not observed cal opportunities -- closes the
  # 10a:15-21 train/serve seam by construction. Signed (not symmetric)
  # quantile grid captures whatever skew is actually present, positionally,
  # without a separate skew-conditional branch.
  resid_fp_cal <- cal_data$fantasy_points_ppr - pred_cal_pt
  alpha        <- fit_power_alpha(pred_cal_vl, abs(resid_fp_cal))
  resid_norm   <- resid_fp_cal / pmax(pred_cal_vl, 1)^alpha
  qgrid        <- signed_quantile_grid(resid_norm, QLEVELS)

  scale_test <- pmax(pred_test_vl, 1)^alpha
  Q_test     <- outer(scale_test, qgrid)
  Q_test     <- sweep(Q_test, 1, pred_test_pt, "+")
  colnames(Q_test) <- paste0("q", gsub("0\\.", "", sprintf("%.2f", QLEVELS)))

  p_start <- p_at_least(Q_test, QLEVELS, thresh["start"])
  p_boom  <- p_at_least(Q_test, QLEVELS, thresh["boom"])

  fold_results[[f]] <- test_data |>
    select(player_id, season, week, opportunities, fantasy_points_ppr) |>
    mutate(pred_fp = pred_test_pt, pred_vol = pred_test_vl, alpha_fold = alpha,
           p_start_raw = p_start, p_boom_raw = p_boom, fold = f) |>
    bind_cols(as_tibble(Q_test))

  t1 <- proc.time()[["elapsed"]]
  cli_alert_info(
    "Fold {sprintf('%03d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | alpha={round(alpha,3)} | pt leaves={best_pt$num_leaves} lr={best_pt$lr} | {round(t1-t0,1)}s"
  )
}

results  <- bind_rows(fold_results)
tune_all <- bind_rows(tune_log)

cli_h1("Harness Integrity Report")
cli_alert_info("Folds run: {nrow(fold_map) - skipped} / {nrow(fold_map)} ({skipped} skipped: 0 test rows or too-small train)")
cli_alert_success("Rows scored: {nrow(results)}")
na_pt <- sum(is.na(results$pred_fp)); na_vl <- sum(is.na(results$pred_vol))
if (na_pt + na_vl == 0L) cli_alert_success("Zero NA predictions") else cli_warn("NA predictions: pt={na_pt}, vl={na_vl}")
cli_alert_info("alpha_fold range [{round(min(results$alpha_fold),3)}, {round(max(results$alpha_fold),3)}] | median {round(median(results$alpha_fold),3)}")

cli_h1("Quick point-estimate correlation check (pred_fp vs realized FP)")
cli_alert_info("Pearson r: {round(cor(results$pred_fp, results$fantasy_points_ppr, use='complete.obs'),4)} | Spearman: {round(cor(results$pred_fp, results$fantasy_points_ppr, method='spearman', use='complete.obs'),4)}")

# ===========================================================================
# SAVE
# ===========================================================================

dir.create("output", showWarnings = FALSE, recursive = TRUE)
out_prefix <- sprintf("output/21d_%s_%s", tolower(POSITION), ARM)
write_csv(results,  paste0(out_prefix, "_fold_predictions.csv"))
write_csv(tune_all, paste0(out_prefix, "_tune_log.csv"))

cli_alert_success("{out_prefix}_fold_predictions.csv ({nrow(results)} rows)")
cli_alert_success("{out_prefix}_tune_log.csv")
cli_h1("21d complete -- {POSITION} [{ARM}]")
