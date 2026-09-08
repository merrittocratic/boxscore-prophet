# R/21n_fp_deployment_models.R
# S1 (shadow mode), step 1: the 10a analogue for the single-stage FP
# architecture. RB and WR ONLY -- TE stays two-stage (2026-09-08 decision:
# TE never got a single-stage arm built or graded during the D29 rebuild,
# so "swap 10a's RB/WR/TE portion" from the main plan's Rewiring section
# cannot be taken literally; 10c will run a MIXED architecture, TE/QB on
# R/10a's models regardless of MODEL_ARCH).
#
# This is R/21d_fp_single_stage_backtest.R's exact per-fold procedure
# (feature sets, tuning grid, conformal machinery -- all copied verbatim,
# not reimplemented) run ONE MORE TIME on ALL data, same "deployment is
# one more fold" framing R/10a_deployment_models.R uses for the two-stage
# models. Ship-decided arms (Steve, 2026-09-08, disclosed override --
# nothing passed the formal pre-registered gate): RB=floorfree, WR=base.
# Per-position env override mirrors R/21e's RECAL_ARM_RB/RECAL_ARM_WR
# idiom, in case that pairing is ever revisited.
#
# ARTIFACTS (consumed by R/10c_weekly_score.R's MODEL_ARCH=fp1 shadow path):
#   data/deploy_models_fp/<pos>_<point|vol>.txt   lgb.save text models
#   data/deployment_params_fp.rds                 features, params,
#                                                 conformal grid (K=11,
#                                                 signed, normalized),
#                                                 alpha, thresholds,
#                                                 trained_through stamp
#   output/21n_deploy_tune_log.csv                chosen hyperparams audit
#
# SINGLE-WRITER: like data/deployment_params.rds, these are deployment
# artifacts, not analysis output -- do not commit outside a coordinated
# ship pass. See feedback_single_writer_artifacts (and its 2026-09-08
# near-miss note on data/fp_recal_maps.rds, the reason R/21e no longer
# writes to a production path by default either).
#
# Usage: Rscript R/21n_fp_deployment_models.R
#   Env FP_ARM_RB / FP_ARM_WR: which R/21c training-table arm to deploy,
#     per position (default "floorfree" / "base" -- the ship decision)
#   Env DP_FP_OUT: deployment params rds path (default
#     "data/deployment_params_fp.rds")
#   Env DEPLOY_MODELS_FP_DIR: model .txt directory (default
#     "data/deploy_models_fp")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

set.seed(42)

FP_ARM <- c(
  RB = Sys.getenv("FP_ARM_RB", "floorfree"),
  WR = Sys.getenv("FP_ARM_WR", "base")
)
stopifnot(all(FP_ARM %in% c("base", "floorfree", "lagusage", "lagusage_pff", "ae", "weather")))

DP_FP_OUT           <- Sys.getenv("DP_FP_OUT", "data/deployment_params_fp.rds")
DEPLOY_MODELS_FP_DIR <- Sys.getenv("DEPLOY_MODELS_FP_DIR", "data/deploy_models_fp")

cli_h1("21n: FP deployment models (single-stage, RB/WR only)")
cli_alert_info("Arms: RB={FP_ARM[['RB']]} | WR={FP_ARM[['WR']]}")

# ===========================================================================
# PARAMETERS -- copied verbatim from R/21d_fp_single_stage_backtest.R
# (non-wide grid only; this is a deployment refit, not a grid search).
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
               WR = c(start = 15, boom = 20))

# ===========================================================================
# Feature sets -- identical to R/21d and R/10a_deployment_models.R.
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

VOL_FEATURES_MAP <- list(RB = RB_VOL_FEATURES, WR = WR_VOL_FEATURES)
EFF_FEATURES_MAP <- list(RB = RB_EFF_FEATURES, WR = WR_EFF_FEATURES)
POINT_FEATURES   <- lapply(names(VOL_FEATURES_MAP), function(p) {
  union(EFF_FEATURES_MAP[[p]], VOL_FEATURES_MAP[[p]])
}) |> setNames(names(VOL_FEATURES_MAP))

# ===========================================================================
# HELPERS -- conformal machinery copied verbatim from R/21d (note: this
# fit_power_alpha uses log(pmax(vol,1)), NOT R/10a's bare log(opp) --
# 21n mirrors 21d, the script that proved this specific recipe).
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

# Deployment fit/cal split: last CAL_FRAC of season-weeks across ALL data --
# identical helper to R/10a_deployment_models.R:278-287.
split_fit_cal <- function(ft) {
  sws      <- ft |> distinct(season, week) |> arrange(season, week)
  n_cal_sw <- max(1L, floor(CAL_FRAC * nrow(sws)))
  cal_sws  <- tail(sws, n_cal_sw)
  fit_sws  <- head(sws, nrow(sws) - n_cal_sw)
  list(
    fit = ft |> semi_join(fit_sws, by = c("season", "week")),
    cal = ft |> semi_join(cal_sws, by = c("season", "week"))
  )
}

trained_through <- function(ft) {
  ft |> summarise(season = max(season),
                  week = max(week[season == max(season)])) |> as.list()
}

dir.create(DEPLOY_MODELS_FP_DIR, showWarnings = FALSE, recursive = TRUE)
save_model <- function(mod, name) {
  path <- file.path(DEPLOY_MODELS_FP_DIR, paste0(name, ".txt"))
  lightgbm::lgb.save(mod, path)
  path
}

tune_rows <- list()

# ===========================================================================
# ONE-POSITION DEPLOYMENT FOLD -- factored out since RB/WR are now
# identical procedure (unlike 10a's eff*vol positions, which differ in
# mechanism/asymmetry; the single-stage point+aux-vol procedure does not).
# ===========================================================================

deploy_position <- function(position) {
  arm <- FP_ARM[[position]]
  train_path <- sprintf("data/fp_train_%s%s.rds", tolower(position),
                        switch(arm, floorfree = "_floorfree", lagusage = "_lagusage",
                               lagusage_pff = "_lagusage_pff", ae = "_ae", weather = "_weather", ""))
  cli_h1("{position} deployment fold [{arm}] ({train_path})")

  ft <- readRDS(train_path)
  sp <- split_fit_cal(ft)
  cli_alert_info("{position} rows: fit={nrow(sp$fit)} cal={nrow(sp$cal)}")

  point_feats <- POINT_FEATURES[[position]]
  vol_feats   <- VOL_FEATURES_MAP[[position]]

  X_fit_pt <- make_matrix(sp$fit, point_feats)
  X_cal_pt <- make_matrix(sp$cal, point_feats)
  X_fit_vl <- make_matrix(sp$fit, vol_feats)
  X_cal_vl <- make_matrix(sp$cal, vol_feats)

  best_pt <- tune_lgbm_component(X_fit_pt, sp$fit$fantasy_points_ppr, X_cal_pt, sp$cal$fantasy_points_ppr)
  best_vl <- tune_lgbm_component(X_fit_vl, as.numeric(sp$fit$opportunities), X_cal_vl, as.numeric(sp$cal$opportunities))

  tune_rows[[paste0(position, "_point")]] <<- tibble(
    position = position, component = "point", num_leaves = best_pt$num_leaves, lr = best_pt$lr,
    min_data_in_leaf = best_pt$min_data_in_leaf, rounds = best_pt$rounds,
    inner_rmse = round(best_pt$inner_rmse, 4)
  )
  tune_rows[[paste0(position, "_vol")]] <<- tibble(
    position = position, component = "vol", num_leaves = best_vl$num_leaves, lr = best_vl$lr,
    min_data_in_leaf = best_vl$min_data_in_leaf, rounds = best_vl$rounds,
    inner_rmse = round(best_vl$inner_rmse, 4)
  )

  pt_rounds <- max(REFIT_ROUNDS_MIN, best_pt$rounds)
  vl_rounds <- max(REFIT_ROUNDS_MIN, best_vl$rounds)

  mod_pt <- fit_lgbm_tuned(X_fit_pt, sp$fit$fantasy_points_ppr,
                           list(num_leaves = best_pt$num_leaves, learning_rate = best_pt$lr,
                                min_data_in_leaf = best_pt$min_data_in_leaf), pt_rounds)
  mod_vl <- fit_lgbm_tuned(X_fit_vl, as.numeric(sp$fit$opportunities),
                           list(num_leaves = best_vl$num_leaves, learning_rate = best_vl$lr,
                                min_data_in_leaf = best_vl$min_data_in_leaf), vl_rounds)

  pred_cal_pt <- predict(mod_pt, X_cal_pt)
  pred_cal_vl <- predict(mod_vl, X_cal_vl)

  resid_fp_cal <- sp$cal$fantasy_points_ppr - pred_cal_pt
  alpha        <- fit_power_alpha(pred_cal_vl, abs(resid_fp_cal))
  resid_norm   <- resid_fp_cal / pmax(pred_cal_vl, 1)^alpha
  qgrid        <- signed_quantile_grid(resid_norm, QLEVELS)
  names(qgrid) <- paste0("q", gsub("0\\.", "", sprintf("%.2f", QLEVELS)))

  cli_alert_success("{position} conformal: alpha={round(alpha, 3)} | qgrid[q50]={round(qgrid['q50'], 3)} | qgrid width q10-q90={round(qgrid['q90']-qgrid['q10'], 2)}")

  point_file <- save_model(mod_pt, paste0(tolower(position), "_point"))
  vol_file   <- save_model(mod_vl, paste0(tolower(position), "_vol"))

  list(
    trained_through = trained_through(ft),
    arm     = arm,
    point   = list(features = point_feats, model_file = point_file, params = best_pt),
    vol     = list(features = vol_feats,   model_file = vol_file,   params = best_vl),
    conformal = list(alpha = alpha, qgrid = qgrid, qlevels = QLEVELS),
    thresh  = THRESH[[position]],
    cal     = sp$cal  # returned for the verification block below, not saved to rds
  )
}

rb_deploy <- deploy_position("RB")
wr_deploy <- deploy_position("WR")

# ===========================================================================
# SAVE DEPLOYMENT PARAMS + AUDIT
# ===========================================================================

cli_h1("Save deployment artifacts")

strip_cal <- function(x) x[setdiff(names(x), "cal")]

deployment_params_fp <- list(
  built   = "21n_fp_deployment_models.R",
  qlevels = QLEVELS,
  rb      = strip_cal(rb_deploy),
  wr      = strip_cal(wr_deploy)
)

saveRDS(deployment_params_fp, DP_FP_OUT)

tune_log <- list_rbind(tune_rows)
readr::write_csv(tune_log, "output/21n_deploy_tune_log.csv")

cli_alert_success("{DP_FP_OUT} (SINGLE-WRITER: do not commit outside a coordinated ship pass)")
cli_alert_success("{DEPLOY_MODELS_FP_DIR}/ ({length(list.files(DEPLOY_MODELS_FP_DIR))} model files, SINGLE-WRITER)")
cli_alert_success("output/21n_deploy_tune_log.csv")

# ===========================================================================
# VERIFICATION: reload everything fresh and sanity-score the cal rows,
# same discipline as R/10a_deployment_models.R:517-536.
# ===========================================================================

cli_h1("Verification: fresh reload + sanity scoring")

dp    <- readRDS(DP_FP_OUT)
cals  <- list(rb = rb_deploy$cal, wr = wr_deploy$cal)
for (pos in c("rb", "wr")) {
  for (cmp in c("point", "vol")) {
    m <- lightgbm::lgb.load(dp[[pos]][[cmp]]$model_file)
    X <- make_matrix(cals[[pos]], dp[[pos]][[cmp]]$features)
    p <- predict(m, X)
    stopifnot(all(is.finite(p)))
    cli_alert_success("{pos}_{cmp}: reload + predict ok ({length(p)} rows, mean={round(mean(p), 3)})")
  }
}

cli_h1("Step 21n complete -- RB trained through {dp$rb$trained_through$season}-W{dp$rb$trained_through$week}, WR trained through {dp$wr$trained_through$season}-W{dp$wr$trained_through$week}")
