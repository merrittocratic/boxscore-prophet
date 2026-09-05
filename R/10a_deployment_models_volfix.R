# R/10a_deployment_models_volfix.R
# CANDIDATE clone of R/10a_deployment_models.R for the volume-carryforward
# fix (branch wr-rb-vol-carryforward-fix). NOT the production script.
#
# Differences from R/10a_deployment_models.R (that file is untouched):
#   (1) this header;
#   (2) RB_VOL_FEATURES/WR_VOL_FEATURES/TE_VOL_FEATURES gain the baseline_*
#       prior-season carryforward columns, matching exactly what
#       04c_wr_asymmetric_conformal_volfix.R / 11c_rb_injury_volfix.R /
#       12c_te_asymmetric_conformal_volfix.R already validated:
#         RB:  baseline_carry_share, baseline_target_share,
#              baseline_snap_share, baseline_team_total_plays
#         WR:  baseline_target_share, baseline_air_yards_share,
#              baseline_snap_share, baseline_team_total_plays
#         TE:  baseline_target_share, baseline_air_yards_share,
#              baseline_snap_share, baseline_tgt_per_snap,
#              baseline_team_total_plays
#       (baseline_epa_per_opp was already in EFF_FEATURES upstream in the
#       real 10a -- unchanged here.)
#   (3) Sys.getenv-based override seam for every OUTPUT path (models dir,
#       deployment_params.rds, tune log), same pattern as the seams already
#       added to 06b0/06b/06c/12d0/12d/12e_te (2026-08-30 volfix recal
#       refit commit). Defaults point at *_volfix paths so a bare `Rscript
#       R/10a_deployment_models_volfix.R` NEVER touches the real
#       data/deploy_models/ or data/deployment_params.rds -- promotion to
#       those real paths is a separate, explicitly-confirmed step.
#   (4) Additional cal-fold export: after each position's deployment fold
#       trains, the SAME held-out cal split (genuinely out-of-sample for
#       these exact model objects -- never touched during fit) is written
#       out in the same fold_predictions.csv schema 04c/11c/12c/13e emit,
#       so the recal chain (06b0/06b/06c, 12d0/12d/12e_te) can be refit
#       against the ACTUAL candidate deployed models, not against the
#       walk-forward backtest models from earlier _volfix commits. This is
#       new -- the real 10a has no equivalent output.
#
# Everything else (training procedure, tuning grid, conformal machinery,
# Vegas join, verification block) is copied verbatim from 10a.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

set.seed(42)

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
COVERAGES      <- c(0.50, 0.80, 0.90)

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

# ===========================================================================
# OUTPUT PATH OVERRIDE SEAM (2026-08-31, volfix deployment candidate)
# ===========================================================================

MODELS_DIR      <- Sys.getenv("DEPLOY_MODELS_DIR",     "data/deploy_models_volfix")
PARAMS_OUT      <- Sys.getenv("DEPLOYMENT_PARAMS_OUT",  "data/deployment_params_volfix.rds")
TUNE_LOG_OUT    <- Sys.getenv("DEPLOY_TUNE_LOG_OUT",    "output/10a_deploy_tune_log_volfix.csv")
CAL_FOLD_PREFIX <- Sys.getenv("DEPLOY_CAL_FOLD_PREFIX", "output/10a_volfix")

cli_alert_info("Models dir: {MODELS_DIR} | params out: {PARAMS_OUT} | tune log: {TUNE_LOG_OUT} | cal-fold prefix: {CAL_FOLD_PREFIX}")

# ===========================================================================
# FEATURE SETS -- copied from 10a, VOL_FEATURES extended with baseline_*
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

# VOLFIX (2026-08-31): baseline_* prior-season carryforward added, matching
# 11c_rb_injury_volfix.R exactly (fixes NA volume features at Week 1 for
# every player, rookie or veteran).
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
# VOLFIX (2026-08-31): baseline_* added, matching
# 04c_wr_asymmetric_conformal_volfix.R exactly.
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
# VOLFIX (2026-08-31): baseline_* added, matching
# 12c_te_asymmetric_conformal_volfix.R exactly (TE has one more member than
# WR: baseline_tgt_per_snap, mirroring TE's extra wt_tgt_per_snap).
TE_VOL_FEATURES <- c(
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_tgt_per_snap",
  "wt_team_total_plays",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far",
  "baseline_target_share", "baseline_air_yards_share", "baseline_snap_share",
  "baseline_tgt_per_snap", "baseline_team_total_plays"
)

QB_COMPONENTS <- list(
  pass_eff  = list(feats = c(
    "prior_pass_epa_per_db", "baseline_pass_epa_per_db", "rolling_pass_epa_per_db",
    "form_residual", "is_cold_start_int", "draft_tier_int",
    "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
    "wt_snap_share", "games_played_so_far", "def_used_fallback_int",
    VEGAS_FEATURES
  ), y = "pass_epa_per_db_obs"),
  db_vol    = list(feats = c(
    "wt_dropbacks", "wt_team_total_plays", "wt_team_pass_rate",
    "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
    "draft_tier_int", "is_cold_start_int", "games_played_so_far"
  ), y = "dropbacks"),
  carry_vol = list(feats = c(
    "wt_carries", "wt_carry_share", "prior_carries_pg", "wt_team_total_plays",
    "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
  ), y = "carries"),
  rush_dir  = list(feats = c(
    "wt_rush_epa_pg", "wt_carries", "wt_carry_share",
    "prior_rush_epa_pg", "prior_carries_pg",
    "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
  ), y = "rush_epa")
)

# ===========================================================================
# HELPERS -- identical machinery to 10a / the backtest scripts
# ===========================================================================

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

q3 <- function(abs_resid) {
  c(conformal_q(abs_resid, 0.50),
    conformal_q(abs_resid, 0.80),
    conformal_q(abs_resid, 0.90))
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

tune_lgbm_component <- function(X_fit, y_fit, X_val, y_val) {
  keep <- !is.na(y_fit)
  dtrain <- lgb.Dataset(X_fit[keep, , drop = FALSE], label = y_fit[keep])

  best_rmse   <- Inf
  best_row    <- NULL
  best_rounds <- 100L

  for (i in seq_len(nrow(TUNE_GRID))) {
    params <- c(
      LGBM_FIXED,
      list(
        num_leaves       = TUNE_GRID$num_leaves[i],
        learning_rate    = TUNE_GRID$lr[i],
        min_data_in_leaf = TUNE_GRID$min_data_in_leaf[i]
      )
    )
    dval <- lgb.Dataset(X_val, label = y_val, reference = dtrain)
    mod <- lgb.train(
      params                = params,
      data                  = dtrain,
      nrounds               = INNER_MAX_ROUNDS,
      valids                = list(val = dval),
      early_stopping_rounds = INNER_EARLY_STOP,
      verbose               = -1L
    )
    preds    <- predict(mod, X_val)
    val_rmse <- sqrt(mean((y_val - preds)^2, na.rm = TRUE))
    if (val_rmse < best_rmse) {
      best_rmse   <- val_rmse
      best_rounds <- mod$best_iter
      best_row    <- TUNE_GRID[i, ]
    }
  }

  list(
    num_leaves       = best_row$num_leaves,
    lr               = best_row$lr,
    min_data_in_leaf = best_row$min_data_in_leaf,
    rounds           = max(REFIT_ROUNDS_MIN, best_rounds),
    inner_rmse       = best_rmse
  )
}

fit_lgbm_tuned <- function(X, y, best) {
  keep   <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(
    params  = c(LGBM_FIXED, list(num_leaves = best$num_leaves,
                                 learning_rate = best$lr,
                                 min_data_in_leaf = best$min_data_in_leaf)),
    data    = dtrain,
    nrounds = best$rounds,
    verbose = -1L
  )
}

# Deployment fit/cal split: last CAL_FRAC of season-weeks across ALL data
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

# Tune + refit one component; returns model, cal predictions, tune record
train_component <- function(fit_data, cal_data, feats, y_col, label) {
  X_fit <- make_matrix(fit_data, feats)
  X_cal <- make_matrix(cal_data, feats)
  y_fit <- as.numeric(fit_data[[y_col]])
  y_cal <- as.numeric(cal_data[[y_col]])

  best <- tune_lgbm_component(X_fit, y_fit, X_cal, y_cal)
  mod  <- fit_lgbm_tuned(X_fit, y_fit, best)

  cli_alert_info(
    "{label}: leaves={best$num_leaves} minnode={best$min_data_in_leaf} lr={best$lr} rounds={best$rounds} inner_rmse={round(best$inner_rmse, 4)}"
  )
  list(model = mod, pred_cal = predict(mod, X_cal), best = best,
       tune = tibble(component = label,
                     num_leaves = best$num_leaves, lr = best$lr,
                     min_data_in_leaf = best$min_data_in_leaf,
                     rounds = best$rounds,
                     inner_rmse = round(best$inner_rmse, 4)))
}

dir.create(MODELS_DIR, showWarnings = FALSE, recursive = TRUE)
save_model <- function(mod, name) {
  path <- file.path(MODELS_DIR, paste0(name, ".txt"))
  lightgbm::lgb.save(mod, path)
  path
}

trained_through <- function(ft) {
  ft |> summarise(season = max(season),
                  week = max(week[season == max(season)])) |> as.list()
}

# Vegas sidecar join (rung 2): opener lines keyed (game_id, posteam).
vegas_lines <- readRDS("data/vegas_open_lines.rds")
join_vegas <- function(ft) {
  out <- ft |> left_join(vegas_lines, by = c("game_id", "posteam"))
  cli_alert_info("Vegas join: {sum(!is.na(out$team_spread))} of {nrow(out)} rows with opener lines")
  out
}

# ---------------------------------------------------------------------------
# Cal-fold export helpers (NEW in the volfix clone): identical construction
# to build_intervals / build_asym_intervals in 11c/04c/12c, applied ONCE to
# the deployment cal split (genuinely out-of-sample for these exact model
# objects) instead of once per backtest fold. scale uses the cal split's OWN
# observed opportunities (same convention as "test_opp" in the backtest
# folds) -- i.e. OBS-VOL scaling, matching what 06b0 expects as input before
# it rescales to PRED-VOL.
# ---------------------------------------------------------------------------

build_intervals_sym <- function(pred, qs, suffix) {
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

build_intervals_asym <- function(pred, qset, suffix, scale = 1) {
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

tune_rows <- list()

# ===========================================================================
# RB DEPLOYMENT FOLD (03a-v2 procedure)
# ===========================================================================

cli_h1("RB deployment fold (03a-v2 procedure, VOLFIX candidate)")

rb_ft <- readRDS("data/rb_feature_table.rds") |>
  encode_features() |>
  left_join(readRDS("data/injury_states_rb.rds"),
            by = c("player_id", "season", "week")) |>
  join_vegas()
stopifnot(!any(is.na(rb_ft$own_practice_int[!is.na(rb_ft$player_id)])))
rb_sp <- split_fit_cal(rb_ft)
cli_alert_info("RB rows: fit={nrow(rb_sp$fit)} cal={nrow(rb_sp$cal)}")

rb_eff <- train_component(rb_sp$fit, rb_sp$cal, RB_EFF_FEATURES, "epa_per_opp_obs", "rb_eff")
rb_vol <- train_component(rb_sp$fit, rb_sp$cal, RB_VOL_FEATURES, "opportunities",   "rb_vol")
tune_rows <- c(tune_rows, list(rb_eff$tune, rb_vol$tune))

# Symmetric component conformal + Mechanism A combined
rb_qs_eff <- q3(abs(rb_sp$cal$epa_per_opp_obs - rb_eff$pred_cal))
rb_qs_vol <- q3(abs(as.numeric(rb_sp$cal$opportunities) - rb_vol$pred_cal))

rb_pred_cal_tot <- rb_eff$pred_cal * rb_vol$pred_cal
rb_raw_resid    <- abs(rb_sp$cal$total_epa - rb_pred_cal_tot)
rb_cal_opp      <- as.numeric(rb_sp$cal$opportunities)
rb_alpha        <- fit_power_alpha(rb_cal_opp, rb_raw_resid)
rb_q_norm       <- q3(rb_raw_resid / rb_cal_opp^rb_alpha)

cli_alert_success("RB conformal: alpha={round(rb_alpha, 3)} | 80% widths: eff={round(2*rb_qs_eff[2], 2)} vol={round(2*rb_qs_vol[2], 2)}")

save_model(rb_eff$model, "rb_eff")
save_model(rb_vol$model, "rb_vol")

# --- Cal-fold export (NEW): OBS-VOL scaled tot arms, self-referential scale
rb_cal_fold <- rb_sp$cal |>
  select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
  bind_cols(
    build_intervals_sym(rb_eff$pred_cal, rb_qs_eff, "eff"),
    build_intervals_sym(rb_vol$pred_cal, rb_qs_vol, "vol")
  ) |>
  mutate(alpha_fold = rb_alpha)
rb_hw50 <- rb_q_norm[1] * rb_cal_opp^rb_alpha
rb_hw80 <- rb_q_norm[2] * rb_cal_opp^rb_alpha
rb_hw90 <- rb_q_norm[3] * rb_cal_opp^rb_alpha
rb_cal_fold <- rb_cal_fold |>
  mutate(
    pred_tot = rb_pred_cal_tot,
    lo_50_tot = pred_tot - rb_hw50, hi_50_tot = pred_tot + rb_hw50,
    lo_80_tot = pred_tot - rb_hw80, hi_80_tot = pred_tot + rb_hw80,
    lo_90_tot = pred_tot - rb_hw90, hi_90_tot = pred_tot + rb_hw90
  )
readr::write_csv(rb_cal_fold, paste0(CAL_FOLD_PREFIX, "_rb_cal_fold_predictions.csv"))
cli_alert_success("{paste0(CAL_FOLD_PREFIX, '_rb_cal_fold_predictions.csv')} ({nrow(rb_cal_fold)} rows)")

# ===========================================================================
# WR DEPLOYMENT FOLD (04b tuning + 04c asymmetric conformal)
# ===========================================================================

cli_h1("WR deployment fold (04b tuning + 04c asymmetric conformal, VOLFIX candidate)")

wr_ft <- readRDS("data/wr_feature_table.rds") |> encode_features() |> join_vegas()
wr_sp <- split_fit_cal(wr_ft)
cli_alert_info("WR rows: fit={nrow(wr_sp$fit)} cal={nrow(wr_sp$cal)}")

wr_eff <- train_component(wr_sp$fit, wr_sp$cal, WR_EFF_FEATURES, "epa_per_opp_obs", "wr_eff")
wr_vol <- train_component(wr_sp$fit, wr_sp$cal, WR_VOL_FEATURES, "opportunities",   "wr_vol")
tune_rows <- c(tune_rows, list(wr_eff$tune, wr_vol$tune))

# Asymmetric signed-residual conformal (04c)
wr_qset_eff <- signed_quantile_set(wr_sp$cal$epa_per_opp_obs - wr_eff$pred_cal)
wr_qset_vol <- signed_quantile_set(as.numeric(wr_sp$cal$opportunities) - wr_vol$pred_cal)

wr_pred_cal_tot <- wr_eff$pred_cal * wr_vol$pred_cal
wr_resid_tot    <- wr_sp$cal$total_epa - wr_pred_cal_tot
wr_cal_opp      <- as.numeric(wr_sp$cal$opportunities)
wr_alpha        <- fit_power_alpha(wr_cal_opp, abs(wr_resid_tot))
wr_qset_tot     <- signed_quantile_set(wr_resid_tot / wr_cal_opp^wr_alpha)

cli_alert_success("WR conformal: alpha={round(wr_alpha, 3)} | tot 80% asym: lo={round(wr_qset_tot$lo[2], 3)} hi={round(wr_qset_tot$hi[2], 3)} (right skew: {wr_qset_tot$hi[2] > abs(wr_qset_tot$lo[2])})")

save_model(wr_eff$model, "wr_eff")
save_model(wr_vol$model, "wr_vol")

# --- Cal-fold export (NEW): asymmetric OBS-VOL scaled tot arms
wr_cal_fold <- wr_sp$cal |>
  select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
  bind_cols(
    build_intervals_asym(wr_eff$pred_cal, wr_qset_eff, "eff"),
    build_intervals_asym(wr_vol$pred_cal, wr_qset_vol, "vol"),
    build_intervals_asym(wr_pred_cal_tot, wr_qset_tot, "tot", scale = wr_cal_opp^wr_alpha)
  ) |>
  mutate(alpha_fold = wr_alpha)
readr::write_csv(wr_cal_fold, paste0(CAL_FOLD_PREFIX, "_wr_cal_fold_predictions.csv"))
cli_alert_success("{paste0(CAL_FOLD_PREFIX, '_wr_cal_fold_predictions.csv')} ({nrow(wr_cal_fold)} rows)")

# ===========================================================================
# TE DEPLOYMENT FOLD (12b tuning + 12c asymmetric conformal -- WR procedure)
# ===========================================================================

cli_h1("TE deployment fold (12b tuning + 12c asymmetric conformal, VOLFIX candidate)")

te_ft <- readRDS("data/te_feature_table.rds") |> encode_features() |> join_vegas()
te_sp <- split_fit_cal(te_ft)
cli_alert_info("TE rows: fit={nrow(te_sp$fit)} cal={nrow(te_sp$cal)}")

te_eff <- train_component(te_sp$fit, te_sp$cal, TE_EFF_FEATURES, "epa_per_opp_obs", "te_eff")
te_vol <- train_component(te_sp$fit, te_sp$cal, TE_VOL_FEATURES, "opportunities",   "te_vol")
tune_rows <- c(tune_rows, list(te_eff$tune, te_vol$tune))

# Asymmetric signed-residual conformal (12c)
te_qset_eff <- signed_quantile_set(te_sp$cal$epa_per_opp_obs - te_eff$pred_cal)
te_qset_vol <- signed_quantile_set(as.numeric(te_sp$cal$opportunities) - te_vol$pred_cal)

te_pred_cal_tot <- te_eff$pred_cal * te_vol$pred_cal
te_resid_tot    <- te_sp$cal$total_epa - te_pred_cal_tot
te_cal_opp      <- as.numeric(te_sp$cal$opportunities)
te_alpha        <- fit_power_alpha(te_cal_opp, abs(te_resid_tot))
te_qset_tot     <- signed_quantile_set(te_resid_tot / te_cal_opp^te_alpha)

cli_alert_success("TE conformal: alpha={round(te_alpha, 3)} | tot 80% asym: lo={round(te_qset_tot$lo[2], 3)} hi={round(te_qset_tot$hi[2], 3)} (right skew: {te_qset_tot$hi[2] > abs(te_qset_tot$lo[2])})")

save_model(te_eff$model, "te_eff")
save_model(te_vol$model, "te_vol")

# --- Cal-fold export (NEW)
te_cal_fold <- te_sp$cal |>
  select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
  bind_cols(
    build_intervals_asym(te_eff$pred_cal, te_qset_eff, "eff"),
    build_intervals_asym(te_vol$pred_cal, te_qset_vol, "vol"),
    build_intervals_asym(te_pred_cal_tot, te_qset_tot, "tot", scale = te_cal_opp^te_alpha)
  ) |>
  mutate(alpha_fold = te_alpha)
readr::write_csv(te_cal_fold, paste0(CAL_FOLD_PREFIX, "_te_cal_fold_predictions.csv"))
cli_alert_success("{paste0(CAL_FOLD_PREFIX, '_te_cal_fold_predictions.csv')} ({nrow(te_cal_fold)} rows)")

# ===========================================================================
# QB DEPLOYMENT FOLD (08c procedure) -- UNCHANGED features, not in scope
# ===========================================================================

cli_h1("QB deployment fold (08c procedure, 4 components, feature set unchanged)")

qb_ft <- readRDS("data/qb_feature_table.rds") |> encode_features() |> join_vegas()
qb_sp <- split_fit_cal(qb_ft)
cli_alert_info("QB rows: fit={nrow(qb_sp$fit)} cal={nrow(qb_sp$cal)}")

qb_models <- list()
qb_pred_cal <- list()
for (nm in names(QB_COMPONENTS)) {
  cmp <- QB_COMPONENTS[[nm]]
  res <- train_component(qb_sp$fit, qb_sp$cal, cmp$feats, cmp$y, paste0("qb_", nm))
  qb_models[[nm]]   <- res$model
  qb_pred_cal[[nm]] <- res$pred_cal
  tune_rows <- c(tune_rows, list(res$tune))
  save_model(res$model, paste0("qb_", nm))
}

qb_qs <- list(
  pass_eff = q3(abs(qb_sp$cal$pass_epa_per_db_obs   - qb_pred_cal$pass_eff)),
  db       = q3(abs(as.numeric(qb_sp$cal$dropbacks) - qb_pred_cal$db_vol)),
  carry    = q3(abs(as.numeric(qb_sp$cal$carries)   - qb_pred_cal$carry_vol)),
  rush     = q3(abs(qb_sp$cal$rush_epa              - qb_pred_cal$rush_dir))
)
qb_pred_cal_tot <- qb_pred_cal$pass_eff * qb_pred_cal$db_vol + qb_pred_cal$rush_dir
qb_qs$tot       <- q3(abs(qb_sp$cal$total_epa - qb_pred_cal_tot))

cli_alert_success("QB conformal: tot 80% width={round(2*qb_qs$tot[2], 1)} EPA (backtest 27.1)")

# ===========================================================================
# SAVE DEPLOYMENT PARAMS + AUDIT
# ===========================================================================

cli_h1("Save deployment artifacts (VOLFIX candidate)")

deployment_params <- list(
  built           = "10a_deployment_models_volfix.R",
  tier_order      = TIER_ORDER,
  coverages       = COVERAGES,
  interval_scale  = "combined RB/WR bounds scale by pred_vol^alpha at scoring time (deployment seam, README 10-series note)",
  rb = list(
    trained_through = trained_through(rb_ft),
    eff = list(features = RB_EFF_FEATURES, model_file = file.path(MODELS_DIR, "rb_eff.txt"),
               params = rb_eff$best, qs = rb_qs_eff),
    vol = list(features = RB_VOL_FEATURES, model_file = file.path(MODELS_DIR, "rb_vol.txt"),
               params = rb_vol$best, qs = rb_qs_vol),
    tot = list(mechanism = "power_law", alpha = rb_alpha, q_norm = rb_q_norm)
  ),
  wr = list(
    trained_through = trained_through(wr_ft),
    eff = list(features = WR_EFF_FEATURES, model_file = file.path(MODELS_DIR, "wr_eff.txt"),
               params = wr_eff$best, qset = wr_qset_eff),
    vol = list(features = WR_VOL_FEATURES, model_file = file.path(MODELS_DIR, "wr_vol.txt"),
               params = wr_vol$best, qset = wr_qset_vol),
    tot = list(mechanism = "power_law_asym", alpha = wr_alpha, qset = wr_qset_tot)
  ),
  te = list(
    trained_through = trained_through(te_ft),
    eff = list(features = TE_EFF_FEATURES, model_file = file.path(MODELS_DIR, "te_eff.txt"),
               params = te_eff$best, qset = te_qset_eff),
    vol = list(features = TE_VOL_FEATURES, model_file = file.path(MODELS_DIR, "te_vol.txt"),
               params = te_vol$best, qset = te_qset_vol),
    tot = list(mechanism = "power_law_asym", alpha = te_alpha, qset = te_qset_tot)
  ),
  qb = list(
    trained_through = trained_through(qb_ft),
    components = imap(QB_COMPONENTS, function(cmp, nm) {
      list(features = cmp$feats, y = cmp$y,
           model_file = file.path(MODELS_DIR, paste0("qb_", nm, ".txt")))
    }),
    qs  = qb_qs,
    tot = list(mechanism = "const_additive")
  )
)

saveRDS(deployment_params, PARAMS_OUT)

tune_log <- list_rbind(tune_rows)
readr::write_csv(tune_log, TUNE_LOG_OUT)

cli_alert_success("{PARAMS_OUT}")
cli_alert_success("{MODELS_DIR} ({length(list.files(MODELS_DIR))} model files)")
cli_alert_success("{TUNE_LOG_OUT}")

# ===========================================================================
# VERIFICATION: reload everything fresh and sanity-score the cal rows
# ===========================================================================

cli_h1("Verification: fresh reload + sanity scoring")

dp <- readRDS(PARAMS_OUT)
cal_sets <- list(rb = rb_sp$cal, wr = wr_sp$cal, te = te_sp$cal)
for (pos in c("rb", "wr", "te")) {
  for (cmp in c("eff", "vol")) {
    m <- lightgbm::lgb.load(dp[[pos]][[cmp]]$model_file)
    X <- make_matrix(cal_sets[[pos]], dp[[pos]][[cmp]]$features)
    p <- predict(m, X)
    stopifnot(all(is.finite(p)))
    cli_alert_success("{pos}_{cmp}: reload + predict ok ({length(p)} rows, mean={round(mean(p), 3)})")
  }
}
for (nm in names(QB_COMPONENTS)) {
  m <- lightgbm::lgb.load(dp$qb$components[[nm]]$model_file)
  X <- make_matrix(qb_sp$cal, dp$qb$components[[nm]]$features)
  p <- predict(m, X)
  stopifnot(all(is.finite(p)))
  cli_alert_success("qb_{nm}: reload + predict ok ({length(p)} rows, mean={round(mean(p), 3)})")
}

cli_h1("Step 10a (VOLFIX candidate) complete -- trained through {dp$rb$trained_through$season}-W{dp$rb$trained_through$week}")
