# R/13b_vegas_ab_volfix.R
# VOLFIX CLONE of R/13b_vegas_ab.R: re-points the Vegas A/B at the
# volume-carryforward-fixed baseline instead of the old shipped models.
#
# WHY A SEPARATE FILE (not a routed-around check): 13b's ARM B refits the
# volume component from the shipped per-fold tune log and asserts it
# reproduces ARM A's pred_vol to within 1e-6 (line ~424 of the original,
# `if (max_vol_diff > 1e-6) cli_abort(...)`) -- this integrity gate exists
# BECAUSE normally volume is untouched by the Vegas experiment, so any
# drift signals a real procedure bug. The volume-carryforward fix
# (75e6b43/6713b0e/90df36a + the retrains in 627e909 and the 12c volfix run
# alongside this file) intentionally changed volume predictions. Pointing
# the ORIGINAL 13b at those retrains would make the gate correctly abort --
# it would be measuring "does refit-from-shipped-tune-log reproduce the
# volfix predictions", which it structurally cannot, since the shipped
# tune log's VOL feature list doesn't include baseline_*. That is the gate
# doing its job, not a bug to silence. Instead, this clone re-defines what
# "ARM A" and "the untouched-volume reference" mean: both now point at the
# volfix retrain (VOL_FEATURES + baseline_*), so the same 1e-6 identity
# check now certifies against the CORRECT baseline.
#
# CHANGES vs 13b_vegas_ab.R (everything else byte-identical):
#   1. CFG$*$preds points at the volfix fold predictions (ARM A):
#        RB: output/11c_rb_injury_fold_predictions_volfix.csv
#        WR: output/04c_wr_asym_fold_predictions_volfix.csv
#        TE: output/12c_te_asym_fold_predictions_volfix.csv
#   2. CFG$*$vol gains the baseline_* carryforward columns, in the EXACT
#      column order used by the corresponding volfix training script
#      (R/11c_rb_injury_volfix.R, R/04c_wr_asymmetric_conformal_volfix.R,
#      R/12c_te_asymmetric_conformal_volfix.R). Order matters here: LightGBM's
#      feature_fraction bagging samples columns by index under a fixed seed,
#      so reordering would change tree structure and break the 1e-6
#      reproduction gate even with identical data and hyperparameters.
#   3. CFG$*$tune_log is UNCHANGED (still the original 11c/04b/12b tune
#      logs) -- the volfix retrains reused those hyperparameters verbatim,
#      no re-tuning, so this stays the correct source for "shipped"
#      per-fold VOL hyperparameters.
#   4. Output stem gains a _volfix tag so these receipts never collide with
#      the original 13b outputs.
# EFF_B features, the nested EFF tuning loop, the walk-forward fold logic,
# the conformal construction, and the rubric/verdict logic are all
# untouched from the original.
#
# --- Original 13b header below, unchanged ---
#
# Ablation ladder rung 2, step 1: Vegas-features A/B for the two-component
# positions (RB / WR / TE). QB runs separately (13c, four components).
#
# Usage: Rscript R/13b_vegas_ab_volfix.R RB|WR|TE
#
# DESIGN (follows the 11c arm pattern; informed by the 13a receipts):
#   - 13a found the volume models FLAT vs Vegas but a monotone total-EPA
#     residual gradient in implied team total at every position, which
#     propagates to +-4-12pp conditional dishonesty in the FP tails.
#   - ARM A = the volfix fold predictions (11c/04c/12c volfix). Not re-run;
#     loaded from the frozen receipts.
#   - ARM B = identical procedure with c("team_spread", "implied_total")
#     added to the EFFICIENCY feature set ONLY. Volume features unchanged
#     (13a cell 1 flat -- minimal intervention, clean attribution): the
#     volume component is REFIT from the volfix per-fold tune log (seed
#     42, deterministic) and must reproduce volfix pred_vol EXACTLY
#     (max |diff| < 1e-6 verified per run, else abort).
#   - Interval mechanism per position matches the shipped arm: RB
#     symmetric power-law (03a-v2), WR/TE asymmetric signed (04c/12c).
#
# CLOSING-LINE CAVEAT: lines are nflverse CLOSING lines -- an upper-bound
# ablation, NOT a deployable feature set (pre-registered Friday-lock rule).
# If arm B wins, deployment requires a point-in-time line source (paid
# archive decision per the paid-data policy).
#
# PRE-COMMITTED ACCEPTANCE (stated 2026-07-19 BEFORE the first run):
#   1. RUBRIC INTACT: arm B pooled combined 80% within +-2pp; low-usage
#      veto |delta| <= 10pp. Fail -> arm B vetoed regardless of gradient.
#   2. PRIMARY: the implied-total gradient of the combined residual
#      (high-bucket mean tot_resid minus low-bucket mean) shrinks by at
#      least 50% vs arm A on the same rows.
#   3. SHARPNESS TIEBREAK: arm B mean 80% combined width within +2% of
#      arm A (a Vegas feature that buys honesty by widening is a fail).
# All three hold -> position PASSES rung 2 at the EPA layer; the FP-chain
# re-run and Vegas-stratum calibration close-out happen at the ship step.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

args <- commandArgs(trailingOnly = TRUE)
POS <- if (length(args) >= 1) toupper(args[1]) else "TE"
stopifnot(POS %in% c("RB", "WR", "TE"))

SEASONS  <- 2014L:2025L
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

ALPHA_LO <- 0.20; ALPHA_HI <- 0.90; ALPHA_FALLBACK <- 0.50
COVERAGES <- c(0.50, 0.80, 0.90)
VEGAS_FEATURES <- c("team_spread", "implied_total")

# ---------------------------------------------------------------------------
# Position config (feature sets copied verbatim from the volfix training
# scripts -- 11c_rb_injury_volfix.R / 04c_wr_asymmetric_conformal_volfix.R /
# 12c_te_asymmetric_conformal_volfix.R -- INCLUDING column order)
# ---------------------------------------------------------------------------

RB_INJURY_FEATURES <- c(
  "own_q_int", "own_practice_int", "weeks_missed", "return_from_absence",
  "above_new_out_share", "above_q_share", "above_long_out_share"
)

CFG <- list(
  RB = list(
    table     = "data/rb_feature_table.rds",
    preds     = "output/11c_rb_injury_fold_predictions_volfix.csv",
    tune_log  = "output/11c_rb_injury_tune_log.csv",
    mechanism = "sym",
    low_lo = 5L, low_hi = 8L,
    eff = c("prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp",
            "form_residual", "is_cold_start_int", "draft_tier_int",
            "def_rush_epa_adj", "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
            "wt_snap_share", "games_played_so_far", "def_used_fallback_int"),
    vol = c("wt_carry_share", "wt_target_share", "wt_snap_share", "wt_team_total_plays",
            "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far",
            "baseline_carry_share", "baseline_target_share", "baseline_snap_share",
            "baseline_team_total_plays",
            RB_INJURY_FEATURES)
  ),
  WR = list(
    table     = "data/wr_feature_table.rds",
    preds     = "output/04c_wr_asym_fold_predictions_volfix.csv",
    tune_log  = "output/04b_wr_lgbm_tune_log.csv",
    mechanism = "asym",
    low_lo = 3L, low_hi = 5L,
    eff = c("prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp",
            "form_residual", "is_cold_start_int", "draft_tier_int",
            "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
            "wt_air_yards_per_target",
            "wt_snap_share", "games_played_so_far", "def_used_fallback_int"),
    vol = c("wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_team_total_plays",
            "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
            "draft_tier_int", "is_cold_start_int", "games_played_so_far",
            "baseline_target_share", "baseline_air_yards_share", "baseline_snap_share",
            "baseline_team_total_plays")
  ),
  TE = list(
    table     = "data/te_feature_table.rds",
    preds     = "output/12c_te_asym_fold_predictions_volfix.csv",
    tune_log  = "output/12b_te_lgbm_tune_log.csv",
    mechanism = "asym",
    low_lo = 3L, low_hi = 5L,
    eff = c("prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp",
            "form_residual", "is_cold_start_int", "draft_tier_int",
            "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
            "wt_air_yards_per_target",
            "wt_snap_share", "games_played_so_far", "def_used_fallback_int"),
    vol = c("wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_tgt_per_snap",
            "wt_team_total_plays", "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
            "draft_tier_int", "is_cold_start_int", "games_played_so_far",
            "baseline_target_share", "baseline_air_yards_share", "baseline_snap_share",
            "baseline_tgt_per_snap", "baseline_team_total_plays")
  )
)
cfg <- CFG[[POS]]

# ---------------------------------------------------------------------------
# Helpers (12b/12c machinery)
# ---------------------------------------------------------------------------

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
  n <- length(abs_resid); prob <- (1 + 1 / n) * alpha
  if (prob >= 1.0) return(Inf)
  quantile(abs_resid, prob, names = FALSE)
}
q3 <- function(r) c(conformal_q(r, .5), conformal_q(r, .8), conformal_q(r, .9))

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

tune_lgbm_component <- function(X_fit, y_fit, X_val, y_val) {
  keep <- !is.na(y_fit)
  dtrain <- lgb.Dataset(X_fit[keep, , drop = FALSE], label = y_fit[keep])
  best_rmse <- Inf; best_row <- NULL; best_rounds <- 100L
  for (i in seq_len(nrow(TUNE_GRID))) {
    params <- c(LGBM_FIXED, list(
      num_leaves = TUNE_GRID$num_leaves[i], learning_rate = TUNE_GRID$lr[i],
      min_data_in_leaf = TUNE_GRID$min_data_in_leaf[i]))
    dval <- lgb.Dataset(X_val, label = y_val, reference = dtrain)
    mod <- lgb.train(params = params, data = dtrain, nrounds = INNER_MAX_ROUNDS,
                     valids = list(val = dval),
                     early_stopping_rounds = INNER_EARLY_STOP, verbose = -1L)
    preds <- predict(mod, X_val)
    rmse <- sqrt(mean((y_val - preds)^2, na.rm = TRUE))
    if (rmse < best_rmse) {
      best_rmse <- rmse; best_rounds <- mod$best_iter; best_row <- TUNE_GRID[i, ]
    }
  }
  list(num_leaves = best_row$num_leaves, lr = best_row$lr,
       min_data_in_leaf = best_row$min_data_in_leaf,
       rounds = best_rounds, inner_rmse = best_rmse)
}

fit_lgbm <- function(X, y, num_leaves, lr, min_node, n_rounds) {
  keep <- !is.na(y)
  dtrain <- lgb.Dataset(X[keep, , drop = FALSE], label = y[keep])
  lgb.train(params = c(LGBM_FIXED, list(num_leaves = num_leaves,
                                        learning_rate = lr,
                                        min_data_in_leaf = min_node)),
            data = dtrain, nrounds = n_rounds, verbose = -1L)
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

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)

# ===========================================================================
# 1. LOAD + VEGAS JOIN
# ===========================================================================

cli_h1("13b VOLFIX Vegas A/B -- {POS} (arm B: EFF + {paste(VEGAS_FEATURES, collapse=', ')})")

# NO !is.na(player_id) filter here: the shipped WR chain TRAINED with the
# NA-player pseudo-rows (04d sized them immaterial but they are part of the
# frozen procedure), so arm B must train on the table exactly as shipped or
# the VOL reproduction gate fails on real-but-irrelevant differences.
# RB/TE tables contain no NA rows, so this is a no-op there. NA players are
# excluded from the comparison joins below instead.
ft <- readRDS(cfg$table) |> encode_features()

if (POS == "RB") {
  ft <- ft |> left_join(readRDS("data/injury_states_rb.rds"),
                        by = c("player_id", "season", "week"))
  stopifnot(!any(is.na(ft$own_practice_int)))
}

# Line source seam: default = nflverse CLOSING lines; set VEGAS_LINES_RDS
# (e.g. data/vegas_open_lines.rds from 13d0) to swap in the OPENER variant.
# VEGAS_TAG suffixes the outputs so the two runs coexist as receipts.
LINES_RDS <- Sys.getenv("VEGAS_LINES_RDS", "")
VEGAS_TAG <- Sys.getenv("VEGAS_TAG", "")

if (nzchar(LINES_RDS)) {
  team_lines <- readRDS(LINES_RDS)
  cli_alert_info("Line source: {LINES_RDS} (tag '{VEGAS_TAG}')")
} else {
  sched <- nflreadr::load_schedules(SEASONS) |> filter(game_type == "REG")
  slope <- unname(coef(lm(result ~ spread_line, data = sched))["spread_line"])
  if (!is.finite(slope) || slope < 0.5) cli_abort("Spread sign convention failed.")
  team_lines <- bind_rows(
    sched |> transmute(game_id, posteam = home_team, team_spread =  spread_line, total_line),
    sched |> transmute(game_id, posteam = away_team, team_spread = -spread_line, total_line)
  ) |>
    mutate(implied_total = (total_line + team_spread) / 2) |>
    select(game_id, posteam, team_spread, implied_total)
  cli_alert_info("Line source: nflverse closing lines")
}

ft <- ft |> left_join(team_lines, by = c("game_id", "posteam"))
cli_alert_info("Vegas NA rows: {sum(is.na(ft$team_spread))} of {nrow(ft)} (LightGBM handles NA natively)")

fold_map <- readRDS("data/fold_map.rds")
tune_log <- readr::read_csv(cfg$tune_log, show_col_types = FALSE)
shipped  <- readr::read_csv(cfg$preds, show_col_types = FALSE) |>
  filter(!is.na(player_id))
if (nrow(tune_log) != nrow(fold_map)) cli_abort("Tune log / fold map mismatch")

EFF_B <- c(cfg$eff, VEGAS_FEATURES)
missing <- setdiff(c(EFF_B, cfg$vol), names(ft))
if (length(missing)) cli_abort("Missing features: {paste(missing, collapse=', ')}")

EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()
cli_alert_success("{POS}: {nrow(ft)} rows | {nrow(fold_map)} folds | expected test rows {EXPECTED_TEST_N} | volfix arm rows {nrow(shipped)}")

# ===========================================================================
# 2. WALK-FORWARD LOOP (arm B: retune EFF, refit VOL from volfix tune log)
# ===========================================================================

cli_h1("Walk-forward loop ({nrow(fold_map)} folds x {nrow(TUNE_GRID)} combos, EFF only)")

fold_results <- vector("list", nrow(fold_map))
tune_rows    <- vector("list", nrow(fold_map))
alpha_log    <- numeric(nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {
  t0 <- proc.time()[["elapsed"]]
  ts <- fold_map$test_season[f]; tw <- fold_map$test_week[f]

  test_data  <- ft |> filter(season == ts, week == tw)
  train_data <- ft |> filter(season < ts | (season == ts & week < tw))

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)
  fit_sws   <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data  <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data  <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # --- EFF arm B: fresh nested tune with Vegas features ---
  best_eff <- tune_lgbm_component(
    make_matrix(fit_data, EFF_B), fit_data$epa_per_opp_obs,
    make_matrix(cal_data, EFF_B), cal_data$epa_per_opp_obs
  )
  mod_eff <- fit_lgbm(make_matrix(fit_data, EFF_B), fit_data$epa_per_opp_obs,
                      best_eff$num_leaves, best_eff$lr, best_eff$min_data_in_leaf,
                      max(REFIT_ROUNDS_MIN, best_eff$rounds))

  # --- VOL: refit from volfix tune log (deterministic reproduction) ---
  tl <- tune_log[f, ]
  mod_vol <- fit_lgbm(make_matrix(fit_data, cfg$vol),
                      as.numeric(fit_data$opportunities),
                      tl$vol_num_leaves, tl$vol_lr, tl$vol_min_node,
                      max(REFIT_ROUNDS_MIN, tl$vol_rounds))

  pred_cal_eff  <- predict(mod_eff, make_matrix(cal_data,  EFF_B))
  pred_test_eff <- predict(mod_eff, make_matrix(test_data, EFF_B))
  pred_cal_vol  <- predict(mod_vol, make_matrix(cal_data,  cfg$vol))
  pred_test_vol <- predict(mod_vol, make_matrix(test_data, cfg$vol))

  pred_cal_tot  <- pred_cal_eff  * pred_cal_vol
  pred_test_tot <- pred_test_eff * pred_test_vol
  cal_opp  <- as.numeric(cal_data$opportunities)
  test_opp <- as.numeric(test_data$opportunities)

  base <- test_data |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa,
           team_spread, implied_total) |>
    mutate(pred_eff = pred_test_eff, pred_vol = pred_test_vol,
           pred_tot = pred_test_tot)

  if (cfg$mechanism == "sym") {
    raw_resid <- abs(cal_data$total_epa - pred_cal_tot)
    alpha     <- fit_power_alpha(cal_opp, raw_resid)
    qn        <- q3(raw_resid / cal_opp^alpha)
    for (i in seq_along(qn)) {
      cv <- c("50", "80", "90")[i]
      hw <- qn[i] * test_opp^alpha
      base[[paste0("lo_", cv, "_tot")]] <- pred_test_tot - hw
      base[[paste0("hi_", cv, "_tot")]] <- pred_test_tot + hw
    }
  } else {
    resid_tot <- cal_data$total_epa - pred_cal_tot
    alpha     <- fit_power_alpha(cal_opp, abs(resid_tot))
    qs        <- signed_quantile_set(resid_tot / cal_opp^alpha)
    sc        <- test_opp^alpha
    base[["med_tot"]] <- pred_test_tot + qs$med * sc
    for (i in seq_along(COVERAGES)) {
      cv <- c("50", "80", "90")[i]
      base[[paste0("lo_", cv, "_tot")]] <- pred_test_tot + qs$lo[i] * sc
      base[[paste0("hi_", cv, "_tot")]] <- pred_test_tot + qs$hi[i] * sc
    }
  }
  alpha_log[f] <- alpha
  fold_results[[f]] <- base |> mutate(fold = f, alpha_fold = alpha)

  tune_rows[[f]] <- tibble(fold = f,
    eff_num_leaves = best_eff$num_leaves, eff_lr = best_eff$lr,
    eff_min_node = best_eff$min_data_in_leaf, eff_rounds = best_eff$rounds,
    eff_inner_rmse = round(best_eff$inner_rmse, 4))

  if (f %% 25 == 0 || f == nrow(fold_map)) {
    cli_alert_info("Fold {sprintf('%03d', f)} [{ts}-W{sprintf('%02d', tw)}] ({round(proc.time()[['elapsed']] - t0, 1)}s/fold)")
  }
}

results_b <- bind_rows(fold_results)
tune_b    <- bind_rows(tune_rows)

# ===========================================================================
# 3. INTEGRITY: VOL REPRODUCTION + ROW COUNTS
# ===========================================================================

cli_h1("Integrity")

if (nrow(results_b) != EXPECTED_TEST_N) {
  cli_warn("Row count {nrow(results_b)} vs expected {EXPECTED_TEST_N}")
} else cli_alert_success("Row count {nrow(results_b)} / {EXPECTED_TEST_N}")

# Duplicate-key guard: the TE table carries one duplicated player-week
# (Conklin 2021-W18, a 12a snap-crosswalk dup -- flagged for the next
# feature-table version). Key-based joins would cross the two rows and
# fake a reproduction failure, so dup keys are excluded and logged.
dup_keys <- bind_rows(
  results_b |> count(player_id, season, week),
  shipped   |> count(player_id, season, week)
) |>
  filter(n > 1) |>
  distinct(player_id, season, week)
if (nrow(dup_keys) > 0) {
  cli_alert_warning("Excluding {nrow(dup_keys)} duplicated player-week key{?s} from A/B joins (feature-table dup, see header of next 12a version)")
}

vol_check <- results_b |>
  filter(!is.na(player_id)) |>
  anti_join(dup_keys, by = c("player_id", "season", "week")) |>
  select(player_id, season, week, pred_vol_b = pred_vol) |>
  inner_join(shipped |> select(player_id, season, week, pred_vol),
             by = c("player_id", "season", "week"))
max_vol_diff <- max(abs(vol_check$pred_vol_b - vol_check$pred_vol))
cli_alert_info("VOL reproduction (vs VOLFIX baseline): {nrow(vol_check)} matched rows, max |pred_vol diff| = {format(max_vol_diff, scientific = TRUE)}")
if (max_vol_diff > 1e-6) cli_abort("VOL arm failed to reproduce the VOLFIX baseline predictions -- procedure drift, do not trust the A/B.")

# ===========================================================================
# 4. A/B TABLES
# ===========================================================================

cli_h1("A/B scoring")

itotal_bucket <- function(it) cut(it, c(-Inf, 20, 26, Inf),
  labels = c("low_implied", "mid_implied", "high_implied"))

ab_rows <- shipped |>
  filter(!is.na(player_id)) |>
  anti_join(dup_keys, by = c("player_id", "season", "week")) |>
  select(player_id, season, week, opportunities, total_epa,
         pred_tot_a = pred_tot, lo_80_a = lo_80_tot, hi_80_a = hi_80_tot,
         lo_50_a = lo_50_tot, hi_50_a = hi_50_tot,
         lo_90_a = lo_90_tot, hi_90_a = hi_90_tot) |>
  inner_join(results_b |>
               select(player_id, season, week, pred_tot_b = pred_tot,
                      lo_50_tot, hi_50_tot, lo_80_tot, hi_80_tot,
                      lo_90_tot, hi_90_tot, team_spread, implied_total),
             by = c("player_id", "season", "week")) |>
  mutate(resid_a = total_epa - pred_tot_a,
         resid_b = total_epa - pred_tot_b,
         ib = itotal_bucket(implied_total))

cli_alert_success("Matched A/B rows: {nrow(ab_rows)} (arm A {nrow(shipped)}, arm B {nrow(results_b)})")

# PRIMARY: implied-total gradient of combined residual
grad_tbl <- ab_rows |>
  filter(!is.na(ib)) |>
  group_by(ib) |>
  summarise(n = n(), arm_a = mean(resid_a), arm_b = mean(resid_b), .groups = "drop")
grad_a <- grad_tbl$arm_a[grad_tbl$ib == "high_implied"] - grad_tbl$arm_a[grad_tbl$ib == "low_implied"]
grad_b <- grad_tbl$arm_b[grad_tbl$ib == "high_implied"] - grad_tbl$arm_b[grad_tbl$ib == "low_implied"]

cli_h2("PRIMARY: combined residual by implied-total bucket")
print(grad_tbl |> mutate(across(c(arm_a, arm_b), ~ round(.x, 3))) |>
        as.data.frame(), row.names = FALSE)
cli_alert_info("Gradient (high - low): arm A = {round(grad_a, 3)} EPA | arm B = {round(grad_b, 3)} EPA | shrink = {round(100 * (1 - abs(grad_b) / abs(grad_a)), 1)}%")

# RUBRIC: coverage arm B (pooled + low-usage) vs arm A
cov_b_pool <- eval_calibration(ab_rows$total_epa,
  ab_rows |> transmute(lo_50 = lo_50_tot, hi_50 = hi_50_tot,
                       lo_80 = lo_80_tot, hi_80 = hi_80_tot,
                       lo_90 = lo_90_tot, hi_90 = hi_90_tot))
cov_a_pool <- eval_calibration(ab_rows$total_epa,
  ab_rows |> transmute(lo_50 = lo_50_a, hi_50 = hi_50_a,
                       lo_80 = lo_80_a, hi_80 = hi_80_a,
                       lo_90 = lo_90_a, hi_90 = hi_90_a))

lo_rows <- ab_rows |> filter(opportunities >= cfg$low_lo, opportunities <= cfg$low_hi)
cov_b_low <- eval_calibration(lo_rows$total_epa,
  lo_rows |> transmute(lo_50 = lo_50_tot, hi_50 = hi_50_tot,
                       lo_80 = lo_80_tot, hi_80 = hi_80_tot,
                       lo_90 = lo_90_tot, hi_90 = hi_90_tot))

b80  <- cov_b_pool |> filter(nominal == 0.80)
a80  <- cov_a_pool |> filter(nominal == 0.80)
bl80 <- cov_b_low  |> filter(nominal == 0.80)

cli_h2("RUBRIC: arm B coverage (80%)")
cli_alert_info("Pooled: arm B {fmt_pp(b80$delta)} w={round(b80$sharpness, 3)} | arm A {fmt_pp(a80$delta)} w={round(a80$sharpness, 3)}")
cli_alert_info("Low-usage (opp {cfg$low_lo}-{cfg$low_hi}): arm B {fmt_pp(bl80$delta)} (n={nrow(lo_rows)})")

rmse_a <- sqrt(mean(ab_rows$resid_a^2)); rmse_b <- sqrt(mean(ab_rows$resid_b^2))
cli_alert_info("Combined RMSE: arm A {round(rmse_a, 3)} | arm B {round(rmse_b, 3)} ({sprintf('%+.2f%%', 100 * (rmse_b / rmse_a - 1))})")

# ===========================================================================
# 5. VERDICT (pre-committed)
# ===========================================================================

cli_h1("13b VOLFIX verdict -- {POS} (pre-committed rule)")

rubric_ok    <- abs(b80$delta) <= 0.02 && abs(bl80$delta) <= 0.10
gradient_ok  <- abs(grad_b) <= 0.5 * abs(grad_a)
sharpness_ok <- b80$sharpness <= a80$sharpness * 1.02

cli_alert_info("1. Rubric intact:    {if (rubric_ok) 'PASS' else 'FAIL'} (pooled {fmt_pp(b80$delta)}, low {fmt_pp(bl80$delta)})")
cli_alert_info("2. Gradient >=50%:   {if (gradient_ok) 'PASS' else 'FAIL'} ({round(grad_a, 2)} -> {round(grad_b, 2)} EPA)")
cli_alert_info("3. Sharpness +2% cap: {if (sharpness_ok) 'PASS' else 'FAIL'} ({round(a80$sharpness, 2)} -> {round(b80$sharpness, 2)})")

if (rubric_ok && gradient_ok && sharpness_ok) {
  cli_alert_success("{POS} PASSES rung 2 at the EPA layer -- Vegas features earn their spot (deployment blocked on a point-in-time line source).")
} else {
  cli_alert_warning("{POS} does NOT pass -- publish the receipt as-is.")
}

# ===========================================================================
# 6. SAVE
# ===========================================================================

cli_h1("Save receipts (VOLFIX -- new files, originals untouched)")
stem <- sprintf("output/13b_%s_vegas_volfix%s", tolower(POS),
                if (nzchar(VEGAS_TAG)) paste0("_", VEGAS_TAG) else "")
readr::write_csv(results_b, paste0(stem, "_fold_predictions.csv"))
readr::write_csv(tune_b,    paste0(stem, "_tune_log.csv"))
readr::write_csv(grad_tbl |> mutate(position = POS,
                                    grad_a = grad_a, grad_b = grad_b,
                                    rmse_a = rmse_a, rmse_b = rmse_b,
                                    pooled80_delta_b = b80$delta,
                                    low80_delta_b = bl80$delta,
                                    sharp80_a = a80$sharpness,
                                    sharp80_b = b80$sharpness),
                paste0(stem, "_ab_table.csv"))
cli_alert_success("{stem}_*.csv")

cli_h1("13b VOLFIX {POS} complete")
