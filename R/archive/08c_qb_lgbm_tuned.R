# R/08c_qb_lgbm_tuned.R
# Step 8c: Nested-walk-forward-tuned LightGBM for the QB EPA model -- the
# 03a-v2/04b recipe applied to the QB architecture locked in 08b.
#
# FROZEN INPUTS AND DECISIONS (do not modify):
#   - Architecture: HYBRID (variant H from 08b bake-off):
#       total_epa = pass_eff x dropbacks + rush_epa_direct
#     plus a tuned carries model for the FP translation layer.
#   - Combined-interval mechanism: CONSTANT WIDTH (08b: all three mechanisms
#     flat across rush tiers, const wins on simplicity; plaw alpha clamped at
#     its floor in 101/204 folds -- no volume scaling to model).
#   - Feature lists: identical to 08b (kept frozen per Steve 2026-07-10;
#     the mover-tier pass offset is accepted and left to recalibration).
#   - qb_feature_table.rds, fold_map.rds, metrics.R.
#
# TUNING: 32-combo grid per component per fold, inner split = last 20% of
# season-weeks as RMSE holdout, exactly as 03a-v2/04b. FOUR tuned components:
# pass_eff, db_vol, carry_vol, rush_dir.
#
# HONESTY CHECKS (pre-committed):
#   - Veto axis: EX-ANTE rush tier (rolling wt_carries) at 80%, +-10pp --
#     the 08b lesson: observed-carry tiers condition on game script, so the
#     deployable conditioning is the veto; observed-tier is reported alongside.
#   - Watch items from 08b2 (report-only, no pass/fail): (a) rush-component
#     residual slope vs prior_carries_pg (untuned: +0.069/carry, t=3.8),
#     (b) per-player star shrinkage (untuned: Allen +2.1, Lamar +2.0).
#
# EXPECTED (stated before run): pooled coverage stays within +-2pp, ex-ante
# tiers within +-3pp, veto passes; 80% combined width narrows from the
# untuned 29.0 EPA toward ~27-28.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS
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

TIER_BREAKS <- c(-Inf, 4, 8, Inf)
TIER_LABELS <- c("statue (0-3)", "mover (4-7)", "scrambler (8+)")

MIN_GAMES_PLAYER <- 20L   # per-player watch-item table floor

# ===========================================================================
# FEATURE SETS -- FROZEN, identical to 08b
# ===========================================================================

PASS_EFF_FEATURES <- c(
  "prior_pass_epa_per_db", "baseline_pass_epa_per_db", "rolling_pass_epa_per_db",
  "form_residual", "is_cold_start_int", "draft_tier_int",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

DB_VOL_FEATURES <- c(
  "wt_dropbacks", "wt_team_total_plays", "wt_team_pass_rate",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

CARRY_VOL_FEATURES <- c(
  "wt_carries", "wt_carry_share", "prior_carries_pg", "wt_team_total_plays",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

RUSH_DIRECT_FEATURES <- c(
  "wt_rush_epa_pg", "wt_carries", "wt_carry_share",
  "prior_rush_epa_pg", "prior_carries_pg",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

COMPONENTS <- list(
  pass_eff  = list(feats = PASS_EFF_FEATURES,    y = "pass_epa_per_db_obs"),
  db_vol    = list(feats = DB_VOL_FEATURES,      y = "dropbacks"),
  carry_vol = list(feats = CARRY_VOL_FEATURES,   y = "carries"),
  rush_dir  = list(feats = RUSH_DIRECT_FEATURES, y = "rush_epa")
)

# ===========================================================================
# HELPERS (house pattern, identical to 04b)
# ===========================================================================

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |> mutate(
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

rush_tier <- function(carries) {
  cut(carries, TIER_BREAKS, labels = TIER_LABELS, right = FALSE)
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

    keep_val <- !is.na(y_val)
    dval <- lgb.Dataset(X_val[keep_val, , drop = FALSE], label = y_val[keep_val],
                        reference = dtrain)

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
    n_rounds <- mod$best_iter

    if (val_rmse < best_rmse) {
      best_rmse   <- val_rmse
      best_rounds <- n_rounds
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

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 8c: QB Nested-Walk-Forward-Tuned LightGBM (hybrid + const)")
cli_alert_info("Grid: {nrow(TUNE_GRID)} combos x 4 components per fold")
cli_alert_info("Construction: FROZEN variant H + const mechanism (from 08b)")
cli_alert_info("Veto axis: EX-ANTE rush tier at 80%, +-10pp")

ft       <- readRDS("data/qb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()

cli_alert_success("QB feature table: {nrow(ft)} rows | Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

for (cmp in COMPONENTS) {
  miss <- setdiff(cmp$feats, names(ft))
  if (length(miss) > 0) cli::cli_abort("Missing features: {paste(miss, collapse=', ')}")
}
cli_alert_success("All model features present")

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds x {nrow(TUNE_GRID)} combos x 4 components)")

fold_results <- vector("list", nrow(fold_map))
tune_log     <- vector("list", nrow(fold_map))

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

  fit_data <- train_data |> semi_join(head(train_sws, nrow(train_sws) - n_cal_sw), by = c("season", "week"))
  cal_data <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # --- Tune, refit, predict each component ---
  preds_cal  <- list()
  preds_test <- list()
  fold_tune  <- list(fold = f)

  for (nm in names(COMPONENTS)) {
    cmp   <- COMPONENTS[[nm]]
    X_fit  <- make_matrix(fit_data,  cmp$feats)
    X_cal  <- make_matrix(cal_data,  cmp$feats)
    X_test <- make_matrix(test_data, cmp$feats)
    y_fit  <- as.numeric(fit_data[[cmp$y]])
    y_cal  <- as.numeric(cal_data[[cmp$y]])

    best <- tune_lgbm_component(X_fit, y_fit, X_cal, y_cal)
    mod  <- fit_lgbm_tuned(X_fit, y_fit, best)

    preds_cal[[nm]]  <- predict(mod, X_cal)
    preds_test[[nm]] <- predict(mod, X_test)

    fold_tune[[paste0(nm, "_leaves")]]     <- best$num_leaves
    fold_tune[[paste0(nm, "_lr")]]         <- best$lr
    fold_tune[[paste0(nm, "_min_node")]]   <- best$min_data_in_leaf
    fold_tune[[paste0(nm, "_rounds")]]     <- best$rounds
    fold_tune[[paste0(nm, "_inner_rmse")]] <- round(best$inner_rmse, 4)
  }

  tune_log[[f]] <- as_tibble(fold_tune)

  # --- Component conformal intervals (scalar) ---
  qs_pass_eff <- q3(abs(cal_data$pass_epa_per_db_obs      - preds_cal$pass_eff))
  qs_db       <- q3(abs(as.numeric(cal_data$dropbacks)    - preds_cal$db_vol))
  qs_carry    <- q3(abs(as.numeric(cal_data$carries)      - preds_cal$carry_vol))
  qs_rush     <- q3(abs(cal_data$rush_epa                 - preds_cal$rush_dir))

  # --- Combined: FROZEN hybrid + const mechanism ---
  pred_cal_tot  <- preds_cal$pass_eff  * preds_cal$db_vol  + preds_cal$rush_dir
  pred_test_tot <- preds_test$pass_eff * preds_test$db_vol + preds_test$rush_dir
  qs_tot        <- q3(abs(cal_data$total_epa - pred_cal_tot))

  fold_results[[f]] <- test_data |>
    select(player_id, player_name, season, week, dropbacks, carries,
           wt_carries, prior_carries_pg, pass_epa, rush_epa, total_epa) |>
    bind_cols(
      build_intervals(preds_test$pass_eff,  qs_pass_eff, "pass_eff"),
      build_intervals(preds_test$db_vol,    qs_db,       "db"),
      build_intervals(preds_test$carry_vol, qs_carry,    "carry"),
      build_intervals(preds_test$rush_dir,  qs_rush,     "rush"),
      build_intervals(pred_test_tot,        qs_tot,      "tot")
    ) |>
    mutate(fold = f)

  t1 <- proc.time()[["elapsed"]]
  if (f %% 10 == 0 || f == 1L || f == nrow(fold_map)) {
    tl <- tune_log[[f]]
    cli_alert_info(
      "Fold {sprintf('%03d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(test_data)} rows | inner RMSE eff={tl$pass_eff_inner_rmse} db={tl$db_vol_inner_rmse} carry={tl$carry_vol_inner_rmse} rush={tl$rush_dir_inner_rmse} | {round(t1-t0)}s"
    )
  }
}

results  <- bind_rows(fold_results)
tune_all <- bind_rows(tune_log)

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

na_tot <- sum(is.na(results$pred_tot))
if (na_tot == 0L) {
  cli_alert_success("Zero NA combined predictions")
} else {
  cli_warn("{na_tot} NA combined predictions")
}

cli_alert_success("Inner tuning: all combos scored on inner holdout only; outer test scored once")

# ===========================================================================
# HYPERPARAMETER AUDIT (summary; full log in CSV)
# ===========================================================================

cli_h1("Selected Hyperparameters (summary)")
for (nm in names(COMPONENTS)) {
  lv <- table(tune_all[[paste0(nm, "_leaves")]])
  cli_alert_info(
    "{nm}: leaves {paste(names(lv), lv, sep='=', collapse=' | ')} | median rounds {median(tune_all[[paste0(nm, '_rounds')]])}"
  )
}

# ===========================================================================
# COVERAGE SCORING
# ===========================================================================

cli_h1("8c Pooled Coverage (all {n_scored} test rows)")

pooled <- bind_rows(
  score_component(results$pass_epa / results$dropbacks, results, "pass_eff", "pass_efficiency"),
  score_component(as.numeric(results$dropbacks),        results, "db",       "dropback_volume"),
  score_component(as.numeric(results$carries),          results, "carry",    "carry_volume"),
  score_component(results$rush_epa,                     results, "rush",     "rush_epa_direct"),
  score_component(results$total_epa,                    results, "tot",      "combined")
)
print(pooled |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# Ex-ante tier (VETO AXIS) + observed tier (reported alongside)
res_ex <- results |> filter(!is.na(wt_carries)) |> mutate(ex_tier = rush_tier(wt_carries))

cli_h2("Combined coverage by EX-ANTE rush tier (VETO AXIS)")
ex_cov <- eval_calibration_stratified(
  res_ex$total_epa, pi_cols(res_ex, "tot"), strata = res_ex$ex_tier
) |> mutate(component = "combined", axis = "ex_ante")
print(ex_cov |> select(stratum, n, nominal, empirical, delta, sharpness), n = Inf)

cli_h2("Combined coverage by OBSERVED rush tier (game-script axis, report only)")
obs_cov <- eval_calibration_stratified(
  results$total_epa, pi_cols(results, "tot"), strata = rush_tier(results$carries)
) |> mutate(component = "combined", axis = "observed")
print(obs_cov |> select(stratum, n, nominal, empirical, delta, sharpness), n = Inf)

# ===========================================================================
# RUBRIC DECISION
# ===========================================================================

cli_h1("Rubric Decision: 8c")

pool80 <- pooled |> filter(component == "combined", nominal == 0.80)
ex_80  <- ex_cov |> filter(nominal == 0.80)
worst  <- ex_80  |> slice_max(abs(delta), n = 1)

cli_alert_info("Pooled 80%: delta={fmt_pp(pool80$delta)} | width={round(pool80$sharpness, 2)} (untuned 08b: 29.0)")
for (i in seq_len(nrow(ex_80))) {
  cli_alert_info("  ex-ante {ex_80$stratum[i]} (n={ex_80$n[i]}): {fmt_pp(ex_80$delta[i])}")
}

if (max(abs(ex_80$delta)) > 0.10) {
  cli_warn("8c VETOED: ex-ante {worst$stratum} 80% delta = {fmt_pp(worst$delta)} (threshold +-10pp)")
} else {
  cli_alert_success("8c veto passed: worst ex-ante tier delta = {fmt_pp(worst$delta)} ({worst$stratum})")
  if (abs(pool80$delta) <= 0.02) {
    cli_alert_success("Pooled coverage within +-2pp -- QB model READY")
  } else {
    cli_alert_info("Pooled coverage {fmt_pp(pool80$delta)} -- outside +-2pp; inspect stratified tables")
  }
}

# ===========================================================================
# WATCH ITEMS FROM 08b2 (report only)
# ===========================================================================

cli_h1("Watch items (08b2 diagnostics re-run on tuned model)")

# (a) rush-component conservatism vs rush identity
wi <- results |>
  filter(!is.na(prior_carries_pg)) |>
  mutate(res_rush = rush_epa - pred_rush)
fit_wi <- lm(res_rush ~ prior_carries_pg, data = wi)
cf <- summary(fit_wi)$coefficients
cli_alert_info(
  "res_rush ~ prior_carries_pg: slope={round(cf[2,1],4)} (t={round(cf[2,3],2)}) | untuned was +0.069 (t=3.84)"
)

# (b) per-player star shrinkage
player_bias <- results |>
  mutate(res_tot = total_epa - pred_tot) |>
  group_by(player_id, player_name) |>
  summarise(
    games    = n(),
    cpg      = round(mean(carries), 2),
    bias_tot = round(mean(res_tot), 2),
    t_tot    = round(mean(res_tot) / (sd(res_tot) / sqrt(n())), 2),
    .groups  = "drop"
  ) |>
  filter(games >= MIN_GAMES_PLAYER) |>
  arrange(desc(abs(bias_tot)))

cli_h2("Largest per-player |bias| (>= {MIN_GAMES_PLAYER} games, top 10)")
print(as.data.frame(head(player_bias, 10)), row.names = FALSE)

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,  "output/08c_qb_fold_predictions.csv")
readr::write_csv(tune_all, "output/08c_qb_tune_log.csv")
readr::write_csv(pooled,   "output/08c_qb_pooled_coverage.csv")
readr::write_csv(bind_rows(ex_cov, obs_cov), "output/08c_qb_tier_coverage.csv")
readr::write_csv(player_bias, "output/08c_qb_player_bias.csv")

cli_alert_success("output/08c_qb_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/08c_qb_tune_log.csv")
cli_alert_success("output/08c_qb_pooled_coverage.csv")
cli_alert_success("output/08c_qb_tier_coverage.csv  (ex-ante veto axis + observed)")
cli_alert_success("output/08c_qb_player_bias.csv")

cli_h1("Step 8c complete")
