# R/13c_qb_vegas_ab.R
# Ablation ladder rung 2, step 1 (QB): Vegas-features A/B for the QB
# four-component architecture. Companion to 13b_vegas_ab.R (RB/WR/TE);
# same design, same pre-committed acceptance, adapted to the 08c spine.
#
# 13a receipts for QB: dropback residual gradient below trigger (-1.03 ->
# -0.14 across implied buckets, under the 2.0 db bar); total-EPA residual
# gradient LARGEST of any position (-2.67 -> +2.42 EPA). The intervention
# therefore targets PASS EFFICIENCY only (per-dropback EPA is where a
# Vegas-priced environment should bite): ARM B = 08c procedure with
# c("team_spread", "implied_total") added to PASS_EFF_FEATURES. The db /
# carry / rush_dir components are REFIT from the shipped 08c tune log
# (seed 42, deterministic) and must reproduce shipped predictions to
# 1e-6 or the run aborts.
#
# Combined mechanism: CONSTANT WIDTH (08b lock) -- q3 on |cal tot resid|.
#
# CLOSING-LINE CAVEAT: same as 13b -- upper-bound ablation, not a
# deployable feature set.
#
# PRE-COMMITTED ACCEPTANCE (identical to 13b, stated before first run):
#   1. RUBRIC INTACT: arm B pooled combined 80% within +-2pp; EX-ANTE
#      rush-tier veto (rolling wt_carries tiers) |delta| <= 10pp each.
#   2. PRIMARY: implied-total gradient of combined residual shrinks >= 50%.
#   3. SHARPNESS: arm B mean 80% combined width within +2% of arm A.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(lightgbm)
  library(cli)
})

source("R/metrics.R")

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

COVERAGES <- c(0.50, 0.80, 0.90)
VEGAS_FEATURES <- c("team_spread", "implied_total")

TIER_BREAKS <- c(-Inf, 4, 8, Inf)
TIER_LABELS <- c("statue (0-3)", "mover (4-7)", "scrambler (8+)")

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

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |> mutate(
    draft_tier_int        = TIER_ORDER[draft_tier],
    is_cold_start_int     = as.integer(is_cold_start),
    def_used_fallback_int = as.integer(def_used_fallback)
  )
}

make_matrix <- function(df, features) df |> select(all_of(features)) |> as.matrix()

conformal_q <- function(abs_resid, alpha) {
  n <- length(abs_resid); prob <- (1 + 1 / n) * alpha
  if (prob >= 1.0) return(Inf)
  quantile(abs_resid, prob, names = FALSE)
}
q3 <- function(r) c(conformal_q(r, .5), conformal_q(r, .8), conformal_q(r, .9))

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
    rmse <- sqrt(mean((y_val - predict(mod, X_val))^2, na.rm = TRUE))
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

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)

# ===========================================================================
# 1. LOAD + VEGAS JOIN
# ===========================================================================

cli_h1("13c QB Vegas A/B (arm B: PASS_EFF + {paste(VEGAS_FEATURES, collapse=', ')})")

ft <- readRDS("data/qb_feature_table.rds") |> filter(!is.na(player_id)) |> encode_features()

# Line source seam (see 13b): default closing; VEGAS_LINES_RDS = openers.
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
cli_alert_info("Vegas NA rows: {sum(is.na(ft$team_spread))} of {nrow(ft)}")

fold_map <- readRDS("data/fold_map.rds")
tune_log <- readr::read_csv("output/08c_qb_tune_log.csv", show_col_types = FALSE)
shipped  <- readr::read_csv("output/08c_qb_fold_predictions.csv", show_col_types = FALSE) |>
  filter(!is.na(player_id))
if (nrow(tune_log) != nrow(fold_map)) cli_abort("Tune log / fold map mismatch")

# REUSE_TUNE_LOG: skip the pass_eff grid and refit from a previous run's
# logged per-fold params (deterministic, seed 42). Used by the ship pass to
# MATERIALIZE the winning arm with full interval columns without re-tuning.
REUSE_TUNE_LOG <- Sys.getenv("REUSE_TUNE_LOG", "")
pe_log <- NULL
if (nzchar(REUSE_TUNE_LOG)) {
  pe_log <- readr::read_csv(REUSE_TUNE_LOG, show_col_types = FALSE)
  if (nrow(pe_log) != nrow(fold_map)) cli_abort("REUSE_TUNE_LOG fold count mismatch")
  cli_alert_info("pass_eff params reused from {REUSE_TUNE_LOG} (no grid search)")
}

PASS_EFF_B <- c(PASS_EFF_FEATURES, VEGAS_FEATURES)
missing <- setdiff(c(PASS_EFF_B, DB_VOL_FEATURES, CARRY_VOL_FEATURES,
                     RUSH_DIRECT_FEATURES), names(ft))
if (length(missing)) cli_abort("Missing features: {paste(missing, collapse=', ')}")

EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()
cli_alert_success("QB: {nrow(ft)} rows | {nrow(fold_map)} folds | expected test rows {EXPECTED_TEST_N} | shipped rows {nrow(shipped)}")

# ===========================================================================
# 2. WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward loop ({nrow(fold_map)} folds; pass_eff retuned, others refit)")

fold_results <- vector("list", nrow(fold_map))
tune_rows    <- vector("list", nrow(fold_map))

for (f in seq_len(nrow(fold_map))) {
  t0 <- proc.time()[["elapsed"]]
  ts <- fold_map$test_season[f]; tw <- fold_map$test_week[f]

  test_data  <- ft |> filter(season == ts, week == tw)
  train_data <- ft |> filter(season < ts | (season == ts & week < tw))
  if (nrow(test_data) == 0) { fold_results[[f]] <- NULL; next }

  train_sws <- train_data |> distinct(season, week) |> arrange(season, week)
  n_cal_sw  <- max(1L, floor(CAL_FRAC * nrow(train_sws)))
  cal_sws   <- tail(train_sws, n_cal_sw)
  fit_sws   <- head(train_sws, nrow(train_sws) - n_cal_sw)
  fit_data  <- train_data |> semi_join(fit_sws, by = c("season", "week"))
  cal_data  <- train_data |> semi_join(cal_sws, by = c("season", "week"))

  # ARM B pass_eff: fresh nested tune with Vegas features (or logged params)
  if (!is.null(pe_log)) {
    pl <- pe_log[f, ]
    best_pe <- list(num_leaves = pl$pe_leaves, lr = pl$pe_lr,
                    min_data_in_leaf = pl$pe_min_node, rounds = pl$pe_rounds,
                    inner_rmse = pl$pe_inner_rmse)
  } else {
    best_pe <- tune_lgbm_component(
      make_matrix(fit_data, PASS_EFF_B), fit_data$pass_epa_per_db_obs,
      make_matrix(cal_data, PASS_EFF_B), cal_data$pass_epa_per_db_obs
    )
  }
  mod_pe <- fit_lgbm(make_matrix(fit_data, PASS_EFF_B), fit_data$pass_epa_per_db_obs,
                     best_pe$num_leaves, best_pe$lr, best_pe$min_data_in_leaf,
                     max(REFIT_ROUNDS_MIN, best_pe$rounds))

  # Other components: refit from shipped tune log
  tl <- tune_log[f, ]
  mod_db <- fit_lgbm(make_matrix(fit_data, DB_VOL_FEATURES),
                     as.numeric(fit_data$dropbacks),
                     tl$db_vol_leaves, tl$db_vol_lr, tl$db_vol_min_node,
                     max(REFIT_ROUNDS_MIN, tl$db_vol_rounds))
  mod_ca <- fit_lgbm(make_matrix(fit_data, CARRY_VOL_FEATURES),
                     as.numeric(fit_data$carries),
                     tl$carry_vol_leaves, tl$carry_vol_lr, tl$carry_vol_min_node,
                     max(REFIT_ROUNDS_MIN, tl$carry_vol_rounds))
  mod_ru <- fit_lgbm(make_matrix(fit_data, RUSH_DIRECT_FEATURES),
                     fit_data$rush_epa,
                     tl$rush_dir_leaves, tl$rush_dir_lr, tl$rush_dir_min_node,
                     max(REFIT_ROUNDS_MIN, tl$rush_dir_rounds))

  p_cal  <- list(pe = predict(mod_pe, make_matrix(cal_data, PASS_EFF_B)),
                 db = predict(mod_db, make_matrix(cal_data, DB_VOL_FEATURES)),
                 ca = predict(mod_ca, make_matrix(cal_data, CARRY_VOL_FEATURES)),
                 ru = predict(mod_ru, make_matrix(cal_data, RUSH_DIRECT_FEATURES)))
  p_test <- list(pe = predict(mod_pe, make_matrix(test_data, PASS_EFF_B)),
                 db = predict(mod_db, make_matrix(test_data, DB_VOL_FEATURES)),
                 ca = predict(mod_ca, make_matrix(test_data, CARRY_VOL_FEATURES)),
                 ru = predict(mod_ru, make_matrix(test_data, RUSH_DIRECT_FEATURES)))

  pred_cal_tot  <- p_cal$pe  * p_cal$db  + p_cal$ru
  pred_test_tot <- p_test$pe * p_test$db + p_test$ru

  qs_tot <- q3(abs(cal_data$total_epa - pred_cal_tot))
  # pass_eff conformal arms (08c mechanism) -- the component that changed;
  # 09a needs its 7-point frame. db/rush/carry arms merge from shipped (13e).
  qs_pe <- q3(abs(cal_data$pass_epa_per_db_obs - p_cal$pe))

  base <- test_data |>
    select(player_id, season, week, dropbacks, carries, wt_carries, total_epa,
           team_spread, implied_total) |>
    mutate(pred_pass_eff = p_test$pe, pred_db = p_test$db,
           pred_carry = p_test$ca, pred_rush = p_test$ru,
           pred_tot = pred_test_tot)
  for (i in seq_along(qs_tot)) {
    cv <- c("50", "80", "90")[i]
    base[[paste0("lo_", cv, "_tot")]] <- pred_test_tot - qs_tot[i]
    base[[paste0("hi_", cv, "_tot")]] <- pred_test_tot + qs_tot[i]
    base[[paste0("lo_", cv, "_pass_eff")]] <- p_test$pe - qs_pe[i]
    base[[paste0("hi_", cv, "_pass_eff")]] <- p_test$pe + qs_pe[i]
  }
  fold_results[[f]] <- base |> mutate(fold = f)

  tune_rows[[f]] <- tibble(fold = f,
    pe_leaves = best_pe$num_leaves, pe_lr = best_pe$lr,
    pe_min_node = best_pe$min_data_in_leaf, pe_rounds = best_pe$rounds,
    pe_inner_rmse = round(best_pe$inner_rmse, 4))

  if (f %% 25 == 0 || f == nrow(fold_map)) {
    cli_alert_info("Fold {sprintf('%03d', f)} [{ts}-W{sprintf('%02d', tw)}] ({round(proc.time()[['elapsed']] - t0, 1)}s/fold)")
  }
}

results_b <- bind_rows(fold_results)
tune_b    <- bind_rows(tune_rows)

# ===========================================================================
# 3. INTEGRITY: NON-TUNED COMPONENT REPRODUCTION
# ===========================================================================

cli_h1("Integrity")

if (nrow(results_b) != EXPECTED_TEST_N) {
  cli_warn("Row count {nrow(results_b)} vs expected {EXPECTED_TEST_N}")
} else cli_alert_success("Row count {nrow(results_b)} / {EXPECTED_TEST_N}")

repro <- results_b |>
  select(player_id, season, week, b_db = pred_db, b_ca = pred_carry, b_ru = pred_rush) |>
  inner_join(shipped |> select(player_id, season, week, pred_db, pred_carry, pred_rush),
             by = c("player_id", "season", "week"))
max_diff <- max(abs(repro$b_db - repro$pred_db), abs(repro$b_ca - repro$pred_carry),
                abs(repro$b_ru - repro$pred_rush))
cli_alert_info("Non-tuned component reproduction: {nrow(repro)} rows, max |diff| = {format(max_diff, scientific = TRUE)}")
if (max_diff > 1e-6) cli_abort("Component reproduction failed -- procedure drift.")

# ===========================================================================
# 4. A/B TABLES + VERDICT
# ===========================================================================

cli_h1("A/B scoring")

itotal_bucket <- function(it) cut(it, c(-Inf, 20, 26, Inf),
  labels = c("low_implied", "mid_implied", "high_implied"))

ab_rows <- shipped |>
  select(player_id, season, week, total_epa, wt_carries,
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
         ib = itotal_bucket(implied_total),
         exante_tier = cut(wt_carries, TIER_BREAKS, labels = TIER_LABELS))

cli_alert_success("Matched A/B rows: {nrow(ab_rows)}")

grad_tbl <- ab_rows |>
  filter(!is.na(ib)) |>
  group_by(ib) |>
  summarise(n = n(), arm_a = mean(resid_a), arm_b = mean(resid_b), .groups = "drop")
grad_a <- grad_tbl$arm_a[grad_tbl$ib == "high_implied"] - grad_tbl$arm_a[grad_tbl$ib == "low_implied"]
grad_b <- grad_tbl$arm_b[grad_tbl$ib == "high_implied"] - grad_tbl$arm_b[grad_tbl$ib == "low_implied"]

cli_h2("PRIMARY: combined residual by implied-total bucket")
print(grad_tbl |> mutate(across(c(arm_a, arm_b), ~ round(.x, 3))) |>
        as.data.frame(), row.names = FALSE)
cli_alert_info("Gradient: arm A = {round(grad_a, 3)} | arm B = {round(grad_b, 3)} | shrink = {round(100 * (1 - abs(grad_b) / abs(grad_a)), 1)}%")

pi6 <- function(df, pre) df |> transmute(
  lo_50 = .data[[paste0("lo_50_", pre)]], hi_50 = .data[[paste0("hi_50_", pre)]],
  lo_80 = .data[[paste0("lo_80_", pre)]], hi_80 = .data[[paste0("hi_80_", pre)]],
  lo_90 = .data[[paste0("lo_90_", pre)]], hi_90 = .data[[paste0("hi_90_", pre)]])

cov_b_pool <- eval_calibration(ab_rows$total_epa, pi6(ab_rows, "tot"))
cov_a_pool <- eval_calibration(ab_rows$total_epa,
  ab_rows |> transmute(lo_50 = lo_50_a, hi_50 = hi_50_a, lo_80 = lo_80_a,
                       hi_80 = hi_80_a, lo_90 = lo_90_a, hi_90 = hi_90_a))
b80 <- cov_b_pool |> filter(nominal == 0.80)
a80 <- cov_a_pool |> filter(nominal == 0.80)

tier_cov <- ab_rows |>
  filter(!is.na(exante_tier)) |>
  group_by(exante_tier) |>
  group_modify(~ eval_calibration(.x$total_epa, pi6(.x, "tot")) |>
                 filter(nominal == 0.80)) |>
  ungroup()

cli_h2("RUBRIC: arm B coverage")
cli_alert_info("Pooled 80%: arm B {fmt_pp(b80$delta)} w={round(b80$sharpness, 2)} | arm A {fmt_pp(a80$delta)} w={round(a80$sharpness, 2)}")
print(tier_cov |> select(exante_tier, empirical, delta, sharpness) |>
        mutate(across(where(is.numeric), ~ round(.x, 3))) |>
        as.data.frame(), row.names = FALSE)

rmse_a <- sqrt(mean(ab_rows$resid_a^2)); rmse_b <- sqrt(mean(ab_rows$resid_b^2))
cli_alert_info("Combined RMSE: A {round(rmse_a, 3)} | B {round(rmse_b, 3)} ({sprintf('%+.2f%%', 100 * (rmse_b / rmse_a - 1))})")

cli_h1("13c verdict (pre-committed rule)")

rubric_ok    <- abs(b80$delta) <= 0.02 && all(abs(tier_cov$delta) <= 0.10)
gradient_ok  <- abs(grad_b) <= 0.5 * abs(grad_a)
sharpness_ok <- b80$sharpness <= a80$sharpness * 1.02

cli_alert_info("1. Rubric intact:     {if (rubric_ok) 'PASS' else 'FAIL'} (pooled {fmt_pp(b80$delta)}; tier max {fmt_pp(max(abs(tier_cov$delta)))})")
cli_alert_info("2. Gradient >= 50%:   {if (gradient_ok) 'PASS' else 'FAIL'} ({round(grad_a, 2)} -> {round(grad_b, 2)} EPA)")
cli_alert_info("3. Sharpness +2% cap: {if (sharpness_ok) 'PASS' else 'FAIL'} ({round(a80$sharpness, 2)} -> {round(b80$sharpness, 2)})")

if (rubric_ok && gradient_ok && sharpness_ok) {
  cli_alert_success("QB PASSES rung 2 at the EPA layer (deployment blocked on point-in-time lines).")
} else {
  cli_alert_warning("QB does NOT pass -- publish the receipt as-is.")
}

cli_h1("Save receipts")
stem <- sprintf("output/13c_qb_vegas%s",
                if (nzchar(VEGAS_TAG)) paste0("_", VEGAS_TAG) else "")
readr::write_csv(results_b, paste0(stem, "_fold_predictions.csv"))
readr::write_csv(tune_b,    paste0(stem, "_tune_log.csv"))
readr::write_csv(grad_tbl |> mutate(position = "QB", grad_a = grad_a, grad_b = grad_b,
                                    rmse_a = rmse_a, rmse_b = rmse_b,
                                    pooled80_delta_b = b80$delta,
                                    sharp80_a = a80$sharpness, sharp80_b = b80$sharpness),
                paste0(stem, "_ab_table.csv"))
cli_alert_success("{stem}_*.csv")

cli_h1("13c complete")
