# R/05_wr_3c_hierarchical_bayes.R
# Step 5: WR upside model -- Hierarchical Bayes + Student-t (clone of 03c for WRs).
#
# PURPOSE: This is the engine for Product 2 (upside model) applied to wide receivers.
# The question: "Which low-target WR has a tail worth streaming?"
# Audience: flex/streamer decisions on committee WRs with 3-5 target floors.
#
# CLONES 03c_hierarchical_bayes.R with WR-specific adjustments:
#   - Input: data/wr_feature_table.rds
#   - EFF_FEATURES: drop def_rush_epa_adj; add wt_air_yards_per_target
#   - sigma submodel: log_opp_scaled + wt_air_yards_per_target (route depth drives WR variance)
#   - VOL_FEATURES: wt_target_share + wt_air_yards_share (no wt_carry_share)
#   - BF_VOL formula: WR volume signals replace RB carry signals
#   - LOW_OPP bucket: 3-5 targets (vs 5-8 carries for RBs)
#   - Volume prior intercept: normal(6, 4) to reflect WR target scale
#   - Comparison: WR 3C vs 04b-WR (not RB 3A/3B)
#
# DO NOT apply power-law conformal. DO NOT fix nu. DO NOT tune priors toward
# passing the veto. Overcoverage or undercoverage is a finding, not a failure.

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(posterior)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS
# ===========================================================================

LOW_OPP_LO <- 3L
LOW_OPP_HI <- 5L

EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_air_yards_per_target",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

VOL_FEATURES <- c(
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_team_total_plays",
  "def_short_pass_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

EFF_SCALE_COLS <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "wt_air_yards_per_target", "wt_snap_share", "games_played_so_far",
  "def_short_pass_epa_adj", "def_deep_pass_epa_adj"
)

VOL_SCALE_COLS <- c(
  "wt_target_share", "wt_air_yards_share", "wt_snap_share", "wt_team_total_plays",
  "def_short_pass_epa_adj", "games_played_so_far"
)

MCMC_CHAINS  <- 2L
MCMC_ITER    <- 1500L
MCMC_WARMUP  <- 750L
MCMC_CORES   <- 2L
MCMC_SEED    <- 42L
BACKEND      <- "cmdstanr"

RHAT_THRESH     <- 1.01
DIV_WARN_THRESH <- 10L

# ===========================================================================
# FORMULAS
# ===========================================================================

BF_EFF <- bf(
  epa_per_opp_obs ~
    prior_epa_per_opp + baseline_epa_per_opp + rolling_epa_per_opp + form_residual +
    is_cold_start_int + draft_tier_int +
    def_short_pass_epa_adj + def_deep_pass_epa_adj +
    wt_air_yards_per_target + wt_snap_share + games_played_so_far + def_used_fallback_int +
    (1 | player_id) + (1 | defteam),
  sigma ~ log_opp_scaled + wt_air_yards_per_target,
  family = student()
)

BF_VOL <- bf(
  opp_raw ~
    wt_target_share + wt_air_yards_share + wt_snap_share + wt_team_total_plays +
    def_short_pass_epa_adj + draft_tier_int + is_cold_start_int + games_played_so_far +
    (1 | player_id) + (1 | defteam),
  family = gaussian()
)

# ===========================================================================
# PRIORS
# ===========================================================================

PRIOR_EFF <- c(
  prior(normal(0,   0.3), class = b),
  prior(normal(0,   0.5), class = Intercept),
  prior(normal(0,   0.3), class = sd),
  prior(normal(-1,  0.5), class = Intercept, dpar = sigma),
  prior(normal(0,   0.3), class = b,         dpar = sigma),
  prior(gamma(2,  0.1),  class = nu)
)

# Volume prior: WR targets per game average ~5-7 in a >=3 target sample.
# Intercept shifted down from the RB prior (10, 5) accordingly.
PRIOR_VOL <- c(
  prior(normal(0,   3),   class = b),
  prior(normal(6,   4),   class = Intercept),
  prior(normal(0,   3),   class = sd),
  prior(normal(0,   5),   class = sigma)
)

# ===========================================================================
# HELPERS  (identical to 03c)
# ===========================================================================

TIER_ORDER <- c("udfa" = 1L, "r6_udfa" = 2L, "r4_5" = 3L, "r2_3" = 4L, "r1" = 5L)

encode_features <- function(df) {
  df |>
    mutate(
      draft_tier_int        = TIER_ORDER[draft_tier],
      is_cold_start_int     = as.integer(is_cold_start),
      def_used_fallback_int = as.integer(def_used_fallback),
      opp_raw               = as.numeric(opportunities)
    )
}

standardize_to_train <- function(train_df, test_df, scale_cols) {
  mu <- sapply(scale_cols, function(c) mean(train_df[[c]], na.rm = TRUE))
  sg <- sapply(scale_cols, function(c) max(sd(train_df[[c]], na.rm = TRUE), 1e-8))
  apply_scale <- function(df) {
    for (col in scale_cols) df[[col]] <- (df[[col]] - mu[[col]]) / sg[[col]]
    df
  }
  list(train = apply_scale(train_df), test = apply_scale(test_df))
}

impute_to_train_median <- function(train_df, test_df, feature_cols) {
  medians <- sapply(feature_cols, function(c) median(train_df[[c]], na.rm = TRUE))
  apply_impute <- function(df) {
    for (col in feature_cols) {
      na_idx <- is.na(df[[col]])
      if (any(na_idx)) df[[col]][na_idx] <- medians[[col]]
    }
    df
  }
  list(train = apply_impute(train_df), test = apply_impute(test_df))
}

add_log_opp_scaled <- function(train_df, test_df) {
  log_opp_train <- log(train_df$opp_raw)
  mu <- mean(log_opp_train, na.rm = TRUE)
  sg <- max(sd(log_opp_train, na.rm = TRUE), 1e-8)
  train_df$log_opp_scaled <- (log(train_df$opp_raw) - mu) / sg
  test_df$log_opp_scaled  <- (log(test_df$opp_raw)  - mu) / sg
  list(train = train_df, test = test_df)
}

extract_posterior_pi <- function(pp_mat, suffix) {
  n_rows <- ncol(pp_mat)
  out    <- vector("list", n_rows)
  for (j in seq_len(n_rows)) {
    col_j <- pp_mat[, j]
    out[[j]] <- c(
      mean(col_j),
      quantile(col_j, c(0.25, 0.75, 0.10, 0.90, 0.05, 0.95), names = FALSE)
    )
  }
  mat <- do.call(rbind, out)
  colnames(mat) <- c("pred", "lo50", "hi50", "lo80", "hi80", "lo90", "hi90")
  tbl <- as_tibble(mat)
  names(tbl) <- c(
    paste0("pred_", suffix),
    paste0("lo_50_", suffix), paste0("hi_50_", suffix),
    paste0("lo_80_", suffix), paste0("hi_80_", suffix),
    paste0("lo_90_", suffix), paste0("hi_90_", suffix)
  )
  tbl
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

check_convergence <- function(fit, fold_id, label) {
  rh  <- max(rhat(fit), na.rm = TRUE)
  div <- tryCatch({
    np <- nuts_params(fit)
    sum(np$Parameter == "divergent__" & np$Value > 0)
  }, error = function(e) NA_integer_)

  if (!is.na(rh) && rh > RHAT_THRESH)
    cli_warn("Fold {fold_id} [{label}]: R-hat max = {round(rh, 4)} > {RHAT_THRESH}")
  if (!is.na(div) && div > DIV_WARN_THRESH)
    cli_warn("Fold {fold_id} [{label}]: {div} divergent transitions")

  tibble(fold = fold_id, model = label, rhat_max = rh, n_divergent = div)
}

fmt_pp <- function(x) sprintf("%+.1fpp", x * 100)
fmt_w  <- function(x) round(x, 3)

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 5: WR Hierarchical Bayes + Student-t (upside model)")
cli_alert_info("Efficiency: Student-t likelihood, sigma ~ log_opp + wt_air_yards_per_target, (1|player_id) + (1|defteam)")
cli_alert_info("Volume:     Gaussian, WR target signals, (1|player_id) + (1|defteam)")
cli_alert_info("Combined:   posterior predictive draws (pp_eff * pp_vol) -- NO conformal step")
cli_alert_info("Backend: {BACKEND} | chains={MCMC_CHAINS} | iter={MCMC_ITER} | warmup={MCMC_WARMUP}")

ft       <- readRDS("data/wr_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- ft |>
  semi_join(fold_map, by = c("season" = "test_season", "week" = "test_week")) |>
  nrow()

cli_alert_success("WR feature table: {nrow(ft)} rows x {ncol(ft)} cols")
cli_alert_success("Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

missing_eff <- setdiff(EFF_FEATURES, names(ft))
missing_vol <- setdiff(VOL_FEATURES, names(ft))
if (length(missing_eff) > 0) cli::cli_abort("Missing EFF_FEATURES: {paste(missing_eff, collapse=', ')}")
if (length(missing_vol) > 0) cli::cli_abort("Missing VOL_FEATURES: {paste(missing_vol, collapse=', ')}")
cli_alert_success("All model features present")

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds -- MCMC, expect ~30-60 min)")

fold_results    <- vector("list", nrow(fold_map))
conv_diag       <- vector("list", nrow(fold_map))
nu_log               <- numeric(nrow(fold_map))
sigma_slope_log_opp  <- numeric(nrow(fold_map))
sigma_slope_log_adot <- numeric(nrow(fold_map))

mod_eff <- NULL
mod_vol <- NULL

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
  if (length(overlap) > 0L) cli_abort("Fold {f}: train/test season-week overlap: {overlap}")

  ALL_FEAT_COLS <- unique(c(EFF_FEATURES, VOL_FEATURES, EFF_SCALE_COLS, VOL_SCALE_COLS))
  imputed    <- impute_to_train_median(train_data, test_data, intersect(ALL_FEAT_COLS, names(train_data)))
  train_data <- imputed$train
  test_data  <- imputed$test

  scaled_eff <- standardize_to_train(train_data, test_data, EFF_SCALE_COLS)
  scaled_vol <- standardize_to_train(scaled_eff$train, scaled_eff$test, VOL_SCALE_COLS)
  lopped     <- add_log_opp_scaled(scaled_vol$train, scaled_vol$test)
  tr         <- lopped$train
  te         <- lopped$test

  if (is.null(mod_eff)) {
    cli_alert_info("Fold {sprintf('%02d', f)}: compiling efficiency model (one-time)...")
    mod_eff <- brm(
      formula = BF_EFF,
      data    = tr,
      prior   = PRIOR_EFF,
      chains  = MCMC_CHAINS, iter = MCMC_ITER, warmup = MCMC_WARMUP,
      cores   = MCMC_CORES,  seed = MCMC_SEED, backend = BACKEND,
      silent  = 2, refresh = 0,
      control = list(adapt_delta = 0.95)
    )
  } else {
    mod_eff <- update(mod_eff, newdata = tr, recompile = FALSE,
                      seed = MCMC_SEED, refresh = 0, silent = 2,
                      control = list(adapt_delta = 0.95))
  }

  if (is.null(mod_vol)) {
    cli_alert_info("Fold {sprintf('%02d', f)}: compiling volume model (one-time)...")
    mod_vol <- brm(
      formula = BF_VOL,
      data    = tr,
      prior   = PRIOR_VOL,
      chains  = MCMC_CHAINS, iter = MCMC_ITER, warmup = MCMC_WARMUP,
      cores   = MCMC_CORES,  seed = MCMC_SEED, backend = BACKEND,
      silent  = 2, refresh = 0
    )
  } else {
    mod_vol <- update(mod_vol, newdata = tr, recompile = FALSE,
                      seed = MCMC_SEED, refresh = 0, silent = 2)
  }

  conv_eff <- check_convergence(mod_eff, f, "eff")
  conv_vol <- check_convergence(mod_vol, f, "vol")
  conv_diag[[f]] <- bind_rows(conv_eff, conv_vol)

  post_nu       <- as_draws_matrix(mod_eff, variable = "nu")[, 1]
  nu_log[f]     <- median(post_nu)

  post_sigma_slope_opp <- tryCatch(
    as_draws_matrix(mod_eff, variable = "b_sigma_log_opp_scaled")[, 1],
    error = function(e) NA_real_
  )
  post_sigma_slope_adot <- tryCatch(
    as_draws_matrix(mod_eff, variable = "b_sigma_wt_air_yards_per_target")[, 1],
    error = function(e) NA_real_
  )
  sigma_slope_log_opp[f]  <- median(post_sigma_slope_opp,  na.rm = TRUE)
  sigma_slope_log_adot[f] <- median(post_sigma_slope_adot, na.rm = TRUE)

  pp_eff <- posterior_predict(mod_eff, newdata = te, allow_new_levels = TRUE, ndraws = NULL)
  pp_vol <- posterior_predict(mod_vol, newdata = te, allow_new_levels = TRUE, ndraws = NULL)

  n_neg <- sum(pp_vol < 1.0)
  if (n_neg > 0) {
    frac_neg <- n_neg / length(pp_vol)
    if (frac_neg > 0.001)
      cli_warn("Fold {f}: {n_neg} volume draws < 1 ({round(frac_neg*100, 2)}%), clamped to 1.0")
    pp_vol <- pmax(pp_vol, 1.0)
  }

  pp_tot <- pp_eff * pp_vol

  pi_eff <- extract_posterior_pi(pp_eff, "eff")
  pi_vol <- extract_posterior_pi(pp_vol, "vol")
  pi_tot <- extract_posterior_pi(pp_tot, "tot")

  fold_results[[f]] <- te |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(pi_eff, pi_vol, pi_tot) |>
    mutate(fold = f)

  t1 <- proc.time()[["elapsed"]]
  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(te)} rows | nu={round(nu_log[f], 1)} | sigma_opp={round(sigma_slope_log_opp[f], 3)} | sigma_adot={round(sigma_slope_log_adot[f], 3)} | {round(t1-t0)}s"
  )
}

results  <- bind_rows(fold_results)
conv_all <- bind_rows(conv_diag)

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

n_rhat_fails <- sum(conv_all$rhat_max > RHAT_THRESH, na.rm = TRUE)
n_div_fails  <- sum(conv_all$n_divergent > DIV_WARN_THRESH, na.rm = TRUE)
if (n_rhat_fails == 0L) {
  cli_alert_success("R-hat: all {nrow(conv_all)} model-folds below {RHAT_THRESH}")
} else {
  cli_warn("{n_rhat_fails} model-folds with R-hat > {RHAT_THRESH}")
  conv_all |>
    filter(rhat_max > RHAT_THRESH) |>
    mutate(row = paste0("  fold=", fold, " model=", model, " rhat=", round(rhat_max, 4))) |>
    pull(row) |> walk(cli_alert_info)
}
if (n_div_fails == 0L) {
  cli_alert_success("Divergent transitions: no folds exceeded {DIV_WARN_THRESH}")
} else {
  cli_warn("{n_div_fails} model-folds with > {DIV_WARN_THRESH} divergent transitions")
}

cli_alert_info("Efficiency nu (Student-t df) across {nrow(fold_map)} folds:")
cli_alert_info(
  "  range [{round(min(nu_log),1)}, {round(max(nu_log),1)}] | median {round(median(nu_log),1)} | mean {round(mean(nu_log),1)}"
)
if (median(nu_log) < 5) {
  cli_alert_success("Median nu < 5: heavy tails in sample -- WR per-target distribution is genuinely fat-tailed")
} else if (median(nu_log) > 30) {
  cli_alert_info("Median nu > 30: effectively Gaussian in sample")
} else {
  cli_alert_info("Median nu = {round(median(nu_log),1)}: moderate tail weight")
}

cli_alert_info("sigma slope: log_opp (negative = more targets -> smaller sigma):")
cli_alert_info(
  "  range [{round(min(sigma_slope_log_opp, na.rm=T),3)}, {round(max(sigma_slope_log_opp, na.rm=T),3)}] | median {round(median(sigma_slope_log_opp, na.rm=T),3)}"
)
cli_alert_info("sigma slope: wt_air_yards_per_target (positive = deeper routes -> wider sigma):")
cli_alert_info(
  "  range [{round(min(sigma_slope_log_adot, na.rm=T),3)}, {round(max(sigma_slope_log_adot, na.rm=T),3)}] | median {round(median(sigma_slope_log_adot, na.rm=T),3)}"
)

# ===========================================================================
# COVERAGE SCORING
# ===========================================================================

cli_h1("5-WR Pooled Coverage (all {n_scored} test rows)")

pooled_wr3c <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_wr3c |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("5-WR Low-Usage Bucket (targets {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low  <- nrow(res_lo)
cli_alert_info("Low-usage rows: {n_low}")

low_wr3c <- bind_rows(
  score_component(res_lo$epa_per_opp_obs,           res_lo, "eff", "efficiency"),
  score_component(as.numeric(res_lo$opportunities), res_lo, "vol", "volume"),
  score_component(res_lo$total_epa,                 res_lo, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_wr3c |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# Stratified combined coverage
res_strat <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
      opportunities <= 9L         ~ "mid  (6-9)",
      TRUE                        ~ "high (10+)"
    ) |> factor(levels = c(paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
                            "mid  (6-9)", "high (10+)"))
  )

strat_wr3c <- eval_calibration_stratified(
  res_strat$total_epa,
  pi_cols(res_strat, "tot"),
  strata = res_strat$opp_bucket
) |> mutate(component = "combined")

cli_h2("Stratified combined coverage at 80%")
print(strat_wr3c |> filter(nominal == 0.80) |>
      select(stratum, n, empirical, delta, sharpness) |>
      mutate(delta_pp = fmt_pp(delta)), n = Inf)

# ===========================================================================
# COMPARISON: WR 3C (upside) vs WR 4b (projection)
# ===========================================================================

cli_h1("Comparison: WR 3C (upside model) vs WR 4b (projection model)")

pooled_4b <- readr::read_csv("output/04b_wr_lgbm_pooled_coverage.csv",    show_col_types = FALSE)
low_4b    <- readr::read_csv("output/04b_wr_lgbm_low_usage_coverage.csv", show_col_types = FALSE)
preds_4b  <- readr::read_csv("output/04b_wr_lgbm_fold_predictions.csv",   show_col_types = FALSE)

res_lo_4b <- preds_4b |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)

score_lo_combined <- function(preds_lo, label) {
  bind_rows(
    score_component(preds_lo$epa_per_opp_obs,           preds_lo, "eff", "efficiency"),
    score_component(as.numeric(preds_lo$opportunities), preds_lo, "vol", "volume"),
    score_component(preds_lo$total_epa,                 preds_lo, "tot", "combined")
  ) |> mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI), model = label)
}

lo_4b_scored   <- score_lo_combined(res_lo_4b,  "4b-WR-LightGBM")
lo_wr3c_scored <- low_wr3c |> mutate(model = "5-WR-HierBayes")
lo_compare     <- bind_rows(lo_4b_scored, lo_wr3c_scored)

cli_h2("Low-Usage Efficiency Coverage ({LOW_OPP_LO}-{LOW_OPP_HI} targets)")
cli_alert_info("model              | 50% delta | 80% delta | 90% delta | 80% width")

lo_eff_cmp <- lo_compare |> filter(component == "efficiency") |>
  select(model, nominal, delta, sharpness) |>
  pivot_wider(names_from = nominal, values_from = c(delta, sharpness),
              names_glue = "{.value}_{nominal}") |>
  mutate(across(starts_with("delta"), fmt_pp),
         across(starts_with("sharpness"), fmt_w))

lo_eff_cmp |>
  mutate(row = paste0(model, " | ", delta_0.5, " | ", delta_0.8, " | ", delta_0.9,
                      " | ", sharpness_0.8)) |>
  pull(row) |> walk(cli_alert_info)

cli_h2("Pooled Combined Coverage (all test rows)")
cli_alert_info("model              | 50% delta | 80% delta | 80% width")

pool_cmp <- bind_rows(
  pooled_4b   |> filter(component == "combined") |> mutate(model = "4b-WR-LightGBM"),
  pooled_wr3c |> filter(component == "combined") |> mutate(model = "5-WR-HierBayes")
) |>
  select(model, nominal, delta, sharpness) |>
  pivot_wider(names_from = nominal, values_from = c(delta, sharpness),
              names_glue = "{.value}_{nominal}") |>
  mutate(across(starts_with("delta"), fmt_pp),
         across(starts_with("sharpness"), fmt_w))

pool_cmp |>
  mutate(row = paste0(model, " | ", delta_0.5, " | ", delta_0.8, " | ", sharpness_0.8)) |>
  pull(row) |> walk(cli_alert_info)

# Upside model product verdict: 3C is not competing against 4b on the same rubric.
# 3C's job is accurate per-touch tail for LOW-opp WRs.
# Flag the efficiency coverage for the low bucket -- that's the key number for this product.
eff_lo_80_3c <- lo_wr3c_scored |> filter(component == "efficiency", nominal == 0.80) |> pull(delta)
eff_lo_80_4b <- lo_4b_scored   |> filter(component == "efficiency", nominal == 0.80) |> pull(delta)

cli_h1("Upside Model Verdict")
cli_alert_info("WR 3C low-opp efficiency 80%: {fmt_pp(eff_lo_80_3c)}")
cli_alert_info("WR 4b low-opp efficiency 80%: {fmt_pp(eff_lo_80_4b)} (projection model, for reference)")

if (eff_lo_80_3c >= -0.03) {
  cli_alert_success(
    "3C low-opp efficiency is honest ({fmt_pp(eff_lo_80_3c)}). WR upside model captures the per-target tail."
  )
} else if (eff_lo_80_3c > eff_lo_80_4b + 0.03) {
  cli_alert_success(
    "3C improves on 4b for low-opp efficiency ({fmt_pp(eff_lo_80_3c)} vs {fmt_pp(eff_lo_80_4b)}). WR upside model is working."
  )
} else {
  cli_alert_info(
    "3C low-opp efficiency {fmt_pp(eff_lo_80_3c)}: review stratified table before deploying as upside engine."
  )
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,     "output/05_wr_hier_fold_predictions.csv")
readr::write_csv(pooled_wr3c, "output/05_wr_hier_pooled_coverage.csv")
readr::write_csv(low_wr3c,    "output/05_wr_hier_low_usage_coverage.csv")
readr::write_csv(conv_all,    "output/05_wr_hier_convergence_diag.csv")
readr::write_csv(
  tibble(
    fold              = seq_along(nu_log),
    nu_median         = nu_log,
    sigma_slope_opp   = sigma_slope_log_opp,
    sigma_slope_adot  = sigma_slope_log_adot
  ),
  "output/05_wr_hier_nu_log.csv"
)

cli_alert_success("output/05_wr_hier_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/05_wr_hier_pooled_coverage.csv")
cli_alert_success("output/05_wr_hier_low_usage_coverage.csv")
cli_alert_success("output/05_wr_hier_convergence_diag.csv")
cli_alert_success("output/05_wr_hier_nu_log.csv")

cli_h1("Step 5 complete -- WR upside model (3C architecture)")
