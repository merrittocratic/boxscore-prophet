# R/03c_hierarchical_bayes.R
# Step 3C: Hierarchical mixed-effects + Student-t likelihood (paradigm comparison).
#
# This is NOT another conformal run. The intervals come from the posterior predictive
# distribution of a generative Bayesian model -- a different philosophy from 3A/3B.
# That is the intended confound of the paradigm comparison.
#
# PRIMARY SCIENTIFIC QUESTION: does a heavy-tail-capable generative model cover
# low-opp (5-8) efficiency where both conformal learners (3A LightGBM -8.7pp,
# 3B RF -10.8pp) missed? Three reads are pre-committed:
#   - 3C also undercovers ~10pp: confirms irreducible noise floor (two families, same miss)
#   - 3C covers honestly: miss was tree-family artifact, heavy tail IS learnable here
#   - 3C overcorrects (wide intervals): admits the tail but not yet competitively sharp
#
# EFFICIENCY MODEL DESIGN -- WHY Student-t + sigma ~ log_opp:
#   (a) Student-t likelihood with estimated nu: the model can choose heavy tails when
#       the data demands them. A Gaussian would refuse to widen; nu is never fixed --
#       if it converges to 30+, that IS the answer (the tail is not heavy in-sample).
#   (b) sigma ~ log_opp_scaled: variance is allowed to DECREASE with touch count.
#       A committee back (6 touches) gets wider sigma than a workhorse (18 touches)
#       from the same week. This directly encodes "Damien Harris on 6 carries can
#       go 60 yards or get stuffed twice; Derrick Henry on 22 averages it out."
#       log_opp_scaled uses train-fold mean/sd, so the sigma coefficient is
#       estimable on the log scale without the heavy carry count dominating.
#
# VOLUME MODEL: Gaussian with player/defense random effects. Volume was calibrated
# in 3A/3B; the paradigm change on efficiency is the question.
#
# COMBINED: posterior predictive draws multiplied row-by-row (pp_eff * pp_vol).
# No conformal step. Treating eff and vol as independent within each draw (separate
# models) -- standard approximation when not fitting a joint model.
#
# DO NOT: apply power-law conformal to 3C. DO NOT fix nu. DO NOT tune priors
# toward passing the veto. Bad calibration is a finding.

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(posterior)
  library(cli)
})

source("R/metrics.R")

# ===========================================================================
# PARAMETERS -- all pre-committed
# ===========================================================================

# Shared fold constants (match 3A/3B exactly)
LOW_OPP_LO <- 5L
LOW_OPP_HI <- 8L

EFF_FEATURES <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "is_cold_start_int", "draft_tier_int",
  "def_rush_epa_adj", "def_short_pass_epa_adj", "def_deep_pass_epa_adj",
  "wt_snap_share", "games_played_so_far", "def_used_fallback_int"
)

VOL_FEATURES <- c(
  "wt_carry_share", "wt_target_share", "wt_snap_share", "wt_team_total_plays",
  "def_rush_epa_adj", "draft_tier_int", "is_cold_start_int", "games_played_so_far"
)

# Continuous efficiency features to standardize fold-by-fold.
# wt_team_total_plays (sd=5) and games_played_so_far (sd=5) would otherwise
# violate the normal(0,1) priors on b -- standardization keeps the scale honest.
EFF_SCALE_COLS <- c(
  "prior_epa_per_opp", "baseline_epa_per_opp", "rolling_epa_per_opp", "form_residual",
  "wt_snap_share", "games_played_so_far",
  "def_rush_epa_adj", "def_short_pass_epa_adj", "def_deep_pass_epa_adj"
)

VOL_SCALE_COLS <- c(
  "wt_carry_share", "wt_target_share", "wt_snap_share", "wt_team_total_plays",
  "def_rush_epa_adj", "games_played_so_far"
)

# MCMC settings: 2 chains x 750 post-warmup = 1500 draws. Enough for reliable
# 5th/95th percentile estimation (quantile se < 0.01 for n_draws=1500).
MCMC_CHAINS  <- 2L
MCMC_ITER    <- 1500L
MCMC_WARMUP  <- 750L
MCMC_CORES   <- 2L
MCMC_SEED    <- 42L
BACKEND      <- "cmdstanr"

# R-hat threshold and veto for convergence reporting
RHAT_THRESH     <- 1.01
DIV_WARN_THRESH <- 10L   # warn if more than this many divergent transitions

# ===========================================================================
# FORMULAS -- defined once, reused via update() across folds
# ===========================================================================

# Efficiency: Student-t with sigma distributional term.
# log_opp_scaled is derived per fold from log(opportunities); it is available at
# prediction time because this is retrospective evaluation (actual opp is known).
# Deployment would need predicted opp in sigma -- same caveat as 3A/3B power-law.
BF_EFF <- bf(
  epa_per_opp_obs ~
    prior_epa_per_opp + baseline_epa_per_opp + rolling_epa_per_opp + form_residual +
    is_cold_start_int + draft_tier_int +
    def_rush_epa_adj + def_short_pass_epa_adj + def_deep_pass_epa_adj +
    wt_snap_share + games_played_so_far + def_used_fallback_int +
    (1 | player_id) + (1 | defteam),
  sigma ~ log_opp_scaled,
  family = student()
)

# Volume: Gaussian -- paradigm change is the story for efficiency;
# volume holds same random effects structure.
BF_VOL <- bf(
  opp_raw ~
    wt_carry_share + wt_target_share + wt_snap_share + wt_team_total_plays +
    def_rush_epa_adj + draft_tier_int + is_cold_start_int + games_played_so_far +
    (1 | player_id) + (1 | defteam),
  family = gaussian()
)

# ===========================================================================
# PRIORS
# ===========================================================================

# Efficiency priors (features are standardized, so these are on a comparable scale)
PRIOR_EFF <- c(
  # Fixed effects: a 1-SD change in any predictor moves EPA/opp by at most ~0.6
  prior(normal(0,   0.3), class = b),
  # Grand mean: 0 EPA/opp +/- 1 (2-sigma covers basically all backs)
  prior(normal(0,   0.5), class = Intercept),
  # Player and defense random effects: SD ~ 0.1-0.2 is plausible
  prior(normal(0,   0.3), class = sd),
  # sigma intercept: log(sigma) ~ -1 => sigma ~ 0.37, consistent with 3A efficiency widths
  prior(normal(-1,  0.5), class = Intercept, dpar = sigma),
  # sigma slope on log_opp: let data choose direction; -0.3 prior SD is informative
  # but not tight enough to prevent the model from estimating a large negative coeff
  prior(normal(0,   0.3), class = b,         dpar = sigma),
  # nu: gamma(2, 0.1) => mode=10, mean=20, SD~14. Allows nu<5 (heavy tails) when
  # data supports it. Do NOT fix nu -- that would destroy the scientific question.
  prior(gamma(2,  0.1),  class = nu)
)

# Volume priors (features standardized; outcome is raw opportunities 5-41)
PRIOR_VOL <- c(
  # A 1-SD feature change moves opp by ~3 at most (prior SD=3)
  prior(normal(0,   3),   class = b),
  # Grand mean around 10-12 touches (reasonable baseline for a filtered back)
  prior(normal(10,  5),   class = Intercept),
  # Random effect SD for player and defense
  prior(normal(0,   3),   class = sd),
  # Residual SD: typical weekly opp variation is 3-5 touches
  prior(normal(0,   5),   class = sigma)
)

# ===========================================================================
# HELPERS
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

# Standardize scale_cols to train mean/sd; apply same transform to test.
# Binary indicators and the volume outcome (opp_raw) are NOT scaled.
standardize_to_train <- function(train_df, test_df, scale_cols) {
  mu <- sapply(scale_cols, function(c) mean(train_df[[c]], na.rm = TRUE))
  sg <- sapply(scale_cols, function(c) {
    s <- sd(train_df[[c]], na.rm = TRUE)
    max(s, 1e-8)
  })
  apply_scale <- function(df) {
    for (col in scale_cols) df[[col]] <- (df[[col]] - mu[[col]]) / sg[[col]]
    df
  }
  list(train = apply_scale(train_df), test = apply_scale(test_df))
}

# Median imputation using training statistics. brms silently drops NA rows, but
# LightGBM in 3A/3B learns NA splits natively and keeps all rows. Imputing ensures
# 3C trains and predicts on the same row set for a fair comparison.
# Only impute feature columns -- never the outcome.
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

# Compute log_opp_scaled from log(opportunities), standardized to train statistics.
# Used in the sigma submodel -- availability at test time is retrospective-only.
add_log_opp_scaled <- function(train_df, test_df) {
  log_opp_train <- log(train_df$opp_raw)
  mu <- mean(log_opp_train, na.rm = TRUE)
  sg <- max(sd(log_opp_train, na.rm = TRUE), 1e-8)
  train_df$log_opp_scaled <- (log(train_df$opp_raw) - mu) / sg
  test_df$log_opp_scaled  <- (log(test_df$opp_raw)  - mu) / sg
  list(train = train_df, test = test_df)
}

# Extract posterior predictive intervals from a draws x rows matrix.
# Uses quantile-based PI: 50% = [Q25, Q75], 80% = [Q10, Q90], 90% = [Q5, Q95].
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

# Extract convergence diagnostics from a brmsfit.
# Returns a 1-row tibble; warns in place if thresholds exceeded.
check_convergence <- function(fit, fold_id, label) {
  rh  <- max(rhat(fit), na.rm = TRUE)
  div <- tryCatch({
    np <- nuts_params(fit)
    sum(np$Parameter == "divergent__" & np$Value > 0)
  }, error = function(e) NA_integer_)

  if (!is.na(rh) && rh > RHAT_THRESH) {
    cli_warn(
      "Fold {fold_id} [{label}]: R-hat max = {round(rh, 4)} > {RHAT_THRESH} -- samples may be unreliable"
    )
  }
  if (!is.na(div) && div > DIV_WARN_THRESH) {
    cli_warn(
      "Fold {fold_id} [{label}]: {div} divergent transitions -- posterior geometry may be problematic"
    )
  }
  tibble(fold = fold_id, model = label, rhat_max = rh, n_divergent = div)
}

fmt_pp  <- function(x) sprintf("%+.1fpp", x * 100)
fmt_w   <- function(x) round(x, 3)

# ===========================================================================
# LOAD FROZEN INPUTS
# ===========================================================================

cli_h1("Step 3C: Hierarchical Bayes + Student-t (paradigm comparison)")
cli_alert_info("Efficiency: Student-t likelihood, sigma ~ log_opp, (1|player_id) + (1|defteam)")
cli_alert_info("Volume:     Gaussian, (1|player_id) + (1|defteam)")
cli_alert_info("Combined:   posterior predictive draws (pp_eff * pp_vol) -- NO conformal step")
cli_alert_info("Backend: {BACKEND} | chains={MCMC_CHAINS} | iter={MCMC_ITER} | warmup={MCMC_WARMUP}")

ft       <- readRDS("data/rb_feature_table.rds")
fold_map <- readRDS("data/fold_map.rds")

EXPECTED_TEST_N <- sum(fold_map$n_test_rows)
cli_alert_success("Feature table: {nrow(ft)} rows x {ncol(ft)} cols")
cli_alert_success("Fold map: {nrow(fold_map)} folds | Expected test rows: {EXPECTED_TEST_N}")

ft <- encode_features(ft)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward fold loop ({nrow(fold_map)} folds -- MCMC, expect ~30-60 min)")

fold_results  <- vector("list", nrow(fold_map))
conv_diag     <- vector("list", nrow(fold_map))

# nu log: extract per-fold estimated nu for the efficiency Student-t tail.
# If median nu < 5 -> heavy tails in sample; > 30 -> effectively Gaussian.
nu_log        <- numeric(nrow(fold_map))
sigma_slope_log <- numeric(nrow(fold_map))

mod_eff <- NULL
mod_vol <- NULL

for (f in seq_len(nrow(fold_map))) {

  t0 <- proc.time()[["elapsed"]]

  test_season <- fold_map$test_season[f]
  test_week   <- fold_map$test_week[f]

  # Identical split logic as 3A/3B -- no cal/fit split needed; posterior uncertainty
  # comes from the model, not from a holdout calibration set.
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

  # Impute NA features to training median -- ensures 3C uses the same rows as
  # 3A/3B (LightGBM handled NAs natively; brms would silently drop those rows).
  # Apply before standardization so imputed values are scaled consistently.
  ALL_FEAT_COLS <- unique(c(EFF_FEATURES, VOL_FEATURES, EFF_SCALE_COLS, VOL_SCALE_COLS))
  imputed    <- impute_to_train_median(train_data, test_data, intersect(ALL_FEAT_COLS, names(train_data)))
  train_data <- imputed$train
  test_data  <- imputed$test

  # Standardize continuous features (fold-aware: train stats only)
  scaled_eff <- standardize_to_train(train_data, test_data, EFF_SCALE_COLS)
  scaled_vol <- standardize_to_train(scaled_eff$train, scaled_eff$test, VOL_SCALE_COLS)

  # Add log_opp_scaled for sigma submodel (train stats only -- no test leakage)
  lopped    <- add_log_opp_scaled(scaled_vol$train, scaled_vol$test)
  tr        <- lopped$train
  te        <- lopped$test

  # -- Efficiency model: Student-t + sigma ~ log_opp --
  if (is.null(mod_eff)) {
    # Fold 1: compile Stan model (takes ~30-60s first time)
    cli_alert_info("Fold {sprintf('%02d', f)}: compiling efficiency model (one-time)...")
    mod_eff <- brm(
      formula       = BF_EFF,
      data          = tr,
      prior         = PRIOR_EFF,
      chains        = MCMC_CHAINS,
      iter          = MCMC_ITER,
      warmup        = MCMC_WARMUP,
      cores         = MCMC_CORES,
      seed          = MCMC_SEED,
      backend       = BACKEND,
      silent        = 2,
      refresh       = 0,
      control       = list(adapt_delta = 0.95)
    )
  } else {
    mod_eff <- update(mod_eff, newdata = tr, recompile = FALSE,
                      seed = MCMC_SEED, refresh = 0, silent = 2,
                      control = list(adapt_delta = 0.95))
  }

  # -- Volume model: Gaussian with player/defense random effects --
  if (is.null(mod_vol)) {
    cli_alert_info("Fold {sprintf('%02d', f)}: compiling volume model (one-time)...")
    mod_vol <- brm(
      formula       = BF_VOL,
      data          = tr,
      prior         = PRIOR_VOL,
      chains        = MCMC_CHAINS,
      iter          = MCMC_ITER,
      warmup        = MCMC_WARMUP,
      cores         = MCMC_CORES,
      seed          = MCMC_SEED,
      backend       = BACKEND,
      silent        = 2,
      refresh       = 0
    )
  } else {
    mod_vol <- update(mod_vol, newdata = tr, recompile = FALSE,
                      seed = MCMC_SEED, refresh = 0, silent = 2)
  }

  # -- Convergence check --
  conv_eff <- check_convergence(mod_eff, f, "eff")
  conv_vol <- check_convergence(mod_vol, f, "vol")
  conv_diag[[f]] <- bind_rows(conv_eff, conv_vol)

  # -- Log nu (tail weight) and sigma slope (opp-conditional variance) --
  post_nu    <- as_draws_matrix(mod_eff, variable = "nu")[, 1]
  nu_log[f]  <- median(post_nu)

  post_sigma_slope <- tryCatch(
    as_draws_matrix(mod_eff, variable = "b_sigma_log_opp_scaled")[, 1],
    error = function(e) NA_real_
  )
  sigma_slope_log[f] <- median(post_sigma_slope, na.rm = TRUE)

  # -- Posterior predictive draws --
  pp_eff <- posterior_predict(mod_eff, newdata = te, allow_new_levels = TRUE,
                              ndraws = NULL)  # all available draws
  pp_vol <- posterior_predict(mod_vol, newdata = te, allow_new_levels = TRUE,
                              ndraws = NULL)

  # Clamp implausible volume draws (Gaussian tails can go negative; opp >= 1)
  n_neg <- sum(pp_vol < 1.0)
  if (n_neg > 0) {
    frac_neg <- n_neg / length(pp_vol)
    if (frac_neg > 0.001) {
      cli_warn("Fold {f}: {n_neg} volume draws < 1 ({round(frac_neg*100, 2)}%), clamped to 1.0")
    }
    pp_vol <- pmax(pp_vol, 1.0)
  }

  # Combined: independent draw multiplication (row-aligned by draw index)
  pp_tot <- pp_eff * pp_vol

  # -- Extract PI quantiles per component --
  pi_eff <- extract_posterior_pi(pp_eff, "eff")
  pi_vol <- extract_posterior_pi(pp_vol, "vol")
  pi_tot <- extract_posterior_pi(pp_tot, "tot")

  fold_results[[f]] <- te |>
    select(player_id, season, week, opportunities, epa_per_opp_obs, total_epa) |>
    bind_cols(pi_eff, pi_vol, pi_tot) |>
    mutate(fold = f)

  t1 <- proc.time()[["elapsed"]]
  cli_alert_info(
    "Fold {sprintf('%02d', f)} [{test_season}-W{sprintf('%02d', test_week)}]: {nrow(te)} rows | nu={round(nu_log[f], 1)} | sigma_slope={round(sigma_slope_log[f], 3)} | {round(t1-t0)}s"
  )
}

results   <- bind_rows(fold_results)
conv_all  <- bind_rows(conv_diag)

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
cli_alert_info("Test rows scored: {n_scored} | Expected: {EXPECTED_TEST_N}")
if (n_scored == EXPECTED_TEST_N) {
  cli_alert_success("Row count matches {EXPECTED_TEST_N}")
} else {
  cli_warn("Row count mismatch: {n_scored} scored, {EXPECTED_TEST_N} expected")
}

na_eff <- sum(is.na(results$pred_eff))
na_vol <- sum(is.na(results$pred_vol))
na_tot <- sum(is.na(results$pred_tot))
if (na_eff + na_vol + na_tot == 0L) {
  cli_alert_success("Zero NA predictions across all components")
} else {
  cli_warn("NA predictions: eff={na_eff}, vol={na_vol}, tot={na_tot}")
}

# Convergence summary
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

# nu summary: the tail-weight story
cli_alert_info("Efficiency nu (Student-t df) across 31 folds:")
cli_alert_info(
  "  range [{round(min(nu_log),1)}, {round(max(nu_log),1)}] | median {round(median(nu_log),1)} | mean {round(mean(nu_log),1)}"
)
if (median(nu_log) < 5) {
  cli_alert_success("Median nu < 5: heavy tails in sample -- model widened the per-touch distribution")
} else if (median(nu_log) > 30) {
  cli_alert_info("Median nu > 30: effectively Gaussian in sample -- noise floor reading confirmed")
} else {
  cli_alert_info("Median nu = {round(median(nu_log),1)}: moderate tail weight")
}

cli_alert_info("sigma slope on log_opp (negative = more opp -> smaller sigma):")
cli_alert_info(
  "  range [{round(min(sigma_slope_log, na.rm=T),3)}, {round(max(sigma_slope_log, na.rm=T),3)}] | median {round(median(sigma_slope_log, na.rm=T),3)}"
)

# ===========================================================================
# 3C COVERAGE SCORING
# ===========================================================================

cli_h1("3C Pooled Coverage (all {n_scored} test rows)")

pooled_3c <- bind_rows(
  score_component(results$epa_per_opp_obs,           results, "eff", "efficiency"),
  score_component(as.numeric(results$opportunities), results, "vol", "volume"),
  score_component(results$total_epa,                 results, "tot", "combined")
)
print(pooled_3c |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

cli_h1("3C Low-Usage Bucket (opp {LOW_OPP_LO}-{LOW_OPP_HI})")

res_lo_3c <- results |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
n_low     <- nrow(res_lo_3c)
cli_alert_info("Low-usage rows: {n_low}")

low_3c <- bind_rows(
  score_component(res_lo_3c$epa_per_opp_obs,           res_lo_3c, "eff", "efficiency"),
  score_component(as.numeric(res_lo_3c$opportunities), res_lo_3c, "vol", "volume"),
  score_component(res_lo_3c$total_epa,                 res_lo_3c, "tot", "combined")
) |>
  mutate(stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))
print(low_3c |> select(component, stratum, nominal, empirical, delta, sharpness), n = Inf)

# Stratified combined coverage -- verifies sharpness is honest in every bucket
cli_h1("3C Stratified Combined Coverage (sharpness honesty check)")

results_strat <- results |>
  mutate(
    opp_bucket = case_when(
      opportunities <= LOW_OPP_HI                     ~ paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
      opportunities <= 13L                            ~ "mid  (9-13)",
      TRUE                                            ~ "high (14+)"
    ) |> factor(levels = c(paste0("low  (", LOW_OPP_LO, "-", LOW_OPP_HI, ")"),
                            "mid  (9-13)", "high (14+)"))
  )

strat_3c <- eval_calibration_stratified(
  results_strat$total_epa,
  pi_cols(results_strat, "tot"),
  strata = results_strat$opp_bucket
) |> mutate(component = "combined")

print(strat_3c |> select(component, stratum, n, nominal, empirical, delta, sharpness), n = Inf)

# ===========================================================================
# THREE-WAY COMPARISON: 3C vs 3B vs 3A
# ===========================================================================

cli_h1("Three-Way Comparison: 3C vs 3B vs 3A")

# Load frozen 3A and 3B outputs
pooled_3a <- readr::read_csv("output/03a_lgbm_pooled_coverage.csv",    show_col_types = FALSE)
low_3a    <- readr::read_csv("output/03a_lgbm_low_usage_coverage.csv", show_col_types = FALSE)
preds_3a  <- readr::read_csv("output/03a_lgbm_fold_predictions.csv",   show_col_types = FALSE)
pooled_3b <- readr::read_csv("output/03b_rf_pooled_coverage.csv",      show_col_types = FALSE)
low_3b    <- readr::read_csv("output/03b_rf_low_usage_coverage.csv",   show_col_types = FALSE)
preds_3b  <- readr::read_csv("output/03b_rf_fold_predictions.csv",     show_col_types = FALSE)

# Re-derive sharpness for 3A/3B low-usage (sharpness not stored in low CSV;
# must be computed from fold predictions)
res_lo_3a <- preds_3a |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)
res_lo_3b <- preds_3b |> filter(opportunities >= LOW_OPP_LO, opportunities <= LOW_OPP_HI)

score_lo <- function(preds_lo, label) {
  bind_rows(
    score_component(preds_lo$epa_per_opp_obs,           preds_lo, "eff", "efficiency"),
    score_component(as.numeric(preds_lo$opportunities), preds_lo, "vol", "volume"),
    score_component(preds_lo$total_epa,                 preds_lo, "tot", "combined")
  ) |>
    mutate(
      stratum = paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI),
      model   = label
    )
}

lo_3a_computed <- score_lo(res_lo_3a, "3A-LightGBM")
lo_3b_computed <- score_lo(res_lo_3b, "3B-RF      ")
lo_3c_computed <- low_3c |> mutate(model = "3C-HierBayes")

lo_all <- bind_rows(lo_3a_computed, lo_3b_computed, lo_3c_computed)

# --- DELIVERABLE 1: Noise-floor adjudication -- efficiency, low-opp ---
cli_h2("1. Noise-Floor Adjudication: Low-Usage Efficiency Coverage")
cli_alert_info("3A (LightGBM conformal) vs 3B (RF conformal) vs 3C (hierarchical Bayes)")
cli_alert_info("model          | 50% delta | 80% delta | 90% delta | 80% width")

lo_eff <- lo_all |> filter(component == "efficiency") |>
  select(model, nominal, delta, sharpness) |>
  pivot_wider(names_from = nominal, values_from = c(delta, sharpness),
              names_glue = "{.value}_{nominal}") |>
  mutate(across(starts_with("delta"), fmt_pp),
         across(starts_with("sharpness"), fmt_w))

lo_eff |>
  mutate(row = paste0(model, " | ", delta_0.5, " | ", delta_0.8, " | ", delta_0.9,
                      " | ", sharpness_0.8)) |>
  pull(row) |>
  walk(cli_alert_info)

# Adjudication verdict
eff_80_3c <- lo_3c_computed |> filter(component == "efficiency", nominal == 0.80) |> pull(delta)
eff_80_3a <- lo_3a_computed |> filter(component == "efficiency", nominal == 0.80) |> pull(delta)
eff_80_3b <- lo_3b_computed |> filter(component == "efficiency", nominal == 0.80) |> pull(delta)

cli_h2("Noise-floor verdict:")
if (eff_80_3c < -0.05) {
  cli_alert_info(
    "3C efficiency 80% delta = {fmt_pp(eff_80_3c)}: STILL UNDERCOVERS"
  )
  if (abs(eff_80_3c - eff_80_3a) < 0.03 && abs(eff_80_3c - eff_80_3b) < 0.03) {
    cli_warn(
      "All three models across two families miss by ~{fmt_pp((eff_80_3a+eff_80_3b+eff_80_3c)/3)} -- NOISE FLOOR CONFIRMED. Committee-back per-touch EPA is irreducible with this feature set."
    )
  } else {
    cli_alert_info(
      "3C undercovers but by a different amount -- partial improvement or different failure mode. Not a clean noise-floor verdict."
    )
  }
} else if (eff_80_3c >= -0.02) {
  cli_alert_success(
    "3C efficiency 80% delta = {fmt_pp(eff_80_3c)}: AT/NEAR NOMINAL -- tree-family artifact confirmed. Heavy-tail likelihood captured the per-touch tail that tree learners could not."
  )
} else {
  cli_alert_info(
    "3C efficiency 80% delta = {fmt_pp(eff_80_3c)}: partial improvement ({fmt_pp(eff_80_3a)} to {fmt_pp(eff_80_3c)}). Not a clean confirmation of either hypothesis."
  )
}

# --- DELIVERABLE 2: Combined rubric, all three models ---
cli_h2("2. Combined Coverage -- Pooled and Low-Usage (rubric decision surface)")

# Pooled combined
pool_combined <- bind_rows(
  pooled_3a |> filter(component == "combined") |> mutate(model = "3A-LightGBM"),
  pooled_3b |> filter(component == "combined") |> mutate(model = "3B-RF      "),
  pooled_3c |> filter(component == "combined") |> mutate(model = "3C-HierBayes")
) |> mutate(stratum = "pooled")

lo_combined <- lo_all |> filter(component == "combined")

all_combined <- bind_rows(pool_combined, lo_combined) |>
  mutate(delta_pp = fmt_pp(delta), width = fmt_w(sharpness))

cli_alert_info("model          | stratum       | 50% delta | 80% delta | 80% width")
for (mod in c("3A-LightGBM", "3B-RF      ", "3C-HierBayes")) {
  all_combined |>
    filter(model == mod) |>
    arrange(stratum, nominal) |>
    group_by(stratum) |>
    summarise(
      d50 = fmt_pp(delta[nominal == 0.50]),
      d80 = fmt_pp(delta[nominal == 0.80]),
      w80 = fmt_w(sharpness[nominal == 0.80]),
      .groups = "drop"
    ) |>
    mutate(row = paste0(mod, " | ", str_pad(stratum, 14), " | ", d50, " | ", d80, " | ", w80)) |>
    pull(row) |>
    walk(cli_alert_info)
}

# --- DELIVERABLE 3: Volume low-usage coverage ---
cli_h2("3. Low-Usage Volume Coverage")

lo_vol <- lo_all |> filter(component == "volume") |>
  select(model, nominal, delta, sharpness) |>
  pivot_wider(names_from = nominal, values_from = c(delta, sharpness),
              names_glue = "{.value}_{nominal}") |>
  mutate(across(starts_with("delta"), fmt_pp),
         across(starts_with("sharpness"), fmt_w))

cli_alert_info("model          | 50% delta | 80% delta | 90% delta | 80% width")
lo_vol |>
  mutate(row = paste0(model, " | ", delta_0.5, " | ", delta_0.8, " | ", delta_0.9,
                      " | ", sharpness_0.8)) |>
  pull(row) |>
  walk(cli_alert_info)

# --- DELIVERABLE 4: Stratified combined (sharpness honesty) ---
cli_h2("4. 3C Stratified Combined Coverage (verify pooled sharpness is honest)")
print(strat_3c |> filter(nominal == 0.80) |>
      select(stratum, n, empirical, delta, sharpness) |>
      mutate(delta_pp = fmt_pp(delta)), n = Inf)

# ===========================================================================
# DECISION RULE (unchanged from 3A/3B)
# ===========================================================================

cli_h1("Decision Rule Evaluation")
cli_alert_info("PRIMARY: pooled combined 80% closest to nominal")
cli_alert_info("TIEBREAK: sharpest (narrowest) among models within +-2pp of nominal")
cli_alert_info("VETO: low-usage combined 80% > +-10pp disqualifies")

models_rubric <- all_combined |>
  filter(nominal == 0.80) |>
  select(model, stratum, delta, sharpness)

pooled_rubric <- models_rubric |> filter(stratum == "pooled") |>
  arrange(abs(delta))

low_rubric <- models_rubric |> filter(stratum == paste0("low_opp_", LOW_OPP_LO, "_", LOW_OPP_HI))

cli_h2("Pooled combined 80% (primary):")
pooled_rubric |>
  mutate(row = paste0(model, ": delta=", fmt_pp(delta), " | width=", fmt_w(sharpness))) |>
  pull(row) |> walk(cli_alert_info)

cli_h2("Low-usage combined 80% (veto check):")
low_rubric |>
  mutate(
    veto_status = ifelse(abs(delta) > 0.10, "VETO TRIGGERED", "pass"),
    row = paste0(model, ": delta=", fmt_pp(delta), " [", veto_status, "]")
  ) |>
  pull(row) |> walk(cli_alert_info)

# Identify disqualified models
vetoed <- low_rubric |> filter(abs(delta) > 0.10) |> pull(model)
if (length(vetoed) > 0) {
  cli_warn("Disqualified by veto: {paste(vetoed, collapse=', ')}")
}

eligible <- pooled_rubric |> filter(!model %in% vetoed)
within_2pp <- eligible |> filter(abs(delta) <= 0.02)

if (nrow(within_2pp) == 0L) {
  winner <- eligible |> slice_min(abs(delta)) |> pull(model)
  cli_alert_success("Winner (primary): {winner[1]} (closest to nominal, none within +-2pp)")
} else if (nrow(within_2pp) == 1L) {
  winner <- within_2pp$model
  cli_alert_success("Winner: {winner} (only eligible model within +-2pp)")
} else {
  winner <- within_2pp |> slice_min(sharpness) |> pull(model)
  cli_alert_success("Tiebreak (all within +-2pp): winner = {winner[1]} (sharpest combined)")
  within_2pp |>
    arrange(sharpness) |>
    mutate(row = paste0(model, ": delta=", fmt_pp(delta), " | width=", fmt_w(sharpness))) |>
    pull(row) |> walk(cli_alert_info)
}

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(results,   "output/03c_hier_fold_predictions.csv")
readr::write_csv(pooled_3c, "output/03c_hier_pooled_coverage.csv")
readr::write_csv(low_3c,    "output/03c_hier_low_usage_coverage.csv")
readr::write_csv(conv_all,  "output/03c_hier_convergence_diag.csv")

# nu summary for the methodology piece
nu_summary <- tibble(
  fold           = seq_along(nu_log),
  nu_median      = nu_log,
  sigma_slope    = sigma_slope_log
)
readr::write_csv(nu_summary, "output/03c_hier_nu_log.csv")

cli_alert_success("output/03c_hier_fold_predictions.csv  ({nrow(results)} rows)")
cli_alert_success("output/03c_hier_pooled_coverage.csv")
cli_alert_success("output/03c_hier_low_usage_coverage.csv")
cli_alert_success("output/03c_hier_convergence_diag.csv")
cli_alert_success("output/03c_hier_nu_log.csv")

cli_h1("Step 3C complete")
