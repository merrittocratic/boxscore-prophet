# R/06b_fp_simulation.R
# Step 6b: Simulation-based P(FP >= 15) / P(FP >= 20) -- v2 of the translation layer.
#
# WHY V2: The 06 normal approximation failed its own calibration check in the
# tails (residual skewness RB=0.90, WR=1.19). PPR scoring is right-skewed (TD
# lumpiness), so a normal with matched mean/sd overstates P(15+) for
# high-projection players and understates P(20+) everywhere.
#
# APPROACH (no parametric distribution anywhere):
#   1-3. Same as 06: pull PPR FP, join outcomes, fit per-position OLS
#        fp_ppr ~ total_epa + opportunities. Keep RAW residuals, binned by
#        opportunity tier (committee/mid/workhorse spread differs in shape,
#        not just width).
#   4. Load fold predictions. Each row carries 7 conformal quantile points
#      (lo_90/lo_80/lo_50/pred/hi_50/hi_80/hi_90) for total EPA and volume --
#      a piecewise-linear empirical CDF per player-week.
#   5. Estimate the EPA-error / volume-error dependence from the fold
#      predictions (Spearman -> Gaussian copula rho). 06 assumed independence.
#   6. Simulate: draw (EPA, volume) via inverse-CDF sampling under the copula,
#      push through the regression, add a resampled residual from the matching
#      volume tier. P(threshold) = fraction of draws clearing the threshold.
#   7. Also compute the 06 normal-approximation probabilities on the SAME rows
#      so the calibration comparison is apples-to-apples.
#
# COHERENCE: P(20+) <= P(15+) holds by construction (same simulated draws).
# TAIL NOTE: inverse-CDF draws beyond the 90% interval extrapolate the
# outermost segment linearly -- modest EPA tails; the FP skew enters through
# the resampled residuals, which is where the diagnosis located it.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

set.seed(42)

PREDICTION_SEASONS <- 2014L:2025L
THRESH_START <- 15
THRESH_BOOM  <- 20
N_SIM        <- 2000
Z_80         <- qnorm(0.90)   # for the v1 normal-approx comparison columns

# Quantile levels of the 7 conformal points per row
CDF_PROBS <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)

# Volume tiers for residual pools -- match the pipeline's stratification buckets
tier_rb <- function(opp) cut(opp, breaks = c(-Inf, 9, 14, Inf),
                             labels = c("low", "mid", "high"), right = FALSE)
tier_wr <- function(opp) cut(opp, breaks = c(-Inf, 6, 10, Inf),
                             labels = c("low", "mid", "high"), right = FALSE)

# ===========================================================================
# 1. PULL WEEKLY PPR FANTASY POINTS
# ===========================================================================

cli_h1("Step 1: Pull weekly PPR fantasy points (nflreadr)")

fp_weekly <- nflreadr::load_player_stats(
  seasons   = PREDICTION_SEASONS,
  stat_type = "offense"
) |>
  filter(season_type == "REG", !is.na(fantasy_points_ppr), !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points_ppr)

cli_alert_success("PPR fantasy points: {nrow(fp_weekly)} player-game rows")

# ===========================================================================
# 2. JOIN TO OUTCOME TABLES
# ===========================================================================

cli_h1("Step 2: Join to RB and WR outcome tables")

rb_outcomes <- readRDS("data/rb_outcomes.rds") |> filter(!is.na(player_id))
wr_outcomes <- readRDS("data/wr_outcomes.rds") |> filter(!is.na(player_id))

rb_fp <- rb_outcomes |> inner_join(fp_weekly, by = c("player_id", "season", "week"))
wr_fp <- wr_outcomes |> inner_join(fp_weekly, by = c("player_id", "season", "week"))

cli_alert_success("RB join: {nrow(rb_fp)} rows | WR join: {nrow(wr_fp)} rows")

# ===========================================================================
# 3. FIT REGRESSIONS, BUILD TIER-CONDITIONAL RESIDUAL POOLS
# ===========================================================================

cli_h1("Step 3: Fit EPA -> PPR regressions, pool residuals by volume tier")

fit_rb <- lm(fantasy_points_ppr ~ total_epa + opportunities, data = rb_fp)
fit_wr <- lm(fantasy_points_ppr ~ total_epa + opportunities, data = wr_fp)

make_pools <- function(fit, df, tier_fn) {
  tibble(resid = residuals(fit), tier = tier_fn(df$opportunities)) |>
    group_by(tier) |>
    group_map(~ .x$resid) |>
    setNames(c("low", "mid", "high"))
}

pools_rb <- make_pools(fit_rb, rb_fp, tier_rb)
pools_wr <- make_pools(fit_wr, wr_fp, tier_wr)

skew <- function(x) mean((x - mean(x))^3) / sd(x)^3

pool_diag <- tibble(
  position = rep(c("RB", "WR"), each = 3),
  tier     = rep(c("low", "mid", "high"), 2),
  n        = c(lengths(pools_rb), lengths(pools_wr)),
  sd       = c(map_dbl(pools_rb, sd), map_dbl(pools_wr, sd)),
  skew     = c(map_dbl(pools_rb, skew), map_dbl(pools_wr, skew))
)
cli_h2("Residual pools (sd and skewness by volume tier)")
print(pool_diag, n = Inf)

# ===========================================================================
# 4. LOAD FOLD PREDICTIONS
# ===========================================================================

cli_h1("Step 4: Load fold predictions")

rb_preds <- readr::read_csv("output/03a_v2_lgbm_fold_predictions.csv",
                            show_col_types = FALSE) |> filter(!is.na(player_id))
wr_preds <- readr::read_csv("output/04b_wr_lgbm_fold_predictions.csv",
                            show_col_types = FALSE) |> filter(!is.na(player_id))

cli_alert_success("RB preds: {nrow(rb_preds)} rows | WR preds: {nrow(wr_preds)} rows")

# ===========================================================================
# 5. ESTIMATE EPA/VOLUME ERROR DEPENDENCE (GAUSSIAN COPULA RHO)
# ===========================================================================

cli_h1("Step 5: Estimate EPA-error / volume-error correlation")

copula_rho <- function(preds) {
  err_tot <- (preds$total_epa - preds$pred_tot) /
    pmax((preds$hi_80_tot - preds$lo_80_tot) / 2, 1e-6)
  err_vol <- (preds$opportunities - preds$pred_vol) /
    pmax((preds$hi_80_vol - preds$lo_80_vol) / 2, 1e-6)
  rs <- cor(err_tot, err_vol, method = "spearman", use = "complete.obs")
  2 * sin(pi * rs / 6)   # Spearman -> Gaussian copula rho
}

rho_rb <- copula_rho(rb_preds)
rho_wr <- copula_rho(wr_preds)
cli_alert_info("Copula rho: RB={round(rho_rb, 3)} | WR={round(rho_wr, 3)} (06 assumed 0)")

# ===========================================================================
# 6. SIMULATE
# ===========================================================================

cli_h1("Step 6: Simulate FP distributions ({N_SIM} draws per player-week)")

# Row-wise piecewise-linear inverse CDF over the 7 conformal quantile points.
# u outside [.05, .95] extrapolates the outermost segment linearly.
inv_cdf <- function(Q, u) {
  n <- nrow(Q)
  i <- pmin(pmax(findInterval(u, CDF_PROBS), 1L), 6L)
  q_lo <- Q[cbind(seq_len(n), i)]
  q_hi <- Q[cbind(seq_len(n), i + 1L)]
  q_lo + (u - CDF_PROBS[i]) / (CDF_PROBS[i + 1L] - CDF_PROBS[i]) * (q_hi - q_lo)
}

quantile_matrix <- function(preds, stem) {
  cols <- paste0(c("lo_90_", "lo_80_", "lo_50_", "pred_", "hi_50_", "hi_80_", "hi_90_"), stem)
  Q <- as.matrix(preds[, cols])
  for (j in 2:7) Q[, j] <- pmax(Q[, j], Q[, j - 1])   # enforce monotone quantiles
  Q
}

simulate_position <- function(preds, fit, pools, tier_fn, rho, position_label) {
  n     <- nrow(preds)
  b     <- coef(fit)
  Q_tot <- quantile_matrix(preds, "tot")
  Q_vol <- quantile_matrix(preds, "vol")

  hit_start <- numeric(n)
  hit_boom  <- numeric(n)

  for (s in seq_len(N_SIM)) {
    z1 <- rnorm(n)
    z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
    epa_draw <- inv_cdf(Q_tot, pnorm(z1))
    opp_draw <- pmax(inv_cdf(Q_vol, pnorm(z2)), 0)

    res  <- numeric(n)
    tier <- tier_fn(opp_draw)
    for (tr in c("low", "mid", "high")) {
      idx <- which(tier == tr)
      if (length(idx)) res[idx] <- sample(pools[[tr]], length(idx), replace = TRUE)
    }

    fp <- b[["(Intercept)"]] + b[["total_epa"]] * epa_draw +
      b[["opportunities"]] * opp_draw + res
    hit_start <- hit_start + (fp >= THRESH_START)
    hit_boom  <- hit_boom  + (fp >= THRESH_BOOM)
  }

  # v1 normal approximation on the same rows, for apples-to-apples comparison
  sigma_epa <- (preds$hi_80_tot - preds$lo_80_tot) / 2 / Z_80
  sigma_opp <- (preds$hi_80_vol - preds$lo_80_vol) / 2 / Z_80
  mu_fp     <- b[["(Intercept)"]] + b[["total_epa"]] * preds$pred_tot +
    b[["opportunities"]] * preds$pred_vol
  sigma_fp  <- sqrt((b[["total_epa"]] * sigma_epa)^2 +
                    (b[["opportunities"]] * sigma_opp)^2 + sigma(fit)^2)

  preds |>
    mutate(
      p_start_sim  = hit_start / N_SIM,
      p_boom_sim   = hit_boom  / N_SIM,
      p_start_norm = pnorm(THRESH_START, mu_fp, sigma_fp, lower.tail = FALSE),
      p_boom_norm  = pnorm(THRESH_BOOM,  mu_fp, sigma_fp, lower.tail = FALSE),
      position     = position_label
    )
}

rb_probs <- simulate_position(rb_preds, fit_rb, pools_rb, tier_rb, rho_rb, "RB")
cli_alert_success("RB simulation complete")
wr_probs <- simulate_position(wr_preds, fit_wr, pools_wr, tier_wr, rho_wr, "WR")
cli_alert_success("WR simulation complete")

all_probs <- bind_rows(rb_probs, wr_probs) |>
  left_join(
    fp_weekly |> distinct(player_id, season, week, .keep_all = TRUE),
    by = c("player_id", "season", "week")
  ) |>
  mutate(
    hit_start = fantasy_points_ppr >= THRESH_START,
    hit_boom  = fantasy_points_ppr >= THRESH_BOOM
  )

cli_alert_success("Probabilities: {nrow(all_probs)} rows ({sum(!is.na(all_probs$fantasy_points_ppr))} with observed FP)")

# ===========================================================================
# 7. CALIBRATION: SIMULATION VS NORMAL APPROXIMATION
# ===========================================================================

cli_h1("Step 7: Calibration -- simulation vs normal approximation")

calibrate <- function(df, prob_col, hit_col, method, thresh, pos) {
  df |>
    filter(position == pos, !is.na(.data[[hit_col]]), !is.na(.data[[prob_col]])) |>
    mutate(bin = cut(.data[[prob_col]], seq(0, 1, 0.1),
                     include.lowest = TRUE, right = FALSE)) |>
    group_by(bin) |>
    summarise(n = n(), pred = mean(.data[[prob_col]]),
              emp = mean(.data[[hit_col]]), .groups = "drop") |>
    mutate(delta = emp - pred, method = method, threshold = thresh, position = pos)
}

grid <- expand_grid(
  pos    = c("RB", "WR"),
  thresh = c("15+", "20+"),
  method = c("norm", "sim")
)

cal_all <- pmap(grid, function(pos, thresh, method) {
  pcol <- paste0(if (thresh == "15+") "p_start_" else "p_boom_", method)
  hcol <- if (thresh == "15+") "hit_start" else "hit_boom"
  calibrate(all_probs, pcol, hcol, method, thresh, pos)
}) |> list_rbind()

comparison <- cal_all |>
  select(position, threshold, method, bin, n, delta) |>
  pivot_wider(names_from = method, values_from = c(n, delta)) |>
  mutate(delta_norm_pp = fmt_pp(delta_norm), delta_sim_pp = fmt_pp(delta_sim))

for (pos in c("RB", "WR")) {
  for (thresh in c("15+", "20+")) {
    cli_h2("{pos} P({thresh}) -- delta by bin, normal vs simulation")
    print(comparison |>
            filter(position == pos, threshold == thresh) |>
            select(bin, n_norm, delta_norm_pp, n_sim, delta_sim_pp),
          n = Inf)
  }
}

# n-weighted mean absolute delta (unweighted averages are dominated by tiny bins)
cal_summary <- cal_all |>
  group_by(position, threshold, method) |>
  summarise(w_mean_abs_delta_pp = sprintf("%.1f", 100 * weighted.mean(abs(delta), n)),
            .groups = "drop") |>
  pivot_wider(names_from = method, values_from = w_mean_abs_delta_pp)

cli_h2("n-weighted mean |delta| (pp): normal vs simulation")
print(cal_summary, n = Inf)

# ===========================================================================
# 8. SAVE OUTPUTS
# ===========================================================================

cli_h1("Step 8: Save outputs")

readr::write_csv(all_probs, "output/06b_fp_sim_probabilities.csv")
readr::write_csv(cal_all,   "output/06b_fp_sim_calibration.csv")

resid_pools_out <- tibble(
  position = rep(c("RB", "WR"), c(sum(lengths(pools_rb)), sum(lengths(pools_wr)))),
  tier     = c(rep(names(pools_rb), lengths(pools_rb)),
               rep(names(pools_wr), lengths(pools_wr))),
  resid    = c(unlist(pools_rb, use.names = FALSE),
               unlist(pools_wr, use.names = FALSE))
)
readr::write_csv(resid_pools_out, "output/06b_resid_pools.csv")

sim_params <- tibble(
  position = c("RB", "WR"),
  rho      = c(rho_rb, rho_wr),
  n_sim    = N_SIM,
  seed     = 42
)
readr::write_csv(sim_params, "output/06b_sim_params.csv")

cli_alert_success("output/06b_fp_sim_probabilities.csv ({nrow(all_probs)} rows)")
cli_alert_success("output/06b_fp_sim_calibration.csv")
cli_alert_success("output/06b_resid_pools.csv (deployment residual pools)")
cli_alert_success("output/06b_sim_params.csv")

cli_h1("Step 6b complete -- simulation translation layer")
