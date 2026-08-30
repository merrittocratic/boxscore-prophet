# R/12d_te_fp_simulation.R
# Step 12d: Simulation-based P(FP >= 12) / P(FP >= 17) for TEs.
#
# TE clone of the 06b simulation translation layer (single position, so the
# RB/WR dual plumbing is dropped). Differences from 06b, all receipt-backed:
#   - THRESHOLDS 12 (start) / 17 (boom): rate-matched to the RB/WR 15/20 hit
#     rates in 12_te_feasibility.R (WR cuts hit at only 17.4%/7.5% for TE).
#   - Volume tiers for residual pools: 3-4 / 5-7 / 8+ targets (TE volume sits
#     below WR's 3-5 / 6-9 / 10+; tier sizes printed for audit).
#   - Reads the PRED-VOL rescaled 12c file by default (12d0) -- the 06b0
#     train/serve-skew lesson is baked in rather than opt-in.
# Everything else identical: spline mu (FP convex in EPA), tier-conditional
# empirical residual pools, Gaussian copula for EPA/volume error dependence,
# piecewise-linear inverse-CDF draws over the 7 conformal points, and the
# normal-approximation comparison columns.
#
# EXPECTED (stated before run): sim beats normal on n-weighted |delta| at
# both thresholds, as it did for RB/WR/QB; TE FP skew 1.26 makes the
# empirical-residual machinery more necessary, not less.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(splines)
  library(cli)
})

set.seed(42)

PREDICTION_SEASONS <- 2014L:2025L  # OUTCOME-FITTING range (loads actual FP to fit the EPA->FP translation). Do NOT bump at season rollover -- extend only after the new season has outcomes. Misleading name predates 2026 rollover.
THRESH_START <- 12
THRESH_BOOM  <- 17
N_SIM        <- 2000
Z_80         <- qnorm(0.90)

CDF_PROBS <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)

# TE volume tiers (targets): low 3-4, mid 5-7, high 8+
tier_te <- function(opp) cut(opp, breaks = c(-Inf, 5, 8, Inf),
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
# 2. JOIN TO TE OUTCOME TABLE
# ===========================================================================

cli_h1("Step 2: Join to TE outcome table")

te_outcomes <- readRDS("data/te_outcomes.rds") |> filter(!is.na(player_id))
te_fp <- te_outcomes |> inner_join(fp_weekly, by = c("player_id", "season", "week"))

cli_alert_success("TE join: {nrow(te_fp)} rows")

# Rung-2 ship (2026-07-26): opener implied total in the translation (see
# 06b header note); centered so missing openers (2025) adjust by zero.
vegas_open <- readRDS("data/vegas_open_lines.rds") |>
  select(game_id, posteam, implied_total)
IT_CENTER <- median(vegas_open$implied_total, na.rm = TRUE)
te_fp <- te_fp |>
  left_join(vegas_open, by = c("game_id", "posteam")) |>
  mutate(it_c = coalesce(implied_total, IT_CENTER) - IT_CENTER)
cli_alert_info("Opener implied total: center {round(IT_CENTER, 2)} | coverage {round(100 * mean(!is.na(te_fp$implied_total)), 1)}%")

# ===========================================================================
# 3. FIT REGRESSION, BUILD TIER-CONDITIONAL RESIDUAL POOLS
# ===========================================================================

cli_h1("Step 3: Fit EPA -> PPR regression, pool residuals by volume tier")

fit_te     <- lm(fantasy_points_ppr ~ ns(total_epa, df = 4) + opportunities + it_c, data = te_fp)
fit_te_lin <- lm(fantasy_points_ppr ~ total_epa + opportunities, data = te_fp)
attr(fit_te, "it_center") <- IT_CENTER

env_check <- tibble(res = te_fp$fantasy_points_ppr - predict(fit_te, te_fp),
                    it = te_fp$implied_total) |>
  filter(!is.na(it)) |>
  mutate(bucket = cut(it, c(-Inf, 20, 26, Inf), labels = c("low", "mid", "high"))) |>
  group_by(bucket) |>
  summarise(mean_res = round(mean(res), 3), n = n(), .groups = "drop")
cli_h2("Translation residual mean by implied-total bucket (want ~0)")
print(env_check, n = Inf)

decile_check <- tibble(res = residuals(fit_te), epa = te_fp$total_epa) |>
  mutate(dec = ntile(epa, 10)) |>
  group_by(dec) |>
  summarise(mean_res = round(mean(res), 2), .groups = "drop")
cli_h2("Spline residual mean by EPA decile (should be ~0 everywhere)")
print(decile_check |> pivot_wider(names_from = dec, values_from = mean_res), n = Inf)

pools_te <- tibble(resid = residuals(fit_te), tier = tier_te(te_fp$opportunities)) |>
  group_by(tier) |>
  group_map(~ .x$resid) |>
  setNames(c("low", "mid", "high"))

skew <- function(x) mean((x - mean(x))^3) / sd(x)^3

pool_diag <- tibble(
  tier = c("low", "mid", "high"),
  n    = lengths(pools_te),
  sd   = map_dbl(pools_te, sd),
  skew = map_dbl(pools_te, skew)
)
cli_h2("TE residual pools (sd and skewness by volume tier)")
print(pool_diag, n = Inf)

# ===========================================================================
# 4. LOAD FOLD PREDICTIONS
# ===========================================================================

cli_h1("Step 4: Load fold predictions")

TE_PRED_FILE <- Sys.getenv("TE_PRED_FILE", "output/13e_te_fold_predictions_predvol.csv")
OUT_PREFIX   <- Sys.getenv("FP_OUT_PREFIX", "12d")
cli_alert_info("TE predictions: {TE_PRED_FILE} | output prefix: {OUT_PREFIX}")

te_preds <- readr::read_csv(TE_PRED_FILE, show_col_types = FALSE) |>
  filter(!is.na(player_id))

te_key <- readRDS("data/te_feature_table.rds") |>
  filter(!is.na(player_id)) |>
  distinct(player_id, season, week, game_id, posteam)
te_preds <- te_preds |>
  left_join(te_key, by = c("player_id", "season", "week")) |>
  left_join(vegas_open, by = c("game_id", "posteam")) |>
  mutate(it_c = coalesce(implied_total, IT_CENTER) - IT_CENTER) |>
  select(-game_id, -posteam, -implied_total)

cli_alert_success("TE preds: {nrow(te_preds)} rows")

# ===========================================================================
# 5. ESTIMATE EPA/VOLUME ERROR DEPENDENCE (GAUSSIAN COPULA RHO)
# ===========================================================================

cli_h1("Step 5: Estimate EPA-error / volume-error correlation")

err_tot <- (te_preds$total_epa - te_preds$pred_tot) /
  pmax((te_preds$hi_80_tot - te_preds$lo_80_tot) / 2, 1e-6)
err_vol <- (te_preds$opportunities - te_preds$pred_vol) /
  pmax((te_preds$hi_80_vol - te_preds$lo_80_vol) / 2, 1e-6)
rs     <- cor(err_tot, err_vol, method = "spearman", use = "complete.obs")
rho_te <- 2 * sin(pi * rs / 6)

cli_alert_info("Copula rho: TE={round(rho_te, 3)}")

# ===========================================================================
# 6. SIMULATE
# ===========================================================================

cli_h1("Step 6: Simulate FP distributions ({N_SIM} draws per player-week)")

inv_cdf <- function(Q, u) {
  n <- nrow(Q)
  i <- pmin(pmax(findInterval(u, CDF_PROBS), 1L), 6L)
  q_lo <- Q[cbind(seq_len(n), i)]
  q_hi <- Q[cbind(seq_len(n), i + 1L)]
  q_lo + (u - CDF_PROBS[i]) / (CDF_PROBS[i + 1L] - CDF_PROBS[i]) * (q_hi - q_lo)
}

quantile_matrix <- function(preds, stem) {
  center <- if (paste0("med_", stem) %in% names(preds)) "med_" else "pred_"
  cols <- paste0(c("lo_90_", "lo_80_", "lo_50_", center, "hi_50_", "hi_80_", "hi_90_"), stem)
  Q <- as.matrix(preds[, cols])
  for (j in 2:7) Q[, j] <- pmax(Q[, j], Q[, j - 1])
  Q
}

n     <- nrow(te_preds)
Q_tot <- quantile_matrix(te_preds, "tot")
Q_vol <- quantile_matrix(te_preds, "vol")

hit_start <- numeric(n)
hit_boom  <- numeric(n)

for (s in seq_len(N_SIM)) {
  z1 <- rnorm(n)
  z2 <- rho_te * z1 + sqrt(1 - rho_te^2) * rnorm(n)
  epa_draw <- inv_cdf(Q_tot, pnorm(z1))
  opp_draw <- pmax(inv_cdf(Q_vol, pnorm(z2)), 0)

  res  <- numeric(n)
  tier <- tier_te(opp_draw)
  for (tr in c("low", "mid", "high")) {
    idx <- which(tier == tr)
    if (length(idx)) res[idx] <- sample(pools_te[[tr]], length(idx), replace = TRUE)
  }

  fp <- predict(fit_te, tibble(total_epa = epa_draw, opportunities = opp_draw,
                               it_c = te_preds$it_c)) + res
  hit_start <- hit_start + (fp >= THRESH_START)
  hit_boom  <- hit_boom  + (fp >= THRESH_BOOM)
}

# v1 baseline on the same rows: LINEAR fit + normal approximation
b_lin     <- coef(fit_te_lin)
sigma_epa <- (te_preds$hi_80_tot - te_preds$lo_80_tot) / 2 / Z_80
sigma_opp <- (te_preds$hi_80_vol - te_preds$lo_80_vol) / 2 / Z_80
mu_fp     <- b_lin[["(Intercept)"]] + b_lin[["total_epa"]] * te_preds$pred_tot +
  b_lin[["opportunities"]] * te_preds$pred_vol
sigma_fp  <- sqrt((b_lin[["total_epa"]] * sigma_epa)^2 +
                  (b_lin[["opportunities"]] * sigma_opp)^2 + sigma(fit_te_lin)^2)

all_probs <- te_preds |>
  mutate(
    p_start_sim  = hit_start / N_SIM,
    p_boom_sim   = hit_boom  / N_SIM,
    p_start_norm = pnorm(THRESH_START, mu_fp, sigma_fp, lower.tail = FALSE),
    p_boom_norm  = pnorm(THRESH_BOOM,  mu_fp, sigma_fp, lower.tail = FALSE),
    position     = "TE"
  ) |>
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

calibrate <- function(df, prob_col, hit_col, method, thresh) {
  df |>
    filter(!is.na(.data[[hit_col]]), !is.na(.data[[prob_col]])) |>
    mutate(bin = cut(.data[[prob_col]], seq(0, 1, 0.1),
                     include.lowest = TRUE, right = FALSE)) |>
    group_by(bin) |>
    summarise(n = n(), pred = mean(.data[[prob_col]]),
              emp = mean(.data[[hit_col]]), .groups = "drop") |>
    mutate(delta = emp - pred, method = method, threshold = thresh, position = "TE")
}

grid <- expand_grid(thresh = c("12+", "17+"), method = c("norm", "sim"))

cal_all <- pmap(grid, function(thresh, method) {
  pcol <- paste0(if (thresh == "12+") "p_start_" else "p_boom_", method)
  hcol <- if (thresh == "12+") "hit_start" else "hit_boom"
  calibrate(all_probs, pcol, hcol, method, thresh)
}) |> list_rbind()

comparison <- cal_all |>
  select(threshold, method, bin, n, delta) |>
  pivot_wider(names_from = method, values_from = c(n, delta)) |>
  mutate(delta_norm_pp = fmt_pp(delta_norm), delta_sim_pp = fmt_pp(delta_sim))

for (thresh in c("12+", "17+")) {
  cli_h2("TE P({thresh}) -- delta by bin, normal vs simulation")
  print(comparison |>
          filter(threshold == thresh) |>
          select(bin, n_norm, delta_norm_pp, n_sim, delta_sim_pp),
        n = Inf)
}

cal_summary <- cal_all |>
  group_by(threshold, method) |>
  summarise(w_mean_abs_delta_pp = sprintf("%.1f", 100 * weighted.mean(abs(delta), n)),
            .groups = "drop") |>
  pivot_wider(names_from = method, values_from = w_mean_abs_delta_pp)

cli_h2("n-weighted mean |delta| (pp): normal vs simulation")
print(cal_summary, n = Inf)

# ===========================================================================
# 8. SAVE OUTPUTS
# ===========================================================================

cli_h1("Step 8: Save outputs")

readr::write_csv(all_probs, paste0("output/", OUT_PREFIX, "_te_fp_sim_probabilities.csv"))
readr::write_csv(cal_all,   paste0("output/", OUT_PREFIX, "_te_fp_sim_calibration.csv"))

resid_pools_out <- tibble(
  position = "TE",
  tier     = rep(names(pools_te), lengths(pools_te)),
  resid    = unlist(pools_te, use.names = FALSE)
)
readr::write_csv(resid_pools_out, paste0("output/", OUT_PREFIX, "_te_resid_pools.csv"))

sim_params <- tibble(position = "TE", rho = rho_te, n_sim = N_SIM, seed = 42,
                     thresh_start = THRESH_START, thresh_boom = THRESH_BOOM)
readr::write_csv(sim_params, paste0("output/", OUT_PREFIX, "_te_sim_params.csv"))

# Overridable seam (2026-08-30, volfix recal refit): see 06b's TRANS_FIT_OUT
# note -- default path unchanged for normal 12d runs.
TE_TRANS_FIT_OUT <- Sys.getenv("TE_TRANS_FIT_OUT", "data/te_fp_translation_fit.rds")
saveRDS(fit_te, TE_TRANS_FIT_OUT)

cli_alert_success("output/{OUT_PREFIX}_te_fp_sim_probabilities.csv ({nrow(all_probs)} rows)")
cli_alert_success("{TE_TRANS_FIT_OUT}")

cli_h1("Step 12d complete -- TE simulation translation layer")
