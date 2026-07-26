# R/09a_qb_fp_simulation.R
# Step 9a: QB translation layer -- simulation-based P(FP >= 20) / P(FP >= 25).
#
# 06b analog for the QB spine (08c hybrid model). Differences from RB/WR,
# all pre-specified in the 08c wrap-up notes:
#   - Scoring: standard fantasy_points (4pt pass TD, verified in 07 by
#     recompute), NOT PPR -- QB receptions are ~0. Thresholds 20/25.
#   - Components kept separate through translation: the regression is
#     fp ~ ns(pass_epa) + rush_epa + carries, not a single total-EPA spline.
#     Feasibility Q2: a rushing EPA point buys more FP than a passing one
#     (rush TDs 6pt vs pass 4pt, rush yards 0.1/yd vs 0.04), so collapsing
#     to total EPA would re-import the scrambler mispricing 08b removed.
#   - Simulation draws the FOUR component intervals from 08c (pass_eff,
#     dropbacks, rush_epa, carries) under a 4x4 Gaussian copula estimated
#     from standardized fold errors (Spearman -> rho). pass_epa is DERIVED
#     per draw as pass_eff x dropbacks, mirroring the hybrid decomposition.
#   - Residual pools binned by carries tier (statue/mover/scrambler,
#     breaks 4/8 -- the 07/08 convention): rush-TD lumpiness makes the FP
#     residual fatter-tailed for running QBs. Pools are built on observed
#     carries and applied to DRAWN carries, same conditioning pattern as
#     06b (pools by observed opportunities, applied to drawn opportunities).
#   - Kneel note: fantasy_points includes kneel yardage (small negative);
#     our rush_epa/carries exclude kneels. Fine -- the regression refits on
#     our quantities and the offset is absorbed by the intercept/residuals.
#
# Normal-approximation baseline (v1-style) computed on the same rows from
# the 08c combined-total intervals + a linear fit, for the calibration
# comparison -- QB skew is milder than RB/WR (feasibility), so the gap
# between sim and normal should be smaller here.
#
# COHERENCE: P(25+) <= P(20+) holds by construction (same simulated draws).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(splines)
  library(cli)
})

set.seed(42)

PREDICTION_SEASONS <- 2014L:2025L
THRESH_START <- 20   # startable QB week
THRESH_BOOM  <- 25   # boom week
N_SIM        <- 2000
Z_80         <- qnorm(0.90)

# Quantile levels of the 7 conformal points per row (symmetric 08c intervals)
CDF_PROBS <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

# Rushing tiers -- the 07/08 convention, [0,4) / [4,8) / [8,Inf)
TIER_BREAKS <- c(-Inf, 4, 8, Inf)
TIER_LABELS <- c("statue", "mover", "scrambler")
rush_tier <- function(carries) {
  cut(carries, TIER_BREAKS, labels = TIER_LABELS, right = FALSE)
}

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)
skew   <- function(x) mean((x - mean(x))^3) / sd(x)^3

# ===========================================================================
# 1. PULL WEEKLY STANDARD FANTASY POINTS
# ===========================================================================

cli_h1("Step 1: Pull weekly standard fantasy points (nflreadr, 4pt pass TD)")

fp_weekly <- nflreadr::load_player_stats(
  seasons   = PREDICTION_SEASONS,
  stat_type = "offense"
) |>
  filter(season_type == "REG", !is.na(fantasy_points), !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points)

cli_alert_success("Fantasy points: {nrow(fp_weekly)} player-game rows")

# ===========================================================================
# 2. JOIN TO QB FEATURE TABLE (observed quantities)
# ===========================================================================

cli_h1("Step 2: Join to QB starter rows")

qb_rows <- readRDS("data/qb_feature_table.rds") |>
  select(player_id, season, week, game_id, posteam,
         pass_epa, rush_epa, dropbacks, carries) |>
  inner_join(fp_weekly, by = c("player_id", "season", "week"))

cli_alert_success("QB join: {nrow(qb_rows)} starter rows with fantasy points")

# Rung-2 ship (2026-07-26): opener implied total in the translation (see
# 06b header note); centered so missing openers (2025) adjust by zero.
vegas_open <- readRDS("data/vegas_open_lines.rds") |>
  select(game_id, posteam, implied_total)
IT_CENTER <- median(vegas_open$implied_total, na.rm = TRUE)
qb_rows <- qb_rows |>
  left_join(vegas_open, by = c("game_id", "posteam")) |>
  mutate(it_c = coalesce(implied_total, IT_CENTER) - IT_CENTER)
cli_alert_info("Opener implied total: center {round(IT_CENTER, 2)} | coverage {round(100 * mean(!is.na(qb_rows$implied_total)), 1)}%")

# ===========================================================================
# 3. FIT TRANSLATION REGRESSION, BUILD TIER-CONDITIONAL RESIDUAL POOLS
# ===========================================================================

cli_h1("Step 3: Fit FP translation, pool residuals by rushing tier")

# Spline on pass_epa (FP convex in EPA -- TD lumpiness, same argument as
# 06b); rush_epa and carries enter linearly (2-8 carries is too thin to
# support a spline, and feasibility found the rush FP exchange rate stable).
# dropbacks enters because FP pays for yardage ACCRUAL (0.04/pass yard)
# that EPA does not credit -- a high-volume zero-EPA game still banks
# ~10 FP of yards. First fit without it showed a monotone residual trend
# (-4.4 FP bottom decile to +4.5 top); with it the trend flattens.
fit_qb <- lm(fantasy_points ~ ns(pass_epa, df = 4) + dropbacks + rush_epa + carries + it_c,
             data = qb_rows)
attr(fit_qb, "it_center") <- IT_CENTER

env_check_qb <- tibble(res = qb_rows$fantasy_points - predict(fit_qb, qb_rows),
                       it = qb_rows$implied_total) |>
  filter(!is.na(it)) |>
  mutate(bucket = cut(it, c(-Inf, 20, 26, Inf), labels = c("low", "mid", "high"))) |>
  group_by(bucket) |>
  summarise(mean_res = round(mean(res), 3), n = n(), .groups = "drop")
cli_h2("Translation residual mean by implied-total bucket (want ~0)")
print(env_check_qb, n = Inf)

# Linear total-EPA fit retained as the v1 "norm" comparison baseline
fit_qb_lin <- lm(fantasy_points ~ I(pass_epa + rush_epa) + carries, data = qb_rows)

# Functional-form checks: residual means should be flat everywhere the
# simulation will condition, including dropbacks now that it is a term.
form_check <- function(fit, df, var, label, k = 10) {
  tibble(res = residuals(fit), v = df[[var]]) |>
    mutate(grp = ntile(v, k)) |>
    group_by(grp) |>
    summarise(mean_res = round(mean(res), 2), .groups = "drop") |>
    mutate(axis = label)
}
cli_h2("Residual mean by decile (should be ~0 everywhere)")
print(bind_rows(
  form_check(fit_qb, qb_rows, "pass_epa",  "pass_epa"),
  form_check(fit_qb, qb_rows, "dropbacks", "dropbacks")
) |> pivot_wider(names_from = grp, values_from = mean_res), n = Inf)

cli_h2("Residual mean by rushing tier (kneel-offset + exchange-rate check)")
tier_check <- tibble(res = residuals(fit_qb), tier = rush_tier(qb_rows$carries)) |>
  group_by(tier) |>
  summarise(n = n(), mean_res = round(mean(res), 2),
            sd_res = round(sd(res), 2), skew = round(skew(res), 2),
            .groups = "drop")
print(tier_check, n = Inf)

pools_qb <- tibble(resid = residuals(fit_qb), tier = rush_tier(qb_rows$carries)) |>
  group_by(tier) |>
  group_map(~ .x$resid) |>
  setNames(TIER_LABELS)

# ===========================================================================
# 4. LOAD 08c FOLD PREDICTIONS
# ===========================================================================

cli_h1("Step 4: Load 08c fold predictions")

# QB source = rung-2 Vegas opener arm (13e canonical) since 2026-07-26
qb_preds <- readr::read_csv("output/13e_qb_fold_predictions.csv",
                            show_col_types = FALSE) |> filter(!is.na(player_id))

qb_key <- readRDS("data/qb_feature_table.rds") |>
  filter(!is.na(player_id)) |>
  distinct(player_id, season, week, game_id, posteam)
qb_preds <- qb_preds |>
  left_join(qb_key, by = c("player_id", "season", "week")) |>
  left_join(vegas_open, by = c("game_id", "posteam")) |>
  mutate(it_c = coalesce(implied_total, IT_CENTER) - IT_CENTER) |>
  select(-game_id, -posteam, -implied_total)

cli_alert_success("QB preds: {nrow(qb_preds)} rows")

# ===========================================================================
# 5. ESTIMATE 4-COMPONENT ERROR DEPENDENCE (GAUSSIAN COPULA)
# ===========================================================================

cli_h1("Step 5: Estimate component-error copula (pass_eff, db, rush, carry)")

std_err <- function(obs, pred, hi80, lo80) {
  (obs - pred) / pmax((hi80 - lo80) / 2, 1e-6)
}

err_mat <- with(qb_preds, cbind(
  pass_eff = std_err(pass_epa / pmax(dropbacks, 1), pred_pass_eff,
                     hi_80_pass_eff, lo_80_pass_eff),
  db       = std_err(dropbacks, pred_db,    hi_80_db,    lo_80_db),
  rush     = std_err(rush_epa,  pred_rush,  hi_80_rush,  lo_80_rush),
  carry    = std_err(carries,   pred_carry, hi_80_carry, lo_80_carry)
))

rho_qb <- 2 * sin(pi * cor(err_mat, method = "spearman",
                           use = "complete.obs") / 6)
diag(rho_qb) <- 1

cli_h2("Copula rho matrix")
print(round(rho_qb, 3))

chol_qb <- tryCatch(chol(rho_qb), error = function(e) {
  # Shrink off-diagonals until positive definite (pairwise Spearman
  # matrices are not guaranteed PD)
  lambda <- 0.95
  repeat {
    R <- rho_qb * lambda; diag(R) <- 1
    ch <- tryCatch(chol(R), error = function(e) NULL)
    if (!is.null(ch)) { cli_alert_warning("rho shrunk by {lambda} for PD"); return(ch) }
    lambda <- lambda - 0.05
  }
})

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
  cols <- paste0(c("lo_90_", "lo_80_", "lo_50_", "pred_", "hi_50_", "hi_80_", "hi_90_"), stem)
  Q <- as.matrix(preds[, cols])
  for (j in 2:7) Q[, j] <- pmax(Q[, j], Q[, j - 1])
  Q
}

n_rows <- nrow(qb_preds)
Q_eff   <- quantile_matrix(qb_preds, "pass_eff")
Q_db    <- quantile_matrix(qb_preds, "db")
Q_rush  <- quantile_matrix(qb_preds, "rush")
Q_carry <- quantile_matrix(qb_preds, "carry")

hit_start <- numeric(n_rows)
hit_boom  <- numeric(n_rows)

for (s in seq_len(N_SIM)) {
  Z <- matrix(rnorm(n_rows * 4), n_rows, 4) %*% chol_qb
  eff_draw   <- inv_cdf(Q_eff,   pnorm(Z[, 1]))
  db_draw    <- pmax(inv_cdf(Q_db,    pnorm(Z[, 2])), 0)
  rush_draw  <- inv_cdf(Q_rush,  pnorm(Z[, 3]))
  carry_draw <- pmax(inv_cdf(Q_carry, pnorm(Z[, 4])), 0)

  pass_epa_draw <- eff_draw * db_draw

  res  <- numeric(n_rows)
  tier <- rush_tier(carry_draw)
  for (tr in TIER_LABELS) {
    idx <- which(tier == tr)
    if (length(idx)) res[idx] <- sample(pools_qb[[tr]], length(idx), replace = TRUE)
  }

  fp <- predict(fit_qb, tibble(it_c = qb_preds$it_c,
                               pass_epa  = pass_epa_draw,
                               dropbacks = db_draw,
                               rush_epa  = rush_draw,
                               carries   = carry_draw)) + res
  hit_start <- hit_start + (fp >= THRESH_START)
  hit_boom  <- hit_boom  + (fp >= THRESH_BOOM)
}

cli_alert_success("Simulation complete")

# v1 baseline on the same rows: linear total-EPA fit + normal approximation
b_lin     <- coef(fit_qb_lin)
sigma_epa <- (qb_preds$hi_80_tot - qb_preds$lo_80_tot) / 2 / Z_80
sigma_car <- (qb_preds$hi_80_carry - qb_preds$lo_80_carry) / 2 / Z_80
mu_fp     <- b_lin[["(Intercept)"]] +
  b_lin[["I(pass_epa + rush_epa)"]] * qb_preds$pred_tot +
  b_lin[["carries"]] * qb_preds$pred_carry
sigma_fp  <- sqrt((b_lin[["I(pass_epa + rush_epa)"]] * sigma_epa)^2 +
                  (b_lin[["carries"]] * sigma_car)^2 + sigma(fit_qb_lin)^2)

qb_probs <- qb_preds |>
  mutate(
    p_start_sim  = hit_start / N_SIM,
    p_boom_sim   = hit_boom  / N_SIM,
    p_start_norm = pnorm(THRESH_START, mu_fp, sigma_fp, lower.tail = FALSE),
    p_boom_norm  = pnorm(THRESH_BOOM,  mu_fp, sigma_fp, lower.tail = FALSE),
    position     = "QB"
  ) |>
  left_join(
    fp_weekly |> distinct(player_id, season, week, .keep_all = TRUE),
    by = c("player_id", "season", "week")
  ) |>
  mutate(
    hit_start = fantasy_points >= THRESH_START,
    hit_boom  = fantasy_points >= THRESH_BOOM
  )

cli_alert_success("Probabilities: {nrow(qb_probs)} rows ({sum(!is.na(qb_probs$fantasy_points))} with observed FP)")

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
    mutate(delta = emp - pred, method = method, threshold = thresh,
           position = "QB")
}

grid <- expand_grid(thresh = c("20+", "25+"), method = c("norm", "sim"))

cal_all <- pmap(grid, function(thresh, method) {
  pcol <- paste0(if (thresh == "20+") "p_start_" else "p_boom_", method)
  hcol <- if (thresh == "20+") "hit_start" else "hit_boom"
  calibrate(qb_probs, pcol, hcol, method, thresh)
}) |> list_rbind()

comparison <- cal_all |>
  select(threshold, method, bin, n, delta) |>
  pivot_wider(names_from = method, values_from = c(n, delta)) |>
  mutate(delta_norm_pp = fmt_pp(delta_norm), delta_sim_pp = fmt_pp(delta_sim))

for (thresh in c("20+", "25+")) {
  cli_h2("QB P({thresh}) -- delta by bin, normal vs simulation")
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

# Ex-ante rushing-tier honesty (the 08c watch item: rush-component
# shrinkage should surface here if it survives translation)
cli_h2("Sim calibration by EX-ANTE rushing tier (prior_carries_pg)")
tier_cal <- qb_probs |>
  filter(!is.na(hit_start)) |>
  mutate(exante_tier = rush_tier(prior_carries_pg)) |>
  group_by(exante_tier) |>
  summarise(n = n(),
            pred_start = mean(p_start_sim), emp_start = mean(hit_start),
            d_start_pp = fmt_pp(emp_start - pred_start),
            pred_boom = mean(p_boom_sim), emp_boom = mean(hit_boom),
            d_boom_pp = fmt_pp(emp_boom - pred_boom), .groups = "drop")
print(tier_cal, n = Inf)

# ===========================================================================
# 8. SAVE OUTPUTS
# ===========================================================================

cli_h1("Step 8: Save outputs")

readr::write_csv(qb_probs, "output/09a_qb_fp_sim_probabilities.csv")
readr::write_csv(cal_all,  "output/09a_qb_fp_sim_calibration.csv")
readr::write_csv(tier_cal |> mutate(across(everything(), as.character)),
                 "output/09a_qb_tier_calibration.csv")

resid_pools_out <- tibble(
  position = "QB",
  tier     = rep(names(pools_qb), lengths(pools_qb)),
  resid    = unlist(pools_qb, use.names = FALSE)
)
readr::write_csv(resid_pools_out, "output/09a_qb_resid_pools.csv")

sim_params <- as_tibble(rho_qb, rownames = "component") |>
  mutate(n_sim = N_SIM, seed = 42)
readr::write_csv(sim_params, "output/09a_qb_sim_params.csv")

saveRDS(fit_qb, "data/qb_fp_translation_fit.rds")

cli_alert_success("output/09a_qb_fp_sim_probabilities.csv ({nrow(qb_probs)} rows)")
cli_alert_success("output/09a_qb_fp_sim_calibration.csv")
cli_alert_success("output/09a_qb_tier_calibration.csv")
cli_alert_success("output/09a_qb_resid_pools.csv")
cli_alert_success("output/09a_qb_sim_params.csv")
cli_alert_success("data/qb_fp_translation_fit.rds")

cli_h1("Step 9a complete -- QB simulation translation layer")
