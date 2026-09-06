# R/06e_rb3c_fp_simulation.R
# Step 6e: RB upside engine (3C hierarchical Bayes) through FP translation.
#
# Wires the RB 3C upside model into the FP probability chain so the
# two-product architecture (D7) ships with its intended engine on the
# low-touch roster. 06b analog with three deliberate reuses:
#   - Translation fit and residual pools are ENGINE-INDEPENDENT (they map
#     observed EPA/volume to FP; no model predictions involved), so the
#     saved 06b artifacts (data/fp_translation_fits.rds RB fit,
#     output/06b_resid_pools.csv RB pools) are loaded, not refit. Refitting
#     would produce identical objects from the same rows.
#   - Simulation machinery identical to 06b (inverse-CDF over the 7
#     conformal points, Gaussian copula, tier-conditional residuals).
#   - Observed FP joined from the 06b output (same join, avoids re-pull).
# 3C-specific: the fold predictions (output/03c_hier_fold_predictions.csv)
# and the copula rho estimated from 3C's own fold errors.
#
# ROSTER NOTE (pre-committed before this run): the two-product boundary was
# scoped at ~8-9 PREDICTED touches, but volume-model shrinkage compresses
# predictions (observed 10th pctile = 6 opp, predicted = 9), so pred_vol<=8
# yields a near-empty roster (415 rows). The streamer/committee roster is
# defined as 3C pred_vol <= 10 (~18% of rows, n~2,100) -- deployable
# (router may use the upside engine's own volume model) and large enough to
# grade. Strata for evaluation and the 06f recal: low <=10 / mid 10-14 /
# high >14 on 3C pred_vol.
#
# PRE-COMMITTED VETO (grades in 06f, after recalibration): the 3C chain
# ships for the streamer roster only if BOTH
#   (a) recalibrated probabilities honest on the roster (|delta| <= 2pp
#       at 15+ and 20+), AND
#   (b) roster Brier not worse than the shipped 3A-v2 chain (6c strat_iso)
#       on the same rows -- sharper committee-back tails are 3C's entire
#       case; if it cannot beat the incumbent there, single-engine wins.

suppressPackageStartupMessages({
  library(tidyverse)
  library(splines)
  library(cli)
})

set.seed(42)

THRESH_START <- 15
THRESH_BOOM  <- 20
N_SIM        <- 2000

CDF_PROBS <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

# Residual-pool tiers: must match 06b exactly (pools are reused)
tier_rb <- function(opp) cut(opp, breaks = c(-Inf, 9, 14, Inf),
                             labels = c("low", "mid", "high"), right = FALSE)

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)

# ===========================================================================
# 1. LOAD SHARED TRANSLATION ARTIFACTS (engine-independent)
# ===========================================================================

cli_h1("Step 1: Load shared RB translation fit + residual pools")

fit_rb <- readRDS("data/fp_translation_fits.rds")$rb

pools_rb <- readr::read_csv("output/06b_resid_pools.csv", show_col_types = FALSE) |>
  filter(position == "RB") |>
  group_by(tier) |>
  group_map(~ .x$resid) |>
  setNames(c("low", "mid", "high"))

cli_alert_success("Translation fit + pools loaded (n = {sum(lengths(pools_rb))} residuals)")

# ===========================================================================
# 2. LOAD 3C FOLD PREDICTIONS, OBSERVED FP
# ===========================================================================

cli_h1("Step 2: Load 3C fold predictions and observed FP")

preds <- readr::read_csv("output/03c_hier_fold_predictions.csv",
                         show_col_types = FALSE) |> filter(!is.na(player_id))

fp_obs <- readr::read_csv("output/06b_fp_sim_probabilities.csv",
                          show_col_types = FALSE) |>
  filter(position == "RB") |>
  distinct(player_id, season, week, fantasy_points_ppr)

cli_alert_success("3C preds: {nrow(preds)} rows | FP rows: {nrow(fp_obs)}")

# ===========================================================================
# 3. COPULA RHO FROM 3C FOLD ERRORS
# ===========================================================================

cli_h1("Step 3: Estimate 3C EPA-error / volume-error correlation")

err_tot <- (preds$total_epa - preds$pred_tot) /
  pmax((preds$hi_80_tot - preds$lo_80_tot) / 2, 1e-6)
err_vol <- (preds$opportunities - preds$pred_vol) /
  pmax((preds$hi_80_vol - preds$lo_80_vol) / 2, 1e-6)
rho_3c <- 2 * sin(pi * cor(err_tot, err_vol, method = "spearman",
                           use = "complete.obs") / 6)
cli_alert_info("Copula rho (3C): {round(rho_3c, 3)} (06b 3A-v2 chain: 0.034)")

# ===========================================================================
# 4. SIMULATE
# ===========================================================================

cli_h1("Step 4: Simulate FP distributions ({N_SIM} draws per player-week)")

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

n_rows <- nrow(preds)
Q_tot  <- quantile_matrix(preds, "tot")
Q_vol  <- quantile_matrix(preds, "vol")

hit_start <- numeric(n_rows)
hit_boom  <- numeric(n_rows)

for (s in seq_len(N_SIM)) {
  z1 <- rnorm(n_rows)
  z2 <- rho_3c * z1 + sqrt(1 - rho_3c^2) * rnorm(n_rows)
  epa_draw <- inv_cdf(Q_tot, pnorm(z1))
  opp_draw <- pmax(inv_cdf(Q_vol, pnorm(z2)), 0)

  res  <- numeric(n_rows)
  tier <- tier_rb(opp_draw)
  for (tr in c("low", "mid", "high")) {
    idx <- which(tier == tr)
    if (length(idx)) res[idx] <- sample(pools_rb[[tr]], length(idx), replace = TRUE)
  }

  fp <- predict(fit_rb, tibble(total_epa = epa_draw, opportunities = opp_draw)) + res
  hit_start <- hit_start + (fp >= THRESH_START)
  hit_boom  <- hit_boom  + (fp >= THRESH_BOOM)
}

probs_3c <- preds |>
  mutate(
    p_start_sim = hit_start / N_SIM,
    p_boom_sim  = hit_boom  / N_SIM,
    position    = "RB"
  ) |>
  left_join(fp_obs, by = c("player_id", "season", "week")) |>
  mutate(
    hit_start = fantasy_points_ppr >= THRESH_START,
    hit_boom  = fantasy_points_ppr >= THRESH_BOOM
  )

cli_alert_success("Probabilities: {nrow(probs_3c)} rows ({sum(!is.na(probs_3c$fantasy_points_ppr))} with observed FP)")

# ===========================================================================
# 5. RAW CALIBRATION, POOLED AND BY EX-ANTE ROSTER STRATUM
# ===========================================================================

cli_h1("Step 5: Raw calibration (recalibration follows in 06f)")

probs_3c <- probs_3c |>
  mutate(stratum = cut(pred_vol, c(-Inf, 10, 14, Inf), right = FALSE,
                       labels = c("exante_low", "exante_mid", "exante_high")))

calibrate <- function(df, prob_col, hit_col, thresh) {
  df |>
    filter(!is.na(.data[[hit_col]])) |>
    mutate(bin = cut(.data[[prob_col]], seq(0, 1, 0.1),
                     include.lowest = TRUE, right = FALSE)) |>
    group_by(bin) |>
    summarise(n = n(), pred = mean(.data[[prob_col]]),
              emp = mean(.data[[hit_col]]), .groups = "drop") |>
    mutate(delta = emp - pred, threshold = thresh, position = "RB")
}

cal_all <- bind_rows(
  calibrate(probs_3c, "p_start_sim", "hit_start", "15+"),
  calibrate(probs_3c, "p_boom_sim",  "hit_boom",  "20+")
)

cal_summary <- cal_all |>
  group_by(threshold) |>
  summarise(w_mean_abs_delta_pp = sprintf("%.1f", 100 * weighted.mean(abs(delta), n)),
            .groups = "drop")
cli_h2("Raw n-weighted mean |delta| (pp)")
print(cal_summary, n = Inf)

cli_h2("Raw calibration by ex-ante stratum (3C pred_vol: <10 / 10-14 / 14+)")
strat_cal <- probs_3c |>
  filter(!is.na(hit_start)) |>
  group_by(stratum) |>
  summarise(n = n(),
            pred_start = mean(p_start_sim), emp_start = mean(hit_start),
            d_start_pp = fmt_pp(emp_start - pred_start),
            pred_boom = mean(p_boom_sim), emp_boom = mean(hit_boom),
            d_boom_pp = fmt_pp(emp_boom - pred_boom), .groups = "drop")
print(strat_cal, n = Inf)

# ===========================================================================
# 6. SAVE
# ===========================================================================

cli_h1("Step 6: Save outputs")

readr::write_csv(probs_3c, "output/06e_rb3c_fp_sim_probabilities.csv")
readr::write_csv(cal_all,  "output/06e_rb3c_fp_sim_calibration.csv")
readr::write_csv(strat_cal |> mutate(across(everything(), as.character)),
                 "output/06e_rb3c_strat_calibration.csv")
readr::write_csv(tibble(position = "RB_3C", rho = rho_3c, n_sim = N_SIM, seed = 42),
                 "output/06e_rb3c_sim_params.csv")

cli_alert_success("output/06e_rb3c_fp_sim_probabilities.csv ({nrow(probs_3c)} rows)")
cli_alert_success("output/06e_rb3c_fp_sim_calibration.csv")
cli_alert_success("output/06e_rb3c_strat_calibration.csv")
cli_alert_success("output/06e_rb3c_sim_params.csv")

cli_h1("Step 6e complete -- 3C simulation translation")
