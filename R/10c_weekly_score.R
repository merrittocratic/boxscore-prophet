# R/10c_weekly_score.R
# Step 10c: Score a weekly slate end-to-end -- the reconciliation milestone.
#
# Chain: 10b slate CSVs -> 10a deployment models (point preds) -> conformal
# intervals from deployment_params.rds -> simulation translation (cloned
# 06b/09a draw logic, saved resid pools + copula rhos) -> recal maps
# (uniform signature function(p, pred_vol)) -> calibrated probabilities.
#
# DEPLOYMENT SEAM (10a, README 10-series): RB/WR combined intervals scale
# by PRED_VOL^alpha (observed volume does not exist pre-kickoff). The vol
# used for scaling is floored at 1 opportunity -- the minimum observable
# volume in the backtest scaling domain (the power law was fit on rows
# with opp >= 1; sub-unit predictions would extrapolate the law below its
# support). The UNfloored pred_vol feeds the recal maps, matching how 06c
# consumed fold pred_vol.
#
# RECONCILIATION (hindcast weeks only): compare final recalibrated
# probabilities vs the backtest chain (06c/09b) for the same player-weeks,
# using each deployed map's recorded winning method to select the backtest
# column -- the comparison tracks what actually shipped. NOT expected
# identical: deployment models trained on ALL data vs fold models on
# strictly-prior data; pred-vol vs obs-vol interval scaling; recal maps
# refit on all seasons vs walk-forward weekly refits.
#
# PRE-COMMITTED BOUNDS (agreed 2026-07-18, before first run):
#   1. Pearson r >= 0.95 per position x threshold
#   2. |mean signed diff| <= 2pp per position x threshold
#   3. no row with |diff| > 10pp without an explainable cause
#      (rows breaching are printed for inspection; any present = FLAG)
# A breach is a STOP for 10d, not a tolerance to widen.
#
# Usage: Rscript R/10c_weekly_score.R [season] [week]
#   Default 2025 15 (hindcast reconciliation run).
#   Requires: Rscript R/10b_weekly_slate.R + 10b2/10b3/10b4 for the same
#   target week (slate CSVs in output/).

suppressPackageStartupMessages({
  library(tidyverse)
  library(lightgbm)
  library(splines)   # ns() terms inside the saved translation fits
  library(cli)
})

set.seed(42)

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2025L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L

WTAG <- sprintf("%d_w%02d", TARGET_SEASON, TARGET_WEEK)

N_SIM     <- 2000
CDF_PROBS <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

# Thresholds (content spec): RB/WR PPR 15/20, QB standard 20/25
THRESH <- list(
  RB = c(start = 15, boom = 20),
  WR = c(start = 15, boom = 20),
  QB = c(start = 20, boom = 25)
)

# Volume tiers for residual pools -- frozen 06b/09a conventions
tier_rb   <- function(opp) cut(opp, c(-Inf, 9, 14, Inf),
                               labels = c("low", "mid", "high"), right = FALSE)
tier_wr   <- function(opp) cut(opp, c(-Inf, 6, 10, Inf),
                               labels = c("low", "mid", "high"), right = FALSE)
rush_tier <- function(carries) cut(carries, c(-Inf, 4, 8, Inf),
                                   labels = c("statue", "mover", "scrambler"),
                                   right = FALSE)

# Reconciliation bounds (pre-committed, see header)
RECON_MIN_R       <- 0.95
RECON_MAX_MEAN_PP <- 2.0
RECON_ROW_FLAG_PP <- 10.0

fmt_pp <- function(x) sprintf("%+.2f", 100 * x)

# ===========================================================================
# 1. LOAD SLATES + DEPLOYMENT ARTIFACTS
# ===========================================================================

cli_h1("Step 10c: score slate {TARGET_SEASON} week {TARGET_WEEK}")

slate_file <- function(stem) {
  path <- sprintf("output/%s_%s.csv", stem, WTAG)
  if (!file.exists(path)) {
    cli_abort("Missing slate {path} -- run the 10b builders for this week first.")
  }
  readr::read_csv(path, show_col_types = FALSE)
}

rb_slate <- slate_file("10b2_rb_slate")
wr_slate <- slate_file("10b3_wr_slate")
qb_slate <- slate_file("10b4_qb_slate")
cli_alert_success("Slates: RB={nrow(rb_slate)} WR={nrow(wr_slate)} QB={nrow(qb_slate)}")

dp <- readRDS("data/deployment_params.rds")
cli_alert_info("Deployment models trained through {dp$rb$trained_through$season}-W{dp$rb$trained_through$week}")

encode_features <- function(df) {
  df |>
    mutate(
      draft_tier_int        = dp$tier_order[draft_tier],
      is_cold_start_int     = as.integer(is_cold_start),
      def_used_fallback_int = as.integer(def_used_fallback)
    )
}

make_matrix <- function(df, features) {
  df |> select(all_of(features)) |> as.matrix()
}

predict_component <- function(df, spec) {
  mod <- lightgbm::lgb.load(spec$model_file)
  p   <- predict(mod, make_matrix(df, spec$features))
  stopifnot(all(is.finite(p)))
  p
}

# Translation + recal + sim artifacts
fp_fits    <- readRDS("data/fp_translation_fits.rds")        # $rb, $wr
qb_fit     <- readRDS("data/qb_fp_translation_fit.rds")
fp_maps    <- readRDS("data/fp_recal_maps.rds")              # RB_15+ etc.
qb_maps    <- readRDS("data/qb_fp_recal_maps.rds")           # QB_20+ etc.

pools_csv  <- readr::read_csv("output/06b_resid_pools.csv",    show_col_types = FALSE)
qb_pools_csv <- readr::read_csv("output/09a_qb_resid_pools.csv", show_col_types = FALSE)
sim_params <- readr::read_csv("output/06b_sim_params.csv",     show_col_types = FALSE)
qb_sim_csv <- readr::read_csv("output/09a_qb_sim_params.csv",  show_col_types = FALSE)

pool_list <- function(df, pos, tiers) {
  map(set_names(tiers), function(tr) df$resid[df$position == pos & df$tier == tr])
}
pools_rb <- pool_list(pools_csv, "RB", c("low", "mid", "high"))
pools_wr <- pool_list(pools_csv, "WR", c("low", "mid", "high"))
pools_qb <- pool_list(qb_pools_csv, "QB", c("statue", "mover", "scrambler"))
stopifnot(all(lengths(pools_rb) > 0), all(lengths(pools_wr) > 0),
          all(lengths(pools_qb) > 0))

rho_rb <- sim_params$rho[sim_params$position == "RB"]
rho_wr <- sim_params$rho[sim_params$position == "WR"]

QB_COMP_ORDER <- c("pass_eff", "db", "rush", "carry")   # 09a draw order
rho_qb <- as.matrix(qb_sim_csv[match(QB_COMP_ORDER, qb_sim_csv$component),
                               QB_COMP_ORDER])
rownames(rho_qb) <- QB_COMP_ORDER
diag(rho_qb) <- 1
chol_qb <- tryCatch(chol(rho_qb), error = function(e) {
  lambda <- 0.95
  repeat {
    R <- rho_qb * lambda; diag(R) <- 1
    ch <- tryCatch(chol(R), error = function(e) NULL)
    if (!is.null(ch)) { cli_alert_warning("rho shrunk by {lambda} for PD"); return(ch) }
    lambda <- lambda - 0.05
  }
})

cli_alert_success("Artifacts loaded | rho RB={round(rho_rb, 3)} WR={round(rho_wr, 3)}")

# ===========================================================================
# 2. POINT PREDICTIONS + CONFORMAL INTERVALS (deployment params)
# ===========================================================================

cli_h1("Point predictions + intervals")

# 7-point quantile frames with the exact 06b/09a column layout, so the
# cloned quantile_matrix/inv_cdf machinery applies unchanged.
sym_cols <- function(pred, qs, suffix) {
  out <- tibble(
    p    = pred,
    lo50 = pred - qs[1], hi50 = pred + qs[1],
    lo80 = pred - qs[2], hi80 = pred + qs[2],
    lo90 = pred - qs[3], hi90 = pred + qs[3]
  )
  names(out) <- paste0(c("pred_", "lo_50_", "hi_50_", "lo_80_", "hi_80_",
                         "lo_90_", "hi_90_"), suffix)
  out
}

asym_cols <- function(pred, qset, suffix, scale = 1) {
  out <- tibble(
    p    = pred,
    m    = pred + qset$med   * scale,
    lo50 = pred + qset$lo[1] * scale, hi50 = pred + qset$hi[1] * scale,
    lo80 = pred + qset$lo[2] * scale, hi80 = pred + qset$hi[2] * scale,
    lo90 = pred + qset$lo[3] * scale, hi90 = pred + qset$hi[3] * scale
  )
  names(out) <- paste0(c("pred_", "med_", "lo_50_", "hi_50_", "lo_80_",
                         "hi_80_", "lo_90_", "hi_90_"), suffix)
  out
}

# Pred-vol floor for the power-law scale only (see header)
vol_scale <- function(pred_vol, alpha, label) {
  n_floor <- sum(pred_vol < 1)
  if (n_floor > 0) {
    cli_alert_warning("{label}: {n_floor} row{?s} with pred_vol < 1 floored to 1 for interval scaling")
  }
  pmax(pred_vol, 1)^alpha
}

# --- RB: symmetric + power-law (03a-v2 mechanism) ---
rb_enc <- encode_features(rb_slate)
rb_pred_eff <- predict_component(rb_enc, dp$rb$eff)
rb_pred_vol <- predict_component(rb_enc, dp$rb$vol)
rb_pred_tot <- rb_pred_eff * rb_pred_vol
rb_sc       <- vol_scale(rb_pred_vol, dp$rb$tot$alpha, "RB")

rb_scored <- bind_cols(
  rb_slate |> select(player_id, player_name, posteam, defteam, game_id,
                     season, week, report_status, practice_status),
  tibble(pred_eff = rb_pred_eff),
  sym_cols(rb_pred_vol, dp$rb$vol$qs, "vol"),
  tibble(pred_tot = rb_pred_tot)
)
# tot bounds row-wise: half-widths vary per row through the volume scale
for (i in seq_along(dp$rb$tot$q_norm)) {
  cv <- c("50", "80", "90")[i]
  hw <- dp$rb$tot$q_norm[i] * rb_sc
  rb_scored[[paste0("lo_", cv, "_tot")]] <- rb_pred_tot - hw
  rb_scored[[paste0("hi_", cv, "_tot")]] <- rb_pred_tot + hw
}
rb_scored <- rb_scored |> mutate(position = "RB", .before = 1)

# --- WR: asymmetric signed qsets + power-law (04c mechanism) ---
wr_enc <- encode_features(wr_slate)
wr_pred_eff <- predict_component(wr_enc, dp$wr$eff)
wr_pred_vol <- predict_component(wr_enc, dp$wr$vol)
wr_pred_tot <- wr_pred_eff * wr_pred_vol
wr_sc       <- vol_scale(wr_pred_vol, dp$wr$tot$alpha, "WR")

wr_scored <- bind_cols(
  wr_slate |> select(player_id, player_name, posteam, defteam, game_id,
                     season, week, report_status, practice_status),
  tibble(pred_eff = wr_pred_eff),
  asym_cols(wr_pred_vol, dp$wr$vol$qset, "vol"),
  asym_cols(wr_pred_tot, dp$wr$tot$qset, "tot", scale = wr_sc)
) |> mutate(position = "WR", .before = 1)

# --- QB: four symmetric components + const-additive combined (08c) ---
qb_enc <- encode_features(qb_slate)
qb_pred <- map(dp$qb$components, function(spec) predict_component(qb_enc, spec))
qb_pred_tot <- qb_pred$pass_eff * qb_pred$db_vol + qb_pred$rush_dir

qb_scored <- bind_cols(
  qb_slate |> select(player_id, player_name, posteam, defteam, game_id,
                     season, week, report_status, practice_status),
  sym_cols(qb_pred$pass_eff,  dp$qb$qs$pass_eff, "pass_eff"),
  sym_cols(qb_pred$db_vol,    dp$qb$qs$db,       "db"),
  sym_cols(qb_pred$rush_dir,  dp$qb$qs$rush,     "rush"),
  sym_cols(qb_pred$carry_vol, dp$qb$qs$carry,    "carry"),
  sym_cols(qb_pred_tot,       dp$qb$qs$tot,      "tot")
) |> mutate(position = "QB", .before = 1)

cli_alert_success("Predictions: RB tot mean={round(mean(rb_pred_tot), 2)} | WR {round(mean(wr_pred_tot), 2)} | QB {round(mean(qb_pred_tot), 2)} EPA")

# ===========================================================================
# 3. SIMULATION TRANSLATION (cloned 06b / 09a draw logic)
# ===========================================================================

cli_h1("Simulation translation ({N_SIM} draws per player-week)")

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

simulate_rbwr <- function(scored, fit, pools, tier_fn, rho, thresh) {
  n     <- nrow(scored)
  Q_tot <- quantile_matrix(scored, "tot")
  Q_vol <- quantile_matrix(scored, "vol")

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

    fp <- predict(fit, tibble(total_epa = epa_draw, opportunities = opp_draw)) + res
    hit_start <- hit_start + (fp >= thresh["start"])
    hit_boom  <- hit_boom  + (fp >= thresh["boom"])
  }

  scored |> mutate(p_start = hit_start / N_SIM, p_boom = hit_boom / N_SIM)
}

rb_scored <- simulate_rbwr(rb_scored, fp_fits$rb, pools_rb, tier_rb, rho_rb, THRESH$RB)
cli_alert_success("RB simulation complete")
wr_scored <- simulate_rbwr(wr_scored, fp_fits$wr, pools_wr, tier_wr, rho_wr, THRESH$WR)
cli_alert_success("WR simulation complete")

simulate_qb <- function(scored, fit, pools, chol_m, thresh) {
  n       <- nrow(scored)
  Q_eff   <- quantile_matrix(scored, "pass_eff")
  Q_db    <- quantile_matrix(scored, "db")
  Q_rush  <- quantile_matrix(scored, "rush")
  Q_carry <- quantile_matrix(scored, "carry")

  hit_start <- numeric(n)
  hit_boom  <- numeric(n)

  for (s in seq_len(N_SIM)) {
    Z <- matrix(rnorm(n * 4), n, 4) %*% chol_m
    eff_draw   <- inv_cdf(Q_eff,   pnorm(Z[, 1]))
    db_draw    <- pmax(inv_cdf(Q_db,    pnorm(Z[, 2])), 0)
    rush_draw  <- inv_cdf(Q_rush,  pnorm(Z[, 3]))
    carry_draw <- pmax(inv_cdf(Q_carry, pnorm(Z[, 4])), 0)

    res  <- numeric(n)
    tier <- rush_tier(carry_draw)
    for (tr in c("statue", "mover", "scrambler")) {
      idx <- which(tier == tr)
      if (length(idx)) res[idx] <- sample(pools[[tr]], length(idx), replace = TRUE)
    }

    fp <- predict(fit, tibble(pass_epa  = eff_draw * db_draw,
                              dropbacks = db_draw,
                              rush_epa  = rush_draw,
                              carries   = carry_draw)) + res
    hit_start <- hit_start + (fp >= thresh["start"])
    hit_boom  <- hit_boom  + (fp >= thresh["boom"])
  }

  scored |> mutate(p_start = hit_start / N_SIM, p_boom = hit_boom / N_SIM)
}

qb_scored <- simulate_qb(qb_scored, qb_fit, pools_qb, chol_qb, THRESH$QB)
cli_alert_success("QB simulation complete")

# ===========================================================================
# 4. RECALIBRATION MAPS -> FINAL PROBABILITIES
# ===========================================================================

cli_h1("Recalibration maps")

apply_maps <- function(scored, map_start, map_boom, vol) {
  p_start_recal <- pmin(pmax(map_start$map(scored$p_start, vol), 0), 1)
  p_boom_recal  <- pmin(pmax(map_boom$map(scored$p_boom,  vol), 0), 1)
  scored |>
    mutate(
      p_start_recal = p_start_recal,
      p_boom_recal  = pmin(p_boom_recal, p_start_recal),   # coherence cap
      recal_method_start = map_start$method,
      recal_method_boom  = map_boom$method
    )
}

rb_scored <- apply_maps(rb_scored, fp_maps[["RB_15+"]], fp_maps[["RB_20+"]], rb_scored$pred_vol)
wr_scored <- apply_maps(wr_scored, fp_maps[["WR_15+"]], fp_maps[["WR_20+"]], wr_scored$pred_vol)
qb_scored <- apply_maps(qb_scored, qb_maps[["QB_20+"]], qb_maps[["QB_25+"]], qb_scored$pred_carry)

for (d in list(rb_scored, wr_scored, qb_scored)) {
  stopifnot(!any(is.na(d$p_start_recal)), !any(is.na(d$p_boom_recal)),
            all(d$p_boom_recal <= d$p_start_recal + 1e-12))
}

cli_alert_success("Maps applied: RB={fp_maps[['RB_15+']]$method}/{fp_maps[['RB_20+']]$method} WR={fp_maps[['WR_15+']]$method}/{fp_maps[['WR_20+']]$method} QB={qb_maps[['QB_20+']]$method}/{qb_maps[['QB_25+']]$method}")

# ===========================================================================
# 5. SAVE SCORED SLATE
# ===========================================================================

cli_h1("Save scored slate")

id_cols <- c("position", "player_id", "player_name", "posteam", "defteam",
             "game_id", "season", "week", "report_status", "practice_status")
prob_cols <- c("p_start", "p_boom", "p_start_recal", "p_boom_recal",
               "recal_method_start", "recal_method_boom")

slim <- function(d, vol_col) {
  d |>
    mutate(thresh_start = THRESH[[position[1]]]["start"],
           thresh_boom  = THRESH[[position[1]]]["boom"],
           pred_vol_out = .data[[vol_col]]) |>
    select(all_of(id_cols), thresh_start, thresh_boom,
           pred_vol = pred_vol_out, pred_tot,
           lo_80_tot, hi_80_tot, lo_90_tot, hi_90_tot,
           all_of(prob_cols))
}

scored_all <- bind_rows(
  slim(rb_scored, "pred_vol"),
  slim(wr_scored, "pred_vol"),
  slim(qb_scored, "pred_carry")
) |>
  arrange(position, desc(p_start_recal))

out_scored <- sprintf("output/10c_scored_slate_%s.csv", WTAG)
readr::write_csv(scored_all, out_scored)
cli_alert_success("{out_scored} ({nrow(scored_all)} rows)")

# Full per-position detail (all interval columns) for downstream 10d use
out_detail <- sprintf("output/10c_scored_detail_%s.csv", WTAG)
readr::write_csv(bind_rows(rb_scored |> mutate(across(everything(), as.character)),
                           wr_scored |> mutate(across(everything(), as.character)),
                           qb_scored |> mutate(across(everything(), as.character))),
                 out_detail)
cli_alert_success("{out_detail}")

# ===========================================================================
# 6. RECONCILIATION VS BACKTEST CHAIN (hindcast weeks only)
# ===========================================================================

cli_h1("Reconciliation vs backtest chain")

bt_rbwr_path <- "output/06c_recal_probabilities.csv"
bt_qb_path   <- "output/09b_qb_recal_probabilities.csv"

bt_col <- function(stem, method) {
  if (method == "raw") stem else paste0(stem, "_", method)
}

bt_rbwr <- readr::read_csv(bt_rbwr_path, show_col_types = FALSE) |>
  filter(season == TARGET_SEASON, week == TARGET_WEEK)
bt_qb <- readr::read_csv(bt_qb_path, show_col_types = FALSE) |>
  filter(season == TARGET_SEASON, week == TARGET_WEEK) |>
  mutate(position = "QB")

if (nrow(bt_rbwr) + nrow(bt_qb) == 0) {
  cli_alert_warning("No backtest rows for {TARGET_SEASON} week {TARGET_WEEK} -- future week, reconciliation skipped.")
} else {

  pick_bt <- function(bt, pos, start_map, boom_map) {
    bt |>
      filter(position == pos) |>
      transmute(player_id, position,
                bt_p_start = .data[[bt_col("p_start", start_map$method)]],
                bt_p_boom  = .data[[bt_col("p_boom",  boom_map$method)]])
  }

  bt_all <- bind_rows(
    pick_bt(bt_rbwr, "RB", fp_maps[["RB_15+"]], fp_maps[["RB_20+"]]),
    pick_bt(bt_rbwr, "WR", fp_maps[["WR_15+"]], fp_maps[["WR_20+"]]),
    pick_bt(bt_qb,   "QB", qb_maps[["QB_20+"]], qb_maps[["QB_25+"]])
  )

  recon <- scored_all |>
    select(position, player_id, player_name, pred_vol,
           p_start_recal, p_boom_recal) |>
    inner_join(bt_all, by = c("position", "player_id")) |>
    mutate(diff_start = p_start_recal - bt_p_start,
           diff_boom  = p_boom_recal  - bt_p_boom)

  n_slate_only <- nrow(scored_all) - nrow(recon)
  cli_alert_info("Matched {nrow(recon)} player-weeks ({n_slate_only} slate rows without backtest counterpart -- did not play / no observed FP)")

  recon_long <- bind_rows(
    recon |> transmute(position, player_id, player_name, pred_vol,
                       threshold = "start", deploy = p_start_recal,
                       backtest = bt_p_start, diff = diff_start),
    recon |> transmute(position, player_id, player_name, pred_vol,
                       threshold = "boom", deploy = p_boom_recal,
                       backtest = bt_p_boom, diff = diff_boom)
  )

  recon_summary <- recon_long |>
    group_by(position, threshold) |>
    summarise(
      n            = n(),
      pearson_r    = cor(deploy, backtest),
      mean_diff_pp = 100 * mean(diff),
      mad_pp       = 100 * mean(abs(diff)),
      max_abs_pp   = 100 * max(abs(diff)),
      n_over_flag  = sum(abs(diff) > RECON_ROW_FLAG_PP / 100),
      .groups = "drop"
    ) |>
    mutate(
      pass_r    = pearson_r >= RECON_MIN_R,
      pass_mean = abs(mean_diff_pp) <= RECON_MAX_MEAN_PP,
      pass_rows = n_over_flag == 0
    )

  cli_h2("Reconciliation summary (bounds: r >= {RECON_MIN_R}, |mean| <= {RECON_MAX_MEAN_PP}pp, rows > {RECON_ROW_FLAG_PP}pp flagged)")
  print(recon_summary |>
          mutate(pearson_r = sprintf("%.4f", pearson_r),
                 across(c(mean_diff_pp, mad_pp, max_abs_pp), ~ sprintf("%+.2f", .x))),
        n = Inf)

  flagged <- recon_long |> filter(abs(diff) > RECON_ROW_FLAG_PP / 100)
  if (nrow(flagged)) {
    cli_h2("Rows over {RECON_ROW_FLAG_PP}pp (inspect before 10d)")
    print(flagged |>
            mutate(across(c(deploy, backtest, diff), ~ sprintf("%.3f", .x))),
          n = Inf)
  }

  out_recon <- sprintf("output/10c_reconciliation_%s.csv", WTAG)
  out_recon_sum <- sprintf("output/10c_reconciliation_summary_%s.csv", WTAG)
  readr::write_csv(recon_long, out_recon)
  readr::write_csv(recon_summary, out_recon_sum)
  cli_alert_success("{out_recon} ({nrow(recon_long)} rows)")
  cli_alert_success("{out_recon_sum}")

  if (all(recon_summary$pass_r & recon_summary$pass_mean & recon_summary$pass_rows)) {
    cli_alert_success("ALL RECONCILIATION BOUNDS PASSED")
  } else {
    cli_alert_danger("RECONCILIATION BOUND BREACHED -- STOP: inspect before 10d (bounds are pre-committed, not tolerances to widen)")
  }
}

cli_h1("Step 10c complete -- {TARGET_SEASON} week {TARGET_WEEK}")
