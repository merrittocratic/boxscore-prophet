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
  library(nflreadr)  # schedules -> kickoff times for the re-score partition
  library(cli)
})

# D27 star_platt ship: shared core for the RB trailing-FP star buckets
# (one implementation, two call sites -- fit side is 18e).
source("R/18e_star_bucket_fns.R")
source("R/10b_roster_helpers.R")   # load_current_depth_chart() for WR/TE/QB role-signal fixes

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L

WTAG <- sprintf("%d_w%02d", TARGET_SEASON, TARGET_WEEK)

N_SIM     <- 2000
CDF_PROBS <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

# Per-position RNG seeds (2026-09-06 rebuild prep), replacing a single
# set.seed(42) shared by all four simulate_*() calls in sequence. Under the
# old scheme, each position's draws depended on its position in the call
# order, not on the position itself -- documented at the old TE-must-be-last
# comment below (only RB, drawing first, actually started from seed 42; WR,
# QB, and TE each drew from wherever the shared stream landed after every
# prior position's N_SIM=2000 iterations). Seeding each position
# independently, keyed by position identity rather than call order, makes
# every position's draws invariant to the order the calls happen to appear
# in. This is a ONE-TIME BREAK: all four positions' probabilities move vs
# every previously shipped run, since none of them draw from the same stream
# position as before. Re-baseline the frozen reconciliation reference after
# this change, not before -- do not diff against old numbers expecting a match.
SIM_SEED <- c(RB = 42L, WR = 43L, QB = 44L, TE = 45L)

# Thresholds (content spec): RB/WR PPR 15/20, TE PPR 12/17 (12_te
# feasibility rate-matched cuts), QB standard 20/25
THRESH <- list(
  RB = c(start = 15, boom = 20),
  WR = c(start = 15, boom = 20),
  TE = c(start = 12, boom = 17),
  QB = c(start = 20, boom = 25)
)

# Volume tiers for residual pools -- frozen 06b/09a/12d conventions
tier_rb   <- function(opp) cut(opp, c(-Inf, 9, 14, Inf),
                               labels = c("low", "mid", "high"), right = FALSE)
tier_wr   <- function(opp) cut(opp, c(-Inf, 6, 10, Inf),
                               labels = c("low", "mid", "high"), right = FALSE)
tier_te   <- function(opp) cut(opp, c(-Inf, 5, 8, Inf),
                               labels = c("low", "mid", "high"), right = FALSE)
rush_tier <- function(carries) cut(carries, c(-Inf, 4, 8, Inf),
                                   labels = c("statue", "mover", "scrambler"),
                                   right = FALSE)

# Reconciliation bounds (pre-committed, see header)
RECON_MIN_R       <- 0.95
RECON_MAX_MEAN_PP <- 2.0
RECON_ROW_FLAG_PP <- 10.0

fmt_pp <- function(x) sprintf("%+.2f", 100 * x)
`%||%` <- function(a, b) if (is.null(a)) b else a

# ===========================================================================
# 1. LOAD SLATES + DEPLOYMENT ARTIFACTS
# ===========================================================================

cli_h1("Step 10c: score slate {TARGET_SEASON} week {TARGET_WEEK}")

# Overridable seam (2026-08-31, volfix candidate hindcast): pick up a
# candidate-augmented slate variant (e.g. "_volfixaug") without touching the
# real 10b builder outputs. Default "" reproduces prior behavior exactly.
SLATE_SUFFIX <- Sys.getenv("SLATE_SUFFIX", "")
slate_file <- function(stem) {
  path <- sprintf("output/%s_%s%s.csv", stem, WTAG, SLATE_SUFFIX)
  if (!file.exists(path)) {
    cli_abort("Missing slate {path} -- run the 10b builders for this week first.")
  }
  readr::read_csv(path, show_col_types = FALSE)
}

rb_slate <- slate_file("10b2_rb_slate")
wr_slate <- slate_file("10b3_wr_slate")
te_slate <- slate_file("10b5_te_slate")
qb_slate <- slate_file("10b4_qb_slate")
cli_alert_success("Slates: RB={nrow(rb_slate)} WR={nrow(wr_slate)} TE={nrow(te_slate)} QB={nrow(qb_slate)}")

# ---------------------------------------------------------------------------
# Kickoff-aware partition (in-season re-scores). A game that has kicked off
# is NEVER re-scored: its published number is whatever the ledger holds from
# the last pre-kickoff run. AS_OF resolution:
#   - env AS_OF ("YYYY-MM-DD HH:MM", ET) -> explicit clock (tests, replays)
#   - unset + at least one future kickoff -> Sys.time() (live production)
#   - unset + all kickoffs in the past -> hindcast mode, full slate scored
#     (the historical-validation path; reconciliation only runs here)
# ---------------------------------------------------------------------------

kickoffs <- nflreadr::load_schedules(TARGET_SEASON) |>
  filter(game_type == "REG", week == TARGET_WEEK) |>
  transmute(game_id,
            kickoff_et = as.POSIXct(paste(gameday, coalesce(gametime, "13:00")),
                                    tz = "America/New_York"))

as_of_env <- Sys.getenv("AS_OF", "")
if (nzchar(as_of_env)) {
  AS_OF <- as.POSIXct(as_of_env, tz = "America/New_York")
  if (is.na(AS_OF)) cli_abort("Could not parse AS_OF='{as_of_env}' (want 'YYYY-MM-DD HH:MM', ET)")
  RUN_MODE <- "rescore"
} else if (all(kickoffs$kickoff_et < Sys.time())) {
  AS_OF <- min(kickoffs$kickoff_et) - 86400   # as-if the day before the week
  RUN_MODE <- "hindcast"
} else {
  AS_OF <- Sys.time()
  RUN_MODE <- "live"
}

live_games <- kickoffs |> filter(kickoff_et > AS_OF)
n_skipped  <- nrow(kickoffs) - nrow(live_games)
cli_alert_info("Mode: {RUN_MODE} | as-of {format(AS_OF, '%Y-%m-%d %H:%M %Z')} | {nrow(live_games)}/{nrow(kickoffs)} games still to kick off")
if (n_skipped > 0) {
  cli_alert_warning("Skipping {n_skipped} already-kicked game{?s}: {paste(setdiff(kickoffs$game_id, live_games$game_id), collapse = ', ')}")
}
if (nrow(live_games) == 0) {
  cli_abort("No games left to score at this AS_OF -- nothing to do.")
}

rb_slate <- rb_slate |> semi_join(live_games, by = "game_id")
wr_slate <- wr_slate |> semi_join(live_games, by = "game_id")
te_slate <- te_slate |> semi_join(live_games, by = "game_id")
qb_slate <- qb_slate |> semi_join(live_games, by = "game_id")
cli_alert_success("Scoring: RB={nrow(rb_slate)} WR={nrow(wr_slate)} TE={nrow(te_slate)} QB={nrow(qb_slate)} players")

# Overridable seams (2026-08-31, volfix candidate hindcast): same
# Sys.getenv pattern as the 06b0/06b/06c/12d0/12d/12e_te seams added
# 2026-08-30. Defaults reproduce prior behavior exactly -- a bare
# `Rscript R/10c_weekly_score.R` still reads/writes the real shipped
# artifacts. Set these to point at data/deployment_params_volfix.rds etc.
# to score a slate against the candidate deployed models without touching
# the real deployment_params.rds / fp_recal_maps.rds / production 10c
# ledger and scored-slate outputs.
DP_FILE             <- Sys.getenv("DP_FILE",             "data/deployment_params.rds")
FP_TRANS_FITS_FILE  <- Sys.getenv("FP_TRANS_FITS_FILE",  "data/fp_translation_fits.rds")
TE_TRANS_FIT_FILE   <- Sys.getenv("TE_TRANS_FIT_FILE",   "data/te_fp_translation_fit.rds")
QB_TRANS_FIT_FILE   <- Sys.getenv("QB_TRANS_FIT_FILE",   "data/qb_fp_translation_fit.rds")
FP_RECAL_MAPS_FILE  <- Sys.getenv("FP_RECAL_MAPS_FILE",  "data/fp_recal_maps.rds")
TE_RECAL_MAPS_FILE  <- Sys.getenv("TE_RECAL_MAPS_FILE",  "data/te_fp_recal_maps.rds")
QB_RECAL_MAPS_FILE  <- Sys.getenv("QB_RECAL_MAPS_FILE",  "data/qb_fp_recal_maps.rds")
RESID_POOLS_FILE    <- Sys.getenv("RESID_POOLS_FILE",    "output/06b_resid_pools.csv")
TE_RESID_POOLS_FILE <- Sys.getenv("TE_RESID_POOLS_FILE", "output/12d_te_resid_pools.csv")
QB_RESID_POOLS_FILE <- Sys.getenv("QB_RESID_POOLS_FILE", "output/09a_qb_resid_pools.csv")
SIM_PARAMS_FILE     <- Sys.getenv("SIM_PARAMS_FILE",     "output/06b_sim_params.csv")
TE_SIM_PARAMS_FILE  <- Sys.getenv("TE_SIM_PARAMS_FILE",  "output/12d_te_sim_params.csv")
QB_SIM_PARAMS_FILE  <- Sys.getenv("QB_SIM_PARAMS_FILE",  "output/09a_qb_sim_params.csv")
OUT_SUFFIX           <- Sys.getenv("OUT_SUFFIX", "")

# S1 shadow mode (2026-09-08): MODEL_ARCH selects RB/WR's architecture only
# -- TE has no single-stage arm (never built/graded during the D29 rebuild)
# and QB was confirmed fine as two-stage, so both always run the twostage
# path below regardless of this flag. "fp1" never touches production: it
# requires a non-empty OUT_SUFFIX so shadow output can never land on a
# production filename, and it reads its own deployment_params_fp.rds /
# fp_recal_maps_fp1.rds artifacts (R/21n, R/21e), never data/deployment_
# params.rds or data/fp_recal_maps.rds.
MODEL_ARCH <- Sys.getenv("MODEL_ARCH", "twostage")
stopifnot(MODEL_ARCH %in% c("twostage", "fp1"))
if (MODEL_ARCH == "fp1") {
  stopifnot(nzchar(OUT_SUFFIX))
}
DP_FP_FILE             <- Sys.getenv("DP_FP_FILE",             "data/deployment_params_fp.rds")
FP_RECAL_MAPS_FP1_FILE <- Sys.getenv("FP_RECAL_MAPS_FP1_FILE", "data/fp_recal_maps_fp1.rds")

cli_alert_info("Deployment params: {DP_FILE} | recal maps: {FP_RECAL_MAPS_FILE} / {TE_RECAL_MAPS_FILE} | out suffix: '{OUT_SUFFIX}' | model arch: {MODEL_ARCH}")

dp <- readRDS(DP_FILE)
cli_alert_info("Deployment models trained through {dp$rb$trained_through$season}-W{dp$rb$trained_through$week}")

if (MODEL_ARCH == "fp1") {
  dp_fp    <- readRDS(DP_FP_FILE)
  fp1_maps <- readRDS(FP_RECAL_MAPS_FP1_FILE)
  cli_alert_info("fp1 params: {DP_FP_FILE} (RB thru {dp_fp$rb$trained_through$season}-W{dp_fp$rb$trained_through$week}, arm={dp_fp$rb$arm} | WR thru {dp_fp$wr$trained_through$season}-W{dp_fp$wr$trained_through$week}, arm={dp_fp$wr$arm}) | fp1 recal maps: {FP_RECAL_MAPS_FP1_FILE}")
}

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

# fp1 (single-stage) direct conformal CDF inversion -- identical to
# R/21d_fp_single_stage_backtest.R:266-284 / R/21n's training-time
# machinery. Row-wise P(Y >= t) from a monotone per-row quantile grid.
p_at_least <- function(Qmat, probs, t) {
  n <- nrow(Qmat)
  out <- numeric(n)
  k <- length(probs)
  for (i in seq_len(n)) {
    q <- Qmat[i, ]
    if (t <= q[1]) {
      slope <- (probs[2] - probs[1]) / (q[2] - q[1])
      cdf <- probs[1] + slope * (t - q[1])
    } else if (t >= q[k]) {
      slope <- (probs[k] - probs[k - 1]) / (q[k] - q[k - 1])
      cdf <- probs[k] + slope * (t - q[k])
    } else {
      cdf <- approx(q, probs, xout = t)$y
    }
    out[i] <- 1 - cdf
  }
  pmin(pmax(out, 0), 1)
}

# fp1 point + interval + raw-probability chain for one position -- mirrors
# R/21d's per-fold serve logic (lines 423-429) using the deployed fp1
# artifacts instead of a fold-local fit. No Monte Carlo needed: the K=11
# signed conformal grid inverts directly (R/21d header: "zero sampling
# noise, deterministic reruns"). lo/hi_80 read off q10/q90, lo/hi_90 off
# q05/q95 -- the grid has no exact 50% pair (QLEVELS has .20/.30/.70/.80,
# not .25/.75), so lo/hi_50_fp is deliberately not produced; nothing
# downstream needs it yet.
score_fp1 <- function(enc, dpos) {
  X_pt <- make_matrix(enc, dpos$point$features)
  X_vl <- make_matrix(enc, dpos$vol$features)
  m_pt <- lightgbm::lgb.load(dpos$point$model_file)
  m_vl <- lightgbm::lgb.load(dpos$vol$model_file)
  pred_pt <- predict(m_pt, X_pt)
  pred_vl <- predict(m_vl, X_vl)
  stopifnot(all(is.finite(pred_pt)), all(is.finite(pred_vl)))

  qgrid <- dpos$conformal$qgrid
  qlevs <- dpos$conformal$qlevels
  alpha <- dpos$conformal$alpha
  scale <- pmax(pred_vl, 1)^alpha
  Q     <- outer(scale, qgrid)
  Q     <- sweep(Q, 1, pred_pt, "+")
  colnames(Q) <- names(qgrid)

  list(pred_fp = pred_pt, pred_vol = pred_vl,
       lo_80_fp = Q[, "q10"], hi_80_fp = Q[, "q90"],
       lo_90_fp = Q[, "q05"], hi_90_fp = Q[, "q95"],
       p_start = p_at_least(Q, qlevs, dpos$thresh["start"]),
       p_boom  = p_at_least(Q, qlevs, dpos$thresh["boom"]))
}

# Translation + recal + sim artifacts
fp_fits    <- readRDS(FP_TRANS_FITS_FILE)        # $rb, $wr
te_fit     <- readRDS(TE_TRANS_FIT_FILE)
qb_fit     <- readRDS(QB_TRANS_FIT_FILE)
fp_maps    <- readRDS(FP_RECAL_MAPS_FILE)        # RB_15+ etc.
te_maps    <- readRDS(TE_RECAL_MAPS_FILE)        # TE_12+ etc.
qb_maps    <- readRDS(QB_RECAL_MAPS_FILE)        # QB_20+ etc.

pools_csv  <- readr::read_csv(RESID_POOLS_FILE,    show_col_types = FALSE)
te_pools_csv <- readr::read_csv(TE_RESID_POOLS_FILE, show_col_types = FALSE)
qb_pools_csv <- readr::read_csv(QB_RESID_POOLS_FILE, show_col_types = FALSE)
sim_params <- readr::read_csv(SIM_PARAMS_FILE,     show_col_types = FALSE)
te_sim_csv <- readr::read_csv(TE_SIM_PARAMS_FILE,  show_col_types = FALSE)
qb_sim_csv <- readr::read_csv(QB_SIM_PARAMS_FILE,  show_col_types = FALSE)

pool_list <- function(df, pos, tiers) {
  map(set_names(tiers), function(tr) df$resid[df$position == pos & df$tier == tr])
}
pools_rb <- pool_list(pools_csv, "RB", c("low", "mid", "high"))
pools_wr <- pool_list(pools_csv, "WR", c("low", "mid", "high"))
pools_te <- pool_list(te_pools_csv, "TE", c("low", "mid", "high"))
pools_qb <- pool_list(qb_pools_csv, "QB", c("statue", "mover", "scrambler"))
stopifnot(all(lengths(pools_rb) > 0), all(lengths(pools_wr) > 0),
          all(lengths(pools_te) > 0), all(lengths(pools_qb) > 0))

rho_rb <- sim_params$rho[sim_params$position == "RB"]
rho_wr <- sim_params$rho[sim_params$position == "WR"]
rho_te <- te_sim_csv$rho[te_sim_csv$position == "TE"]

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

# --- RB: symmetric + power-law (03a-v2 mechanism), OR fp1 direct conformal ---
rb_enc <- encode_features(rb_slate)
if (MODEL_ARCH == "fp1") {
  rb_fp1 <- score_fp1(rb_enc, dp_fp$rb)
  rb_scored <- bind_cols(
    rb_slate |> select(player_id, player_name, posteam, defteam, game_id,
                       season, week, report_status, practice_status,
                       team_spread, implied_total),
    tibble(pred_fp = rb_fp1$pred_fp, pred_vol = rb_fp1$pred_vol,
           lo_80_fp = rb_fp1$lo_80_fp, hi_80_fp = rb_fp1$hi_80_fp,
           lo_90_fp = rb_fp1$lo_90_fp, hi_90_fp = rb_fp1$hi_90_fp,
           p_start = rb_fp1$p_start, p_boom = rb_fp1$p_boom)
  ) |> mutate(position = "RB", .before = 1)
} else {
  rb_pred_eff <- predict_component(rb_enc, dp$rb$eff)
  rb_pred_vol <- predict_component(rb_enc, dp$rb$vol)
  rb_pred_tot <- rb_pred_eff * rb_pred_vol
  rb_sc       <- vol_scale(rb_pred_vol, dp$rb$tot$alpha, "RB")

  rb_scored <- bind_cols(
    rb_slate |> select(player_id, player_name, posteam, defteam, game_id,
                       season, week, report_status, practice_status,
                       team_spread, implied_total),
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
}

# WR/TE depth-chart role-signal floor/ceiling (2026-09-10 audit -- see
# R/archive/oneoff/depth_chart_role_audit.R and the approved plan at
# ~/.claude/plans/unified-painting-gosling.md). Same root cause as the QB
# fix above but a smaller, two-population residual: the 2026-08-31
# carryforward fix already gives RB/WR/TE a baseline_* fallback (QB's
# db_vol never got one), so most players are fine -- but (a) cold-start
# players (is_cold_start=1) still fall to a flat draft-tier constant with
# zero player-specific role info, and (b) non-cold-start role-changers
# (is_cold_start=0) carry their OWN prior-role share/efficiency forward,
# which is actively wrong, not just missing, if their role changed. A
# cold-start-only gate (like QB's) misses population (b) entirely.
#
# Corrects BOTH the volume-share features (baseline_target_share/
# baseline_snap_share/baseline_air_yards_share) AND baseline_epa_per_opp
# (the efficiency model's own fallback) together, for the same reason the
# QB fix ended up needing both pass_eff and db_vol: WR/TE's twostage total
# is also pred_eff * pred_vol, multiplicative, so flooring volume alone
# while a flagged player's efficiency reads negative (e.g. Malik Washington
# carries a real -0.151 prior_epa_per_opp from his smaller prior role) makes
# the total WORSE, not better -- confirmed this trap BEFORE shipping this
# time, not after, unlike the QB fix's first two attempts tonight.
#
# Uses load_current_depth_chart() (R/10b_roster_helpers.R) rather than the
# QB fix's per-player-latest-snapshot approach -- unsafe for WR's 3-lane
# depth chart (see that helper's own comment). WR starter = pos_rank <= 3
# (three simultaneous lanes via pos_slot); TE starter = pos_rank == 1.
# RB explicitly excluded: audited 2026-09-10, RB's real error is
# over-ranked veteran handcuffs, a different problem, not this blind spot.
#
# Applied once, before the fp1/twostage branch below, since the corrected
# baseline_* columns feed both architectures identically (confirmed: both
# dp$wr$vol$features and dp_fp$wr$vol/point$features include the same
# baseline_target_share/snap_share/air_yards_share names) -- so this
# benefits the real (twostage, default) run AND the fp1 shadow pass with
# one correction point.
apply_role_signal_correction <- function(enc, position, starter_rank_max) {
  dc <- tryCatch(
    load_current_depth_chart(TARGET_SEASON, position, AS_OF) |>
      dplyr::rename(player_id = gsis_id),
    error = function(e) {
      cli_alert_warning("{position} depth chart fetch failed ({conditionMessage(e)}) -- role-signal floor/ceiling skipped this run")
      tibble(player_id = character(), pos_rank = integer())
    })
  if (nrow(dc) == 0) return(enc)

  d <- enc |>
    dplyr::left_join(dc |> dplyr::select(player_id, dc_rank = pos_rank), by = "player_id") |>
    dplyr::mutate(starter = !is.na(dc_rank) & dc_rank <= starter_rank_max)

  # Share floor stays at the confirmed-starter MEDIAN -- tested against a
  # 30th-percentile share floor too (2026-09-10) and it visibly weakened the
  # real under-ranked cases (Denzel Boston, Malik Washington) without a
  # matching problem to justify it; share/role volume isn't the axis where
  # "typical starter" broke down.
  #
  # Efficiency floor uses the 30th percentile instead (same bar as the
  # under_flag threshold below) -- THIS one Steve's own football read on
  # Charlie Kolar caught directly: TE starters are genuinely bimodal
  # (receiving TE1s vs. run-blocking TE1s by scheme), so the "typical
  # starter" EFFICIENCY median (0.306 EPA/opp for TE, pulled up by elite
  # receiving TEs) is too generous a floor for a legitimately low-target
  # blocking role -- it should only guarantee "at least as good as a
  # below-average REAL starter's efficiency," not "typical starter's."
  # The backup ceiling keeps using the backup median on both axes --
  # over-crediting isn't the concern on that side, no adjustment needed.
  share_cols <- c("baseline_target_share", "baseline_snap_share", "baseline_air_yards_share")
  starter_ok <- !d$is_cold_start_int & d$starter
  backup_ok  <- !d$is_cold_start_int & !d$starter
  ref_starter <- setNames(sapply(share_cols, function(cn) median(d[[cn]][starter_ok], na.rm = TRUE)), share_cols)
  ref_starter_q30 <- quantile(d$baseline_target_share[starter_ok], 0.30, na.rm = TRUE)
  ref_backup  <- setNames(sapply(share_cols, function(cn) median(d[[cn]][backup_ok], na.rm = TRUE)), share_cols)
  ref_starter_epa <- quantile(d$baseline_epa_per_opp[starter_ok], 0.30, na.rm = TRUE)
  ref_backup_epa  <- median(d$baseline_epa_per_opp[backup_ok],  na.rm = TRUE)
  if (any(is.na(ref_starter)) || is.na(ref_starter_q30) || any(is.na(ref_backup)) ||
      is.na(ref_starter_epa) || is.na(ref_backup_epa)) return(enc)

  under_flag <- (d$is_cold_start_int == 1 & d$starter) |
    (d$is_cold_start_int == 0 & d$starter & d$baseline_target_share < ref_starter_q30)
  over_flag  <- d$is_cold_start_int == 1 & !d$starter & d$baseline_target_share > ref_backup["baseline_target_share"]

  # Efficiency shrinkage (added 2026-09-10, Steve's own football read on
  # Charlie Kolar caught this): the share-column floor/ceiling above is
  # correct to leave baseline_epa_per_opp untouched via a simple pmax/pmin
  # for is_cold_start players (they have no real number to protect), but
  # for the stale_baseline_starter/backup population -- players WITH a real
  # prior_epa_per_opp -- pmax/pmin only ever RAISES a too-low number, never
  # corrects a too-HIGH one built on a thin sample. Found exactly that:
  # Kolar's 0.344 EPA/opp (elite-tier) comes from just 15 targets (barely
  # clears MIN_PRIOR_OPP=10 in R/10b5_te_slate.R:32), 7 of them deep passes
  # -- a couple of explosive plays, not demonstrated receiving talent (Steve:
  # he's a run-blocking TE1 by scheme, not a pass-catching specialist).
  # Checked the other 9 flagged TEs + 34 flagged WRs: this is systematic,
  # not a one-off -- several other thin-sample efficiency numbers in the
  # 0.6-0.8 EPA/opp range on n<50 opportunities. Empirical-Bayes shrinkage
  # toward the reference (weight K "prior opportunities" of trust in the
  # reference vs. the player's own n real opportunities) fixes this
  # generally: a 15-target sample gets pulled hard toward the reference: a
  # 77-target one (Evan Engram) barely moves.
  K_SHRINK <- 20
  plays_path <- sprintf("data/%s_plays.rds", tolower(position))
  prior_opp <- if (file.exists(plays_path)) {
    readRDS(plays_path) |>
      dplyr::filter(season == TARGET_SEASON - 1L) |>
      dplyr::group_by(player_id) |>
      dplyr::summarise(n_opp = dplyr::n(), raw_epa_per_opp = sum(epa, na.rm = TRUE) / dplyr::n(), .groups = "drop")
  } else {
    tibble(player_id = character(), n_opp = integer(), raw_epa_per_opp = double())
  }
  d <- d |> dplyr::left_join(prior_opp, by = "player_id") |>
    dplyr::mutate(n_opp = dplyr::coalesce(n_opp, 0L), raw_epa_per_opp = dplyr::coalesce(raw_epa_per_opp, 0))
  shrink_toward <- function(n, raw, ref) (n * raw + K_SHRINK * ref) / (n + K_SHRINK)

  # prior_epa_per_opp is a SEPARATE feature from baseline_epa_per_opp in
  # both dp$wr/te$eff$features -- for a non-cold-start player the two start
  # out identical (baseline_epa_per_opp = prior_epa_per_opp verbatim, see
  # R/10b5_te_slate.R:176-182), so shrinking only baseline_epa_per_opp
  # leaves the eff model still reading the player's raw, unshrunk number
  # straight off prior_epa_per_opp. Caught this via Kolar's pred_eff barely
  # moving (0.297, should have landed near 0.24) despite baseline_epa_per_opp
  # shrinking correctly -- same "missed a duplicate feature" shape as the
  # QB fix's own false starts tonight. Both must move together.
  if (any(under_flag, na.rm = TRUE)) {
    under_flag[is.na(under_flag)] <- FALSE
    cli_alert_warning("{position} depth-chart starter floor: {sum(under_flag)} player(s) ({paste(enc$player_name[under_flag], collapse=', ')}) -- baseline share floored + efficiency shrunk toward confirmed-starter reference")
    for (cn in share_cols) enc[[cn]][under_flag] <- pmax(enc[[cn]][under_flag], ref_starter[[cn]])
    shrunk <- shrink_toward(d$n_opp[under_flag], d$raw_epa_per_opp[under_flag], ref_starter_epa)
    enc$baseline_epa_per_opp[under_flag] <- shrunk
    enc$prior_epa_per_opp[under_flag]    <- shrunk
  }
  if (any(over_flag, na.rm = TRUE)) {
    over_flag[is.na(over_flag)] <- FALSE
    cli_alert_warning("{position} depth-chart backup ceiling: {sum(over_flag)} player(s) ({paste(enc$player_name[over_flag], collapse=', ')}) -- baseline share capped + efficiency shrunk toward confirmed-backup reference")
    for (cn in share_cols) enc[[cn]][over_flag] <- pmin(enc[[cn]][over_flag], ref_backup[[cn]])
    shrunk <- shrink_toward(d$n_opp[over_flag], d$raw_epa_per_opp[over_flag], ref_backup_epa)
    enc$baseline_epa_per_opp[over_flag] <- shrunk
    enc$prior_epa_per_opp[over_flag]    <- shrunk
  }
  enc
}

# --- WR: asymmetric signed qsets + power-law (04c mechanism), OR fp1 ---
wr_enc <- encode_features(wr_slate)
wr_enc <- apply_role_signal_correction(wr_enc, "WR", starter_rank_max = 3L)
if (MODEL_ARCH == "fp1") {
  wr_fp1 <- score_fp1(wr_enc, dp_fp$wr)
  wr_scored <- bind_cols(
    wr_slate |> select(player_id, player_name, posteam, defteam, game_id,
                       season, week, report_status, practice_status,
                       team_spread, implied_total),
    tibble(pred_fp = wr_fp1$pred_fp, pred_vol = wr_fp1$pred_vol,
           lo_80_fp = wr_fp1$lo_80_fp, hi_80_fp = wr_fp1$hi_80_fp,
           lo_90_fp = wr_fp1$lo_90_fp, hi_90_fp = wr_fp1$hi_90_fp,
           p_start = wr_fp1$p_start, p_boom = wr_fp1$p_boom)
  ) |> mutate(position = "WR", .before = 1)
} else {
  wr_pred_eff <- predict_component(wr_enc, dp$wr$eff)
  wr_pred_vol <- predict_component(wr_enc, dp$wr$vol)
  wr_pred_tot <- wr_pred_eff * wr_pred_vol
  wr_sc       <- vol_scale(wr_pred_vol, dp$wr$tot$alpha, "WR")

  wr_scored <- bind_cols(
    wr_slate |> select(player_id, player_name, posteam, defteam, game_id,
                       season, week, report_status, practice_status,
                       team_spread, implied_total),
    tibble(pred_eff = wr_pred_eff),
    asym_cols(wr_pred_vol, dp$wr$vol$qset, "vol"),
    asym_cols(wr_pred_tot, dp$wr$tot$qset, "tot", scale = wr_sc)
  ) |> mutate(position = "WR", .before = 1)
}

# --- TE: asymmetric signed qsets + power-law (12c mechanism, WR clone) ---
te_enc <- encode_features(te_slate)
te_enc <- apply_role_signal_correction(te_enc, "TE", starter_rank_max = 1L)
te_pred_eff <- predict_component(te_enc, dp$te$eff)
te_pred_vol <- predict_component(te_enc, dp$te$vol)
te_pred_tot <- te_pred_eff * te_pred_vol
te_sc       <- vol_scale(te_pred_vol, dp$te$tot$alpha, "TE")

te_scored <- bind_cols(
  te_slate |> select(player_id, player_name, posteam, defteam, game_id,
                     season, week, report_status, practice_status,
                     team_spread, implied_total),
  tibble(pred_eff = te_pred_eff),
  asym_cols(te_pred_vol, dp$te$vol$qset, "vol"),
  asym_cols(te_pred_tot, dp$te$tot$qset, "tot", scale = te_sc)
) |> mutate(position = "TE", .before = 1)

# --- QB: four symmetric components + const-additive combined (08c) ---
qb_enc <- encode_features(qb_slate)
qb_pred <- map(dp$qb$components, function(spec) predict_component(qb_enc, spec))

# QB depth-chart starter floor -- official, structured, point-in-time
# nflverse depth-chart data (NOT beat-reporter text -- same category
# CLAUDE.md already allows training on, see R/11b's injury layer) closes a
# real blind spot: db_vol's own features (wt_dropbacks/wt_team_total_plays/
# wt_team_pass_rate) are ALL NA before a player's current-season debut, so
# in a debut week BOTH db_vol and pass_eff collapse to draft_tier_int +
# is_cold_start_int + opponent terms alone -- a backup who becomes the new
# starter via free agency/trade (is_cold_start=1, zero current-season
# games, often a low draft tier) reads exactly like a real scrub even
# though the team's OWN depth chart lists him QB1. Found 2026-09-09 on
# Malik Willis (MIA), model rank 79 of ~90 QBs, officially depth-chart QB1
# since he signed months ago. apply_news_override's capped +/-10pp nudge
# (built for injury-style corrections, see below) cannot fix a gap this
# large.
#
# Must correct the ACTUAL components fed to simulate_qb() below, not a
# derived summary -- two dead ends found first (2026-09-09):
#   v1 floored db_vol alone, which made things WORSE for players whose
#     pass_eff is negative (also driven by the same is_cold_start tier
#     fallback) -- a bigger volume times a negative efficiency is a
#     bigger negative total.
#   v2 floored qb_pred_tot (the pass_eff*db_vol+rush_dir summary), which
#     changed NOTHING: simulate_qb() never reads pred_tot -- it redraws
#     eff/db/rush/carry independently from each component's OWN quantile
#     matrix (pred_pass_eff, pred_db, ...) and recombines them inside the
#     Monte Carlo. Flooring a column nothing downstream consumes is a
#     silent no-op, not a fix.
# The comparison population also has to be OTHER CONFIRMED STARTERS, not
# just "not cold start" -- the QB slate carries every rostered QB2/QB3 too
# (88 total vs. 36 real depth-chart starters this week), and most of those
# backups clear is_cold_start on career mop-up-duty dropbacks without ever
# being a real starter; a median over all 84 non-flagged QBs is dominated
# by benchwarmers, not the 32 real starters among them.
# Both pass_eff AND db_vol get replaced (not just floored) for flagged
# players: their existing point estimates carry no real signal either way
# (built entirely from draft-tier/cold-start proxies that don't apply to
# this population), so pmax-floor vs. replace only matters when the
# proxy happens to already beat the real-starter median, which isn't a
# case worth preserving here.
qb_depth_chart_starters <- tryCatch({
  nflreadr::load_depth_charts(TARGET_SEASON) |>
    filter(pos_abb == "QB") |>
    mutate(dt_parsed = as.POSIXct(dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) |>
    filter(!is.na(dt_parsed), dt_parsed <= AS_OF) |>
    group_by(gsis_id) |>
    slice_max(dt_parsed, n = 1, with_ties = FALSE) |>   # latest snapshot per player, ANY rank --
    ungroup() |>                                        # must resolve "current rank" before filtering
    filter(pos_rank == 1L) |>                            # on rank==1, or a stale "was #1 once" date wins
    pull(gsis_id)                                        # (real bug, 2026-09-09: flagged Rudolph/O'Connell
}, error = function(e) {                                  # as starters off a preseason camp-battle snapshot)
  cli_alert_warning("QB depth chart fetch failed ({conditionMessage(e)}) -- starter-floor check skipped this run")
  character(0)
})

qb_floor_flag <- qb_enc$player_id %in% qb_depth_chart_starters &
  qb_enc$is_cold_start_int == 1 & qb_enc$games_played_so_far == 0

if (any(qb_floor_flag)) {
  established_starter_mask <- qb_enc$player_id %in% qb_depth_chart_starters &
    qb_enc$is_cold_start_int == 0
  pass_eff_repl <- median(qb_pred$pass_eff[established_starter_mask], na.rm = TRUE)
  db_vol_repl   <- median(qb_pred$db_vol[established_starter_mask],   na.rm = TRUE)
  if (!is.na(pass_eff_repl) && !is.na(db_vol_repl)) {
    cli_alert_warning("QB depth-chart starter floor: {sum(qb_floor_flag)} player{?s} flagged as official QB1 with no usable role history ({paste(qb_enc$player_name[qb_floor_flag], collapse=', ')}) -- pass_eff/db_vol replaced with established-starter median ({round(pass_eff_repl,3)} EPA/db, {round(db_vol_repl,1)} db, n={sum(established_starter_mask)} real starters)")
    qb_pred$pass_eff <- if_else(qb_floor_flag, pmax(qb_pred$pass_eff, pass_eff_repl), qb_pred$pass_eff)
    qb_pred$db_vol   <- if_else(qb_floor_flag, pmax(qb_pred$db_vol,   db_vol_repl),   qb_pred$db_vol)
  }
}

qb_pred_tot <- qb_pred$pass_eff * qb_pred$db_vol + qb_pred$rush_dir

qb_scored <- bind_cols(
  qb_slate |> select(player_id, player_name, posteam, defteam, game_id,
                     season, week, report_status, practice_status,
                     team_spread, implied_total),
  sym_cols(qb_pred$pass_eff,  dp$qb$qs$pass_eff, "pass_eff"),
  sym_cols(qb_pred$db_vol,    dp$qb$qs$db,       "db"),
  sym_cols(qb_pred$rush_dir,  dp$qb$qs$rush,     "rush"),
  sym_cols(qb_pred$carry_vol, dp$qb$qs$carry,    "carry"),
  sym_cols(qb_pred_tot,       dp$qb$qs$tot,      "tot")
) |> mutate(position = "QB", .before = 1,
            depth_chart_starter_floor = qb_floor_flag)

rbwr_pred_col   <- if (MODEL_ARCH == "fp1") "pred_fp" else "pred_tot"
rbwr_pred_label <- if (MODEL_ARCH == "fp1") "FP" else "EPA"
rb_pred_mean    <- round(mean(rb_scored[[rbwr_pred_col]]), 2)
wr_pred_mean    <- round(mean(wr_scored[[rbwr_pred_col]]), 2)
cli_alert_success("Predictions: RB {rbwr_pred_label} mean={rb_pred_mean} | WR {rbwr_pred_label} mean={wr_pred_mean} | TE {round(mean(te_pred_tot), 2)} EPA | QB {round(mean(qb_pred_tot), 2)} EPA")

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

  # Vegas translation term (2026-07-26): the saved fits carry it_c with the
  # training center stored as attr; unposted lines coalesce to neutral zero.
  it_ctr <- attr(fit, "it_center") %||% NA_real_
  it_c   <- if (is.finite(it_ctr)) {
    coalesce(scored$implied_total, it_ctr) - it_ctr
  } else rep(0, n)

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

    fp <- predict(fit, tibble(total_epa = epa_draw, opportunities = opp_draw,
                              it_c = it_c)) + res
    hit_start <- hit_start + (fp >= thresh["start"])
    hit_boom  <- hit_boom  + (fp >= thresh["boom"])
  }

  scored |> mutate(p_start = hit_start / N_SIM, p_boom = hit_boom / N_SIM)
}

if (MODEL_ARCH == "fp1") {
  cli_alert_info("RB/WR: fp1 direct conformal CDF inversion already produced p_start/p_boom -- no Monte Carlo simulation needed (R/21d header: zero sampling noise, deterministic reruns)")
} else {
  set.seed(SIM_SEED[["RB"]])
  rb_scored <- simulate_rbwr(rb_scored, fp_fits$rb, pools_rb, tier_rb, rho_rb, THRESH$RB)
  cli_alert_success("RB simulation complete")

  set.seed(SIM_SEED[["WR"]])
  wr_scored <- simulate_rbwr(wr_scored, fp_fits$wr, pools_wr, tier_wr, rho_wr, THRESH$WR)
  cli_alert_success("WR simulation complete")
}

simulate_qb <- function(scored, fit, pools, chol_m, thresh) {
  n       <- nrow(scored)
  it_ctr  <- attr(fit, "it_center") %||% NA_real_
  it_c    <- if (is.finite(it_ctr)) {
    coalesce(scored$implied_total, it_ctr) - it_ctr
  } else rep(0, n)
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
                              carries   = carry_draw,
                              it_c      = it_c)) + res
    hit_start <- hit_start + (fp >= thresh["start"])
    hit_boom  <- hit_boom  + (fp >= thresh["boom"])
  }

  scored |> mutate(p_start = hit_start / N_SIM, p_boom = hit_boom / N_SIM)
}

set.seed(SIM_SEED[["QB"]])
qb_scored <- simulate_qb(qb_scored, qb_fit, pools_qb, chol_qb, THRESH$QB)
cli_alert_success("QB simulation complete")

# TE no longer needs to simulate last for RNG reasons -- each position now
# seeds its own stream (SIM_SEED above), so call order cannot jitter another
# position's draws. Kept last anyway simply to avoid reordering anything else
# in this commit; the old jitter risk this comment used to warn about
# (a QB row moving 2.6pp from a shared stream) can no longer happen.
set.seed(SIM_SEED[["TE"]])
te_scored <- simulate_rbwr(te_scored, te_fit, pools_te, tier_te, rho_te, THRESH$TE)
cli_alert_success("TE simulation complete")

# ===========================================================================
# 4. RECALIBRATION MAPS -> FINAL PROBABILITIES
# ===========================================================================

cli_h1("Recalibration maps")

apply_maps <- function(scored, map_start, map_boom, vol, star_bucket = NULL) {
  # Widened uniform signature (2026-07-26): function(p, vol, spread, implied).
  # Slate Vegas columns may be NA (unposted lines) -- the Vegas closures
  # coalesce internally to a neutral adjustment.
  # D27 (2026-09-05): maps flagged needs_bucket take a fifth argument,
  # the RB trailing-FP star bucket; all other closures keep 4 args.
  call_map <- function(m, p) {
    if (isTRUE(m$needs_bucket)) {
      stopifnot(!is.null(star_bucket), !any(is.na(star_bucket)))
      m$map(p, vol, scored$team_spread, scored$implied_total, star_bucket)
    } else {
      m$map(p, vol, scored$team_spread, scored$implied_total)
    }
  }
  p_start_recal <- pmin(pmax(call_map(map_start, scored$p_start), 0), 1)
  p_boom_recal  <- pmin(pmax(call_map(map_boom, scored$p_boom), 0), 1)
  scored |>
    mutate(
      p_start_recal = p_start_recal,
      p_boom_recal  = pmin(p_boom_recal, p_start_recal),   # coherence cap
      recal_method_start = map_start$method,
      recal_method_boom  = map_boom$method
    )
}

# D27: RB star buckets, ex-ante within the slate universe. trailing FP
# uses strictly-prior completed games only (lag construction), so the
# target week never leaks even on hindcast replays.
rb_scored <- star_assign_buckets(rb_scored, star_trailing_fp(2016:TARGET_SEASON))

rbwr_maps <- if (MODEL_ARCH == "fp1") fp1_maps else fp_maps
rb_scored <- apply_maps(rb_scored, rbwr_maps[["RB_15+"]], rbwr_maps[["RB_20+"]], rb_scored$pred_vol,
                        star_bucket = rb_scored$star_bucket)
wr_scored <- apply_maps(wr_scored, rbwr_maps[["WR_15+"]], rbwr_maps[["WR_20+"]], wr_scored$pred_vol)
te_scored <- apply_maps(te_scored, te_maps[["TE_12+"]], te_maps[["TE_17+"]], te_scored$pred_vol)
qb_scored <- apply_maps(qb_scored, qb_maps[["QB_20+"]], qb_maps[["QB_25+"]], qb_scored$pred_carry)

# ===========================================================================
# 4b. NEWS OVERRIDE LAYER (2026-09-09) -- live per CLAUDE.md's carve-out
# for text/beat-reporter signal: "lives in a live override layer instead,
# graded in-season, not trained on." R/10i_news_override.R produces the
# candidate file this reads; NEVER trained on, applied here at scoring
# time only. Bounded nudge (max MAX_OVERRIDE_SHIFT_PP, scaled by the
# classifier's own confidence) -- never a wholesale replacement of the
# recalibrated probability. Optional and silent when absent: no override
# file for this week (R/10i hasn't run, or nothing qualified) is a
# no-op, not a failure -- matches the "must apply live, never gate
# behind a season of proof" design Steve set 2026-09-09, with the
# pre/post values always kept side by side so nothing is silently
# overwritten (same "nulls with receipts" discipline as everywhere else
# in this pipeline).
NEWS_OVERRIDES_FILE  <- Sys.getenv("NEWS_OVERRIDES_FILE",
  sprintf("data/news_overrides_%d_w%02d.csv", TARGET_SEASON, TARGET_WEEK))
MAX_OVERRIDE_SHIFT_PP <- 0.10

apply_news_override <- function(scored, overrides) {
  if (is.null(overrides) || nrow(overrides) == 0) {
    return(scored |> mutate(
      p_start_recal_preoverride = p_start_recal,
      p_boom_recal_preoverride  = p_boom_recal,
      override_flag_type  = NA_character_,
      override_confidence = NA_real_,
      override_reason     = NA_character_
    ))
  }
  ov <- overrides |> distinct(gsis_id, .keep_all = TRUE)
  scored |>
    left_join(ov |> select(gsis_id, override_flag_type = flag_type,
                           override_confidence = confidence, override_reason = reason),
              by = c("player_id" = "gsis_id")) |>
    mutate(
      p_start_recal_preoverride = p_start_recal,
      p_boom_recal_preoverride  = p_boom_recal,
      .shift = case_when(
        override_flag_type == "role_change_up"   ~  MAX_OVERRIDE_SHIFT_PP * coalesce(override_confidence, 0),
        override_flag_type == "role_change_down" ~ -MAX_OVERRIDE_SHIFT_PP * coalesce(override_confidence, 0),
        TRUE ~ 0
      ),
      p_start_recal = pmin(pmax(p_start_recal + .shift, 0), 1),
      p_boom_recal  = pmin(pmax(p_boom_recal  + .shift, 0), 1),
      p_boom_recal  = pmin(p_boom_recal, p_start_recal)   # re-enforce coherence after the nudge
    ) |>
    select(-.shift)
}

news_overrides <- if (file.exists(NEWS_OVERRIDES_FILE)) {
  ov <- readr::read_csv(NEWS_OVERRIDES_FILE, show_col_types = FALSE) |>
    mutate(gsis_id = as.character(gsis_id))
  cli_alert_info("News overrides: {NEWS_OVERRIDES_FILE} ({nrow(ov)} candidates)")
  ov
} else {
  cli_alert_info("No news override file for this week ({NEWS_OVERRIDES_FILE}) -- skipping (expected if R/10i hasn't run, or nothing qualified)")
  NULL
}

rb_scored <- apply_news_override(rb_scored, news_overrides)
wr_scored <- apply_news_override(wr_scored, news_overrides)
te_scored <- apply_news_override(te_scored, news_overrides)
qb_scored <- apply_news_override(qb_scored, news_overrides)

# QB starter-before-backup invariant (Steve, 2026-09-10): a confirmed
# current-week backup (official depth chart rank 2+) must never outrank a
# confirmed current-week starter (rank 1) in ANY published number -- a
# bench QB gets ~0 real snaps this week barring injury, a starter gets a
# full game, and no amount of individual-component modeling nuance changes
# that. Applied as a final, explicit, easy-to-audit constraint AFTER
# everything else (simulation, recal maps, news override) rather than
# chased through the eff x vol x simulation chain: the depth-chart-starter-
# floor block above already fixed db_vol/pass_eff for players like Malik
# Willis, but that alone wasn't sufficient -- longtime backups (e.g. Mason
# Rudolph) who cleared the historical dropback threshold from old spot
# starts are NOT is_cold_start, so they keep their own (non-floored)
# prediction, and the Monte Carlo's rush-tier residual-pool draw can still
# put a lower-mean backup ahead of a higher-mean starter in the final
# probability. Only touches an actual violation; everyone else is
# untouched.
qb_current_rank <- tryCatch({
  nflreadr::load_depth_charts(TARGET_SEASON) |>
    filter(pos_abb == "QB") |>
    mutate(dt_parsed = as.POSIXct(dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) |>
    filter(!is.na(dt_parsed), dt_parsed <= AS_OF) |>
    group_by(gsis_id) |>
    slice_max(dt_parsed, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(gsis_id, current_pos_rank = pos_rank)
}, error = function(e) {
  cli_alert_warning("QB depth chart fetch failed (starter-before-backup check) -- skipped this run")
  tibble(gsis_id = character(), current_pos_rank = integer())
})

qb_scored <- qb_scored |> left_join(qb_current_rank, by = c("player_id" = "gsis_id"))
starter_mask <- !is.na(qb_scored$current_pos_rank) & qb_scored$current_pos_rank == 1L
backup_mask  <- !is.na(qb_scored$current_pos_rank) & qb_scored$current_pos_rank >= 2L

if (any(starter_mask) && any(backup_mask)) {
  min_starter_start <- min(qb_scored$p_start_recal[starter_mask], na.rm = TRUE)
  min_starter_boom  <- min(qb_scored$p_boom_recal[starter_mask],  na.rm = TRUE)
  violation <- backup_mask & (qb_scored$p_start_recal >= min_starter_start)
  if (any(violation)) {
    cli_alert_warning("QB starter-before-backup invariant: {sum(violation)} confirmed backup(s) ({paste(qb_scored$player_name[violation], collapse=', ')}) scored at/above the weakest confirmed starter -- capped below (a bench QB cannot outrank a starter)")
    cap_start <- max(min_starter_start - 0.005, 0)
    cap_boom  <- max(min_starter_boom  - 0.005, 0)
    qb_scored$p_start_recal[violation] <- pmin(qb_scored$p_start_recal[violation], cap_start)
    qb_scored$p_boom_recal[violation]  <- pmin(qb_scored$p_boom_recal[violation],  cap_boom, qb_scored$p_start_recal[violation])
  }
}
qb_scored <- qb_scored |> select(-current_pos_rank)

for (d in list(rb_scored, wr_scored, te_scored, qb_scored)) {
  stopifnot(!any(is.na(d$p_start_recal)), !any(is.na(d$p_boom_recal)),
            all(d$p_boom_recal <= d$p_start_recal + 1e-12))
}

cli_alert_success("Maps applied: RB={rbwr_maps[['RB_15+']]$method}/{rbwr_maps[['RB_20+']]$method} WR={rbwr_maps[['WR_15+']]$method}/{rbwr_maps[['WR_20+']]$method} TE={te_maps[['TE_12+']]$method}/{te_maps[['TE_17+']]$method} QB={qb_maps[['QB_20+']]$method}/{qb_maps[['QB_25+']]$method}")

# ===========================================================================
# 5. SAVE SCORED SLATE
# ===========================================================================

cli_h1("Save scored slate")

id_cols <- c("position", "player_id", "player_name", "posteam", "defteam",
             "game_id", "season", "week", "report_status", "practice_status")
prob_cols <- c("p_start", "p_boom", "p_start_recal", "p_boom_recal",
               "recal_method_start", "recal_method_boom",
               "p_start_recal_preoverride", "p_boom_recal_preoverride",
               "override_flag_type", "override_confidence", "override_reason")

if (MODEL_ARCH == "fp1") {
  # fp1 RB/WR carry pred_fp/lo_XX_fp (FP-space), TE/QB carry pred_tot/lo_XX_tot
  # (EPA-space) in the SAME run/file -- both column families always exist
  # (NA where not applicable) so a fp1 row's FP-space numbers can never
  # silently occupy the EPA-space pred_tot column ("a silent unit change
  # cannot propagate", per the S1 plan note). Never runs in the default
  # (twostage) path, so production's scored-slate schema is untouched.
  slim <- function(d, vol_col) {
    d <- d |>
      mutate(thresh_start = THRESH[[position[1]]]["start"],
             thresh_boom  = THRESH[[position[1]]]["boom"],
             pred_vol_out = .data[[vol_col]])
    for (col in c("pred_tot", "lo_80_tot", "hi_80_tot", "lo_90_tot", "hi_90_tot",
                  "pred_fp",  "lo_80_fp",  "hi_80_fp",  "lo_90_fp",  "hi_90_fp")) {
      if (!col %in% names(d)) d[[col]] <- NA_real_
    }
    d |>
      select(all_of(id_cols), thresh_start, thresh_boom,
             pred_vol = pred_vol_out,
             pred_tot, lo_80_tot, hi_80_tot, lo_90_tot, hi_90_tot,
             pred_fp,  lo_80_fp,  hi_80_fp,  lo_90_fp,  hi_90_fp,
             all_of(prob_cols))
  }
} else {
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
}

scored_all <- bind_rows(
  slim(rb_scored, "pred_vol"),
  slim(wr_scored, "pred_vol"),
  slim(te_scored, "pred_vol"),
  slim(qb_scored, "pred_carry")
) |>
  arrange(position, desc(p_start_recal))

out_scored <- sprintf("output/10c_scored_slate_%s%s.csv", WTAG, OUT_SUFFIX)
readr::write_csv(scored_all, out_scored)
cli_alert_success("{out_scored} ({nrow(scored_all)} rows)")

# Locked-probabilities ledger: every run appends the rows it scored (all
# pre-kickoff by construction). Receipts (10d) grade the LATEST run per
# player -- the final statement made before that player's game kicked off.
ledger_path <- sprintf("output/10c_ledger_%s%s.csv", WTAG, OUT_SUFFIX)
ledger_rows <- scored_all |>
  left_join(kickoffs, by = "game_id") |>
  mutate(run_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
         as_of  = format(AS_OF, "%Y-%m-%d %H:%M:%S"),
         run_mode = RUN_MODE)
readr::write_csv(ledger_rows, ledger_path, append = file.exists(ledger_path))
cli_alert_success("{ledger_path} (+{nrow(ledger_rows)} rows, mode={RUN_MODE})")

# Full per-position detail (all interval columns) for downstream 10d use
out_detail <- sprintf("output/10c_scored_detail_%s%s.csv", WTAG, OUT_SUFFIX)
readr::write_csv(bind_rows(rb_scored |> mutate(across(everything(), as.character)),
                           wr_scored |> mutate(across(everything(), as.character)),
                           te_scored |> mutate(across(everything(), as.character)),
                           qb_scored |> mutate(across(everything(), as.character))),
                 out_detail)
cli_alert_success("{out_detail}")

# ===========================================================================
# 6. RECONCILIATION VS BACKTEST CHAIN (hindcast weeks only)
# ===========================================================================

cli_h1("Reconciliation vs backtest chain")

if (MODEL_ARCH == "fp1") {
  cli_alert_info("fp1 shadow run: skipping two-stage reconciliation -- comparing single-stage probabilities against the two-stage backtest baseline is not a valid check for a different architecture. S2 (R/21p_shadow_grade.R) covers fp1 vs production vs ECR instead.")
  cli_h1("Step 10c complete -- {TARGET_SEASON} week {TARGET_WEEK} ({RUN_MODE}, fp1 shadow)")
  quit(save = "no", status = 0)
}

if (n_skipped > 0) {
  cli_alert_info("Partial slate ({RUN_MODE} mode, {n_skipped} games skipped) -- reconciliation only runs on full-slate hindcasts.")
  cli_h1("Step 10c complete -- {TARGET_SEASON} week {TARGET_WEEK} ({RUN_MODE})")
  quit(save = "no", status = 0)
}

# FIXED 2026-09-05 (found by the D27 ship gates): these pointed at the
# unsuffixed rung-2-era files, so every post-D24 hindcast reconciled the
# volfix deployment against the PRE-volfix backtest -- guaranteed WR/TE
# breach. The volfix backtest lives in the _volfix-prefixed files (D24
# refit); the unsuffixed ones are the frozen rung-2 receipts.
bt_rbwr_path <- "output/06c_volfix_recal_probabilities.csv"
bt_te_path   <- "output/12e_te_volfix_recal_probabilities.csv"
bt_qb_path   <- "output/09b_qb_recal_probabilities.csv"

bt_col <- function(stem, method) {
  if (method == "raw") stem else paste0(stem, "_", method)
}

bt_rbwr <- readr::read_csv(bt_rbwr_path, show_col_types = FALSE) |>
  filter(season == TARGET_SEASON, week == TARGET_WEEK)
bt_te <- readr::read_csv(bt_te_path, show_col_types = FALSE) |>
  filter(season == TARGET_SEASON, week == TARGET_WEEK)
bt_qb <- readr::read_csv(bt_qb_path, show_col_types = FALSE) |>
  filter(season == TARGET_SEASON, week == TARGET_WEEK) |>
  mutate(position = "QB")

if (nrow(bt_rbwr) + nrow(bt_te) + nrow(bt_qb) == 0) {
  cli_alert_warning("No backtest rows for {TARGET_SEASON} week {TARGET_WEEK} -- future week, reconciliation skipped.")
} else {

  pick_bt <- function(bt, pos, start_map, boom_map) {
    # D27: star_platt has no column in the production backtest file --
    # its honest walk-forward columns live in the 18d validation output
    # (the 10acand chain, i.e. the deployed models' own cal folds).
    # Cross-chain caveat noted there; a bounds breach is a STOP as ever.
    if (pos == "RB" && start_map$method == "star_platt") {
      return(readr::read_csv("output/18d_rb_star_recal_probabilities.csv",
                             show_col_types = FALSE) |>
               filter(season == TARGET_SEASON, week == TARGET_WEEK) |>
               transmute(player_id, position = "RB",
                         bt_p_start = p_start_new, bt_p_boom = p_boom_new))
    }
    bt |>
      filter(position == pos) |>
      transmute(player_id, position,
                bt_p_start = .data[[bt_col("p_start", start_map$method)]],
                bt_p_boom  = .data[[bt_col("p_boom",  boom_map$method)]])
  }

  bt_all <- bind_rows(
    pick_bt(bt_rbwr, "RB", fp_maps[["RB_15+"]], fp_maps[["RB_20+"]]),
    pick_bt(bt_rbwr, "WR", fp_maps[["WR_15+"]], fp_maps[["WR_20+"]]),
    pick_bt(bt_te,   "TE", te_maps[["TE_12+"]], te_maps[["TE_17+"]]),
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

  out_recon <- sprintf("output/10c_reconciliation_%s%s.csv", WTAG, OUT_SUFFIX)
  out_recon_sum <- sprintf("output/10c_reconciliation_summary_%s%s.csv", WTAG, OUT_SUFFIX)
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
