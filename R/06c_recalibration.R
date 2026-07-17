# R/06c_recalibration.R
# Step 6c: Walk-forward recalibration of P(FP >= 15) and P(FP >= 20).
#
# MOTIVATION: After the 2026-07-05 investigation, every structural layer is
# individually calibrated (EPA interval tails marginal + conditional, volume
# tails via 04c, translation mean curve via spline, empirical residual pools,
# copula dependence) yet boom probabilities still understate by +5-7pp for
# both positions -- several ~1-2pp effects compounding, no single culprit.
# The principled finish is a thin recalibration map from stated probability
# to empirical rate, fit ONLY on past weeks and applied forward. Train and
# eval are separated in time, so this is graded on weeks it never saw --
# same walk-forward honesty discipline as the EPA models, not a fudge.
#
# 2026-07-17 EXTENSION (volume-conditional maps): the pooled maps are honest
# on average but blind to WHO the probability belongs to. Diagnostic on the
# 07-05 run, stratified by EX-ANTE predicted volume (pred_vol -- deployable,
# no game-script conditioning): WR streamer stratum (pred <= 5 targets)
# understates P(15+) by +3.8pp (6.1 stated vs 9.9 actual); WR high-volume
# stratum overstates P(20+) by -6.7pp. RB strata all within +-1.8pp.
# A map whose only input is the stated probability cannot fix conditional
# miscalibration, so three volume-aware candidates enter the bake-off:
#   * strat_platt / strat_iso: existing fitters, fit within ex-ante volume
#     strata (pooled fallback when a stratum has < MIN_STRAT_N train rows)
#   * platt_vol: logistic regression of hit on logit(p) + pred_vol --
#     smooth 3-parameter conditional map, no thin-slice problem
#
# DESIGN:
#   - Input: output/06b_fp_sim_probabilities.csv (product config:
#     spline translation + 03a-v2 RB + 04c asymmetric WR)
#   - Maps: position (RB/WR) x threshold (15+/20+)
#   - Methods judged by a PRE-COMMITTED rule (updated 2026-07-17, committed
#     BEFORE the extended run):
#       PRIMARY:  n-weighted mean |bin delta| over (ex-ante stratum x
#                 probability bin) cells on the eval window. Stratified
#                 because the pooled version of this metric is exactly what
#                 the incumbent pooled maps saturate; conditional honesty is
#                 the property the product needs.
#       SANITY:   Brier score must not degrade vs raw
#     Incumbents (pooled platt/iso) compete under the same metric; if no
#     conditional map beats them out-of-sample, the incumbent stays.
#   - Walk-forward: 2014-2015 are burn-in; from 2016 on, each season-week is
#     scored by maps fit on all rows strictly before it, refit weekly
#   - Coherence: recalibrated P(20+) capped at recalibrated P(15+) per row
#   - Deployment: winning-method maps refit on ALL data -> data/fp_recal_maps.rds
#     Each deployed map has uniform signature function(p, pred_vol) so the
#     production runner does not need to know which method won.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

EVAL_START_SEASON <- 2016L   # 2014-2015 burn-in
P_EPS             <- 1e-4
MIN_STRAT_N       <- 300L    # below this many train rows, stratum falls back
                             # to the pooled fit (pre-committed, not tuned)

# Ex-ante volume strata (pred_vol = model's predicted opportunities, known
# pre-kickoff). Cut points mirror the roster concepts: streamer / mid /
# locked-in starter. Stratifying by OBSERVED volume would condition on game
# script (see QB 08b false-STOP lesson) -- ex-ante only.
# RB low boundary moved 8 -> 10 (2026-07-17, after the 06f veto): volume-
# model shrinkage means pred_vol <= 8 holds only 57 rows, so the low
# stratum always hit the pooled fallback and the streamer roster
# (pred_vol < 10, where the two-product router actually cuts) ran +3.25pp
# cold. At 10 the stratum has ~1,800 rows and fits its own map.
STRATA_BREAKS <- list(RB = c(-Inf, 10, 14, Inf), WR = c(-Inf, 5, 9, Inf))
STRATA_LABELS <- c("exante_low", "exante_mid", "exante_high")

stratum_of <- function(position, pred_vol) {
  out <- character(length(pred_vol))
  for (pos in names(STRATA_BREAKS)) {
    idx <- which(position == pos)
    if (length(idx)) {
      out[idx] <- as.character(cut(pred_vol[idx], STRATA_BREAKS[[pos]],
                                   labels = STRATA_LABELS))
    }
  }
  factor(out, levels = STRATA_LABELS)
}

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)

# ===========================================================================
# RECALIBRATION FITTERS -- each returns a prediction function
# ===========================================================================

# NOTE on closures: everything a returned prediction function needs is bound
# LOCALLY before the closure is built. Deployment maps are saveRDS'd and
# loaded by the production runner in a fresh session -- globalenv bindings
# (P_EPS, STRATA_LABELS, helper fns) are NOT serialized with a closure, so a
# closure that reaches for them would only work in the session that fit it.

clamp_p <- function(p) pmin(pmax(p, P_EPS), 1 - P_EPS)

fit_platt <- function(p, hit) {
  clamp <- local({ eps <- P_EPS; function(q) pmin(pmax(q, eps), 1 - eps) })
  df  <- tibble(x = qlogis(clamp(p)), y = hit)
  fit <- tryCatch(glm(y ~ x, family = binomial, data = df),
                  error = function(e) NULL, warning = function(w) {
                    suppressWarnings(glm(y ~ x, family = binomial, data = df))
                  })
  if (is.null(fit)) return(identity)
  function(pnew) {
    as.numeric(predict(fit, newdata = data.frame(x = qlogis(clamp(pnew))),
                       type = "response"))
  }
}

fit_isotonic <- function(p, hit) {
  iso <- isoreg(p, as.numeric(hit))
  # isoreg returns yf ordered by sorted x
  fun <- tryCatch(
    approxfun(sort(iso$x), iso$yf, method = "linear", rule = 2, ties = mean),
    error = function(e) NULL
  )
  if (is.null(fun)) return(identity)
  function(pnew) pmin(pmax(fun(pnew), 0), 1)
}

# Stratified wrapper: fit `fitter` within each ex-ante stratum, falling back
# to the pooled fit when a stratum is thin. Returns function(pnew, snew).
fit_strat <- function(p, hit, stratum, fitter) {
  labs   <- STRATA_LABELS
  pooled <- fitter(p, hit)
  maps <- map(set_names(labs), function(s) {
    idx <- which(stratum == s)
    if (length(idx) >= MIN_STRAT_N) fitter(p[idx], hit[idx]) else pooled
  })
  function(pnew, snew) {
    out <- rep(NA_real_, length(pnew))
    for (s in labs) {
      idx <- which(snew == s)
      if (length(idx)) out[idx] <- maps[[s]](pnew[idx])
    }
    out
  }
}

# Smooth conditional map: hit ~ logit(p) + pred_vol. Returns
# function(pnew, vnew); NULL on fit failure (caller falls back to pooled).
fit_platt_vol <- function(p, hit, vol) {
  clamp <- local({ eps <- P_EPS; function(q) pmin(pmax(q, eps), 1 - eps) })
  df  <- tibble(x = qlogis(clamp(p)), v = vol, y = hit)
  fit <- tryCatch(glm(y ~ x + v, family = binomial, data = df),
                  error = function(e) NULL, warning = function(w) {
                    suppressWarnings(glm(y ~ x + v, family = binomial, data = df))
                  })
  if (is.null(fit)) return(NULL)
  function(pnew, vnew) {
    as.numeric(predict(fit,
      newdata = data.frame(x = qlogis(clamp(pnew)), v = vnew),
      type = "response"))
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ===========================================================================
# LOAD PRODUCT PROBABILITIES
# ===========================================================================

cli_h1("Step 6c: Walk-forward recalibration (volume-conditional bake-off)")

probs <- readr::read_csv("output/06b_fp_sim_probabilities.csv", show_col_types = FALSE) |>
  filter(!is.na(fantasy_points_ppr)) |>
  select(player_id, season, week, position, pred_vol,
         p_start = p_start_sim, p_boom = p_boom_sim, hit_start, hit_boom) |>
  mutate(stratum = stratum_of(position, pred_vol)) |>
  arrange(season, week)

cli_alert_success("{nrow(probs)} scored player-weeks | seasons {min(probs$season)}-{max(probs$season)}")
cli_alert_info("Burn-in: seasons < {EVAL_START_SEASON} | eval window: {EVAL_START_SEASON}+")

strat_counts <- probs |> count(position, stratum)
cli_h2("Ex-ante stratum counts (all seasons)")
print(strat_counts, n = Inf)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward weekly refits")

eval_weeks <- probs |>
  filter(season >= EVAL_START_SEASON) |>
  distinct(season, week) |>
  arrange(season, week)

cli_alert_info("{nrow(eval_weeks)} evaluation season-weeks x 2 positions x 2 thresholds x 5 methods")

recal_one <- function(df, p_col, hit_col) {
  # df: one position, sorted by (season, week), with stratum + pred_vol.
  # Returns platt/iso/strat_platt/strat_iso/platt_vol columns.
  n <- nrow(df)
  out <- list(platt = rep(NA_real_, n), iso = rep(NA_real_, n),
              strat_platt = rep(NA_real_, n), strat_iso = rep(NA_real_, n),
              platt_vol = rep(NA_real_, n))
  for (i in seq_len(nrow(eval_weeks))) {
    s <- eval_weeks$season[i]; w <- eval_weeks$week[i]
    idx_test  <- which(df$season == s & df$week == w)
    if (!length(idx_test)) next
    idx_train <- which(df$season < s | (df$season == s & df$week < w))
    p_tr <- df[[p_col]][idx_train]; h_tr <- df[[hit_col]][idx_train]
    s_tr <- df$stratum[idx_train];  v_tr <- df$pred_vol[idx_train]
    p_te <- df[[p_col]][idx_test]
    s_te <- df$stratum[idx_test];   v_te <- df$pred_vol[idx_test]

    f_platt <- fit_platt(p_tr, h_tr)
    out$platt[idx_test] <- f_platt(p_te)
    out$iso[idx_test]   <- fit_isotonic(p_tr, h_tr)(p_te)
    out$strat_platt[idx_test] <- fit_strat(p_tr, h_tr, s_tr, fit_platt)(p_te, s_te)
    out$strat_iso[idx_test]   <- fit_strat(p_tr, h_tr, s_tr, fit_isotonic)(p_te, s_te)
    f_pv <- fit_platt_vol(p_tr, h_tr, v_tr)
    out$platt_vol[idx_test] <-
      if (is.null(f_pv)) f_platt(p_te) else f_pv(p_te, v_te)
  }
  as_tibble(out)
}

CAND_METHODS <- c("platt", "iso", "strat_platt", "strat_iso", "platt_vol")

recal <- probs |>
  group_by(position) |>
  group_modify(~ {
    d <- .x |> arrange(season, week)
    bind_cols(
      d,
      recal_one(d, "p_start", "hit_start") |>
        rename_with(~ paste0("p_start_", .x)),
      recal_one(d, "p_boom", "hit_boom") |>
        rename_with(~ paste0("p_boom_", .x))
    )
  }) |>
  ungroup() |>
  filter(season >= EVAL_START_SEASON)

# Coherence: P(20+) <= P(15+) within each method
for (m in CAND_METHODS) {
  recal[[paste0("p_boom_", m)]] <-
    pmin(recal[[paste0("p_boom_", m)]], recal[[paste0("p_start_", m)]])
}

cli_alert_success("Recalibrated {nrow(recal)} eval-window rows")

# ===========================================================================
# EVALUATION: RAW VS POOLED VS VOLUME-CONDITIONAL
# ===========================================================================

cli_h1("Evaluation on {EVAL_START_SEASON}+ (out-of-time)")

col_for <- function(thresh, method) {
  stem <- if (thresh == "15+") "p_start" else "p_boom"
  if (method == "raw") stem else paste0(stem, "_", method)
}

calibrate <- function(df, prob_col, hit_col, method, thresh, pos, by_stratum = FALSE) {
  g <- df |>
    filter(position == pos, !is.na(.data[[prob_col]])) |>
    mutate(bin = cut(.data[[prob_col]], seq(0, 1, 0.1),
                     include.lowest = TRUE, right = FALSE))
  g <- if (by_stratum) group_by(g, stratum, bin) else group_by(g, bin)
  g |>
    summarise(n = n(), pred = mean(.data[[prob_col]]),
              emp = mean(.data[[hit_col]]), .groups = "drop") |>
    mutate(delta = emp - pred, method = method, threshold = thresh, position = pos)
}

grid <- expand_grid(
  pos    = c("RB", "WR"),
  thresh = c("15+", "20+"),
  method = c("raw", CAND_METHODS)
)

cal_all <- pmap(grid, function(pos, thresh, method) {
  calibrate(recal, col_for(thresh, method),
            if (thresh == "15+") "hit_start" else "hit_boom",
            method, thresh, pos)
}) |> list_rbind()

cal_strat <- pmap(grid, function(pos, thresh, method) {
  calibrate(recal, col_for(thresh, method),
            if (thresh == "15+") "hit_start" else "hit_boom",
            method, thresh, pos, by_stratum = TRUE)
}) |> list_rbind()

brier <- pmap(grid, function(pos, thresh, method) {
  d <- recal |> filter(position == pos)
  p <- d[[col_for(thresh, method)]]
  h <- as.numeric(d[[if (thresh == "15+") "hit_start" else "hit_boom"]])
  tibble(position = pos, threshold = thresh, method = method,
         brier = mean((p - h)^2, na.rm = TRUE))
}) |> list_rbind()

summary_tbl <- cal_all |>
  group_by(position, threshold, method) |>
  summarise(w_mean_abs_delta = weighted.mean(abs(delta), n), .groups = "drop") |>
  left_join(
    cal_strat |>
      group_by(position, threshold, method) |>
      summarise(strat_w_mean_abs_delta = weighted.mean(abs(delta), n),
                .groups = "drop"),
    by = c("position", "threshold", "method")
  ) |>
  left_join(brier, by = c("position", "threshold", "method"))

cli_h2("Judge metric (stratified) + pooled |delta| (pp) + Brier, by method")
print(summary_tbl |>
        mutate(pooled_pp = sprintf("%.2f", 100 * w_mean_abs_delta),
               strat_pp  = sprintf("%.2f", 100 * strat_w_mean_abs_delta),
               brier     = sprintf("%.5f", brier)) |>
        select(position, threshold, method, strat_pp, pooled_pp, brier) |>
        arrange(position, threshold, strat_pp), n = Inf)

# Pre-committed pick: lowest STRATIFIED weighted |delta| per
# position-threshold, subject to Brier not degrading vs raw
picks <- summary_tbl |>
  group_by(position, threshold) |>
  group_modify(~ {
    raw_brier <- .x$brier[.x$method == "raw"]
    cand <- .x |> filter(method != "raw", brier <= raw_brier + 1e-4)
    if (!nrow(cand)) return(tibble(pick = "raw", reason = "no method beat raw Brier"))
    best <- cand |> slice_min(strat_w_mean_abs_delta, n = 1, with_ties = FALSE)
    tibble(pick = best$method, reason = "lowest stratified weighted |delta|, Brier ok")
  }) |>
  ungroup()

cli_h2("Method picks (pre-committed rule)")
print(picks, n = Inf)

# Stratum-level honesty: mean stated vs empirical by ex-ante stratum,
# raw vs incumbent pooled iso vs picked method
cli_h2("Stratum-level honesty (mean stated vs empirical, pp delta)")
strata_tbl <- pmap(picks, function(position, threshold, pick, reason) {
  hit_col <- if (threshold == "15+") "hit_start" else "hit_boom"
  methods <- unique(c("raw", "iso", pick))
  map(methods, function(m) {
    recal |>
      filter(.data$position == .env$position, !is.na(.data[[col_for(threshold, m)]])) |>
      group_by(stratum) |>
      summarise(n = n(),
                pred = mean(.data[[col_for(threshold, m)]]),
                emp  = mean(.data[[hit_col]]), .groups = "drop") |>
      mutate(delta_pp = 100 * (emp - pred), method = m,
             threshold = threshold, position = position)
  }) |> list_rbind()
}) |> list_rbind()

print(strata_tbl |>
        mutate(across(c(pred, emp), ~ sprintf("%.3f", .x)),
               delta_pp = sprintf("%+.1f", delta_pp)) |>
        select(position, threshold, method, stratum, n, pred, emp, delta_pp),
      n = Inf)

# ===========================================================================
# DEPLOYMENT MAPS (winning method, refit on ALL data)
# ===========================================================================

cli_h1("Deployment maps (refit on all seasons)")

all_rows <- probs   # full history including burn-in

deploy_maps <- pmap(picks, function(position, threshold, pick, reason) {
  d <- all_rows |> filter(.data$position == .env$position)
  p <- d[[if (threshold == "15+") "p_start" else "p_boom"]]
  h <- d[[if (threshold == "15+") "hit_start" else "hit_boom"]]
  s <- d$stratum
  v <- d$pred_vol
  breaks <- STRATA_BREAKS[[position]]
  labs   <- STRATA_LABELS
  # Uniform signature: every deployed map is function(p, pred_vol)
  fn <- switch(pick,
    raw         = function(pnew, vnew) pnew,
    platt       = { f <- fit_platt(p, h);    function(pnew, vnew) f(pnew) },
    iso         = { f <- fit_isotonic(p, h); function(pnew, vnew) f(pnew) },
    strat_platt = { f <- fit_strat(p, h, s, fit_platt)
                    function(pnew, vnew)
                      f(pnew, cut(vnew, breaks, labels = labs)) },
    strat_iso   = { f <- fit_strat(p, h, s, fit_isotonic)
                    function(pnew, vnew)
                      f(pnew, cut(vnew, breaks, labels = labs)) },
    platt_vol   = { f <- fit_platt_vol(p, h, v) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew"))
                      function(pnew, vnew) f(pnew)
                    else f }
  )
  list(position = position, threshold = threshold, method = pick,
       strata_breaks = breaks, map = fn)
})
names(deploy_maps) <- paste(picks$position, picks$threshold, sep = "_")

saveRDS(deploy_maps, "data/fp_recal_maps.rds")

# Inspection grid: for conditional maps, evaluate at each stratum's median
# ex-ante volume so the CSV shows the map the product will actually apply
vol_refs <- all_rows |>
  group_by(position, stratum) |>
  summarise(pred_vol_ref = median(pred_vol), .groups = "drop")

map_grid <- map(deploy_maps, function(m) {
  refs <- vol_refs |> filter(position == m$position)
  pmap(refs, function(position, stratum, pred_vol_ref) {
    # p_recal computed BEFORE tibble(): inside the tibble data mask, scalar
    # columns already defined get recycled to the length of p_raw, so a
    # rep(pred_vol_ref, ...) there would silently blow up to 101 x 101
    p_seq   <- seq(0, 1, 0.01)
    p_recal <- m$map(p_seq, rep(pred_vol_ref, length(p_seq)))
    tibble(position = position, threshold = m$threshold, method = m$method,
           stratum = as.character(stratum), pred_vol_ref = pred_vol_ref,
           p_raw = p_seq, p_recal = p_recal)
  }) |> list_rbind()
}) |> list_rbind()

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")

readr::write_csv(recal,     "output/06c_recal_probabilities.csv")
readr::write_csv(cal_all,   "output/06c_recal_calibration.csv")
readr::write_csv(cal_strat, "output/06c_recal_calibration_strat.csv")
readr::write_csv(summary_tbl |>
                   mutate(across(c(w_mean_abs_delta, strat_w_mean_abs_delta),
                                 ~ sprintf("%.4f", 100 * .x), .names = "{.col}_pp"),
                          brier = sprintf("%.5f", brier)) |>
                   select(position, threshold, method,
                          strat_w_mean_abs_delta_pp, w_mean_abs_delta_pp, brier),
                 "output/06c_recal_summary.csv")
readr::write_csv(strata_tbl, "output/06c_recal_strata.csv")
readr::write_csv(map_grid,   "output/06c_recal_map_grid.csv")

cli_alert_success("output/06c_recal_probabilities.csv ({nrow(recal)} rows)")
cli_alert_success("output/06c_recal_calibration.csv")
cli_alert_success("output/06c_recal_calibration_strat.csv")
cli_alert_success("output/06c_recal_summary.csv")
cli_alert_success("output/06c_recal_strata.csv")
cli_alert_success("output/06c_recal_map_grid.csv")
cli_alert_success("data/fp_recal_maps.rds (deployment maps)")

cli_h1("Step 6c complete")
