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
# DESIGN:
#   - Input: output/06b_fp_sim_probabilities.csv (product config:
#     spline translation + 03a-v2 RB + 04c asymmetric WR)
#   - Four maps: position (RB/WR) x threshold (15+/20+)
#   - Two candidate methods, judged by a PRE-COMMITTED rule:
#       PRIMARY:  n-weighted mean |bin delta| on the eval window
#       SANITY:   Brier score must not degrade vs raw
#     * Platt: logistic regression of hit on logit(p) -- smooth, 2 params
#     * Isotonic: monotone step fit, linearly interpolated -- flexible
#   - Walk-forward: 2014-2015 are burn-in; from 2016 on, each season-week is
#     scored by maps fit on all rows strictly before it, refit weekly
#   - Coherence: recalibrated P(20+) capped at recalibrated P(15+) per row
#   - Deployment: winning-method maps refit on ALL data -> data/fp_recal_maps.rds
#     plus a probability grid CSV for inspection

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

EVAL_START_SEASON <- 2016L   # 2014-2015 burn-in
P_EPS             <- 1e-4

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)

# ===========================================================================
# RECALIBRATION FITTERS -- each returns a function p -> p_recal
# ===========================================================================

fit_platt <- function(p, hit) {
  df  <- tibble(x = qlogis(pmin(pmax(p, P_EPS), 1 - P_EPS)), y = hit)
  fit <- tryCatch(glm(y ~ x, family = binomial, data = df),
                  error = function(e) NULL, warning = function(w) {
                    suppressWarnings(glm(y ~ x, family = binomial, data = df))
                  })
  if (is.null(fit)) return(identity)
  function(pnew) {
    as.numeric(predict(fit,
      newdata = tibble(x = qlogis(pmin(pmax(pnew, P_EPS), 1 - P_EPS))),
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

`%||%` <- function(a, b) if (is.null(a)) b else a

# ===========================================================================
# LOAD PRODUCT PROBABILITIES
# ===========================================================================

cli_h1("Step 6c: Walk-forward recalibration")

probs <- readr::read_csv("output/06b_fp_sim_probabilities.csv", show_col_types = FALSE) |>
  filter(!is.na(fantasy_points_ppr)) |>
  select(player_id, season, week, position,
         p_start = p_start_sim, p_boom = p_boom_sim, hit_start, hit_boom) |>
  arrange(season, week)

cli_alert_success("{nrow(probs)} scored player-weeks | seasons {min(probs$season)}-{max(probs$season)}")
cli_alert_info("Burn-in: seasons < {EVAL_START_SEASON} | eval window: {EVAL_START_SEASON}+")

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward weekly refits")

eval_weeks <- probs |>
  filter(season >= EVAL_START_SEASON) |>
  distinct(season, week) |>
  arrange(season, week)

cli_alert_info("{nrow(eval_weeks)} evaluation season-weeks x 2 positions x 2 thresholds x 2 methods")

recal_one <- function(df, p_col, hit_col) {
  # df: one position, sorted by (season, week). Returns platt/iso columns.
  out_platt <- rep(NA_real_, nrow(df))
  out_iso   <- rep(NA_real_, nrow(df))
  for (i in seq_len(nrow(eval_weeks))) {
    s <- eval_weeks$season[i]; w <- eval_weeks$week[i]
    idx_test  <- which(df$season == s & df$week == w)
    if (!length(idx_test)) next
    idx_train <- which(df$season < s | (df$season == s & df$week < w))
    p_tr <- df[[p_col]][idx_train]; h_tr <- df[[hit_col]][idx_train]
    out_platt[idx_test] <- fit_platt(p_tr, h_tr)(df[[p_col]][idx_test])
    out_iso[idx_test]   <- fit_isotonic(p_tr, h_tr)(df[[p_col]][idx_test])
  }
  tibble(platt = out_platt, iso = out_iso)
}

recal <- probs |>
  group_by(position) |>
  group_modify(~ {
    d <- .x |> arrange(season, week)
    bind_cols(
      d,
      recal_one(d, "p_start", "hit_start") |> rename(p_start_platt = platt, p_start_iso = iso),
      recal_one(d, "p_boom",  "hit_boom")  |> rename(p_boom_platt  = platt, p_boom_iso  = iso)
    )
  }) |>
  ungroup() |>
  filter(season >= EVAL_START_SEASON)

# Coherence: P(20+) <= P(15+) within each method
recal <- recal |>
  mutate(
    p_boom_platt = pmin(p_boom_platt, p_start_platt),
    p_boom_iso   = pmin(p_boom_iso,   p_start_iso)
  )

cli_alert_success("Recalibrated {nrow(recal)} eval-window rows")

# ===========================================================================
# EVALUATION: RAW VS PLATT VS ISOTONIC
# ===========================================================================

cli_h1("Evaluation on {EVAL_START_SEASON}+ (out-of-time)")

calibrate <- function(df, prob_col, hit_col, method, thresh, pos) {
  df |>
    filter(position == pos, !is.na(.data[[prob_col]])) |>
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
  method = c("raw", "platt", "iso")
)

col_for <- function(thresh, method) {
  stem <- if (thresh == "15+") "p_start" else "p_boom"
  if (method == "raw") stem else paste0(stem, "_", method)
}

cal_all <- pmap(grid, function(pos, thresh, method) {
  calibrate(recal, col_for(thresh, method),
            if (thresh == "15+") "hit_start" else "hit_boom",
            method, thresh, pos)
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
  left_join(brier, by = c("position", "threshold", "method")) |>
  mutate(w_mean_abs_delta_pp = sprintf("%.2f", 100 * w_mean_abs_delta),
         brier = sprintf("%.5f", brier))

cli_h2("n-weighted mean |delta| (pp) and Brier, by method")
print(summary_tbl |>
        select(position, threshold, method, w_mean_abs_delta_pp, brier) |>
        arrange(position, threshold, method), n = Inf)

# Pre-committed pick: lowest weighted |delta| per position-threshold,
# subject to Brier not degrading vs raw
picks <- summary_tbl |>
  group_by(position, threshold) |>
  group_modify(~ {
    raw_brier <- .x$brier[.x$method == "raw"]
    cand <- .x |> filter(method != "raw", brier <= as.numeric(raw_brier) + 1e-4)
    if (!nrow(cand)) return(tibble(pick = "raw", reason = "no method beat raw Brier"))
    best <- cand |> slice_min(w_mean_abs_delta, n = 1)
    tibble(pick = best$method, reason = "lowest weighted |delta|, Brier ok")
  }) |>
  ungroup()

cli_h2("Method picks (pre-committed rule)")
print(picks, n = Inf)

# Bin detail for the picked methods vs raw
cli_h2("Bin-level detail: raw vs picked method")
for (i in seq_len(nrow(picks))) {
  pos <- picks$position[i]; thresh <- picks$threshold[i]; pk <- picks$pick[i]
  cmp <- cal_all |>
    filter(position == pos, threshold == thresh, method %in% c("raw", pk)) |>
    select(method, bin, n, delta) |>
    pivot_wider(names_from = method, values_from = c(n, delta)) |>
    mutate(across(starts_with("delta"), ~ sprintf("%+.1f", 100 * .x)))
  cli_h3("{pos} P({thresh}) -- picked: {pk}")
  print(cmp, n = Inf)
}

# ===========================================================================
# DEPLOYMENT MAPS (winning method, refit on ALL data)
# ===========================================================================

cli_h1("Deployment maps (refit on all seasons)")

all_rows <- probs   # full history including burn-in

deploy_maps <- pmap(picks, function(position, threshold, pick, reason) {
  d <- all_rows |> filter(.data$position == .env$position)
  p <- d[[if (threshold == "15+") "p_start" else "p_boom"]]
  h <- d[[if (threshold == "15+") "hit_start" else "hit_boom"]]
  fn <- switch(pick,
    raw   = identity,
    platt = fit_platt(p, h),
    iso   = fit_isotonic(p, h)
  )
  list(position = position, threshold = threshold, method = pick, map = fn)
})
names(deploy_maps) <- paste(picks$position, picks$threshold, sep = "_")

saveRDS(deploy_maps, "data/fp_recal_maps.rds")

map_grid <- map(deploy_maps, function(m) {
  tibble(position = m$position, threshold = m$threshold, method = m$method,
         p_raw = seq(0, 1, 0.01), p_recal = m$map(seq(0, 1, 0.01)))
}) |> list_rbind()

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")

readr::write_csv(recal,       "output/06c_recal_probabilities.csv")
readr::write_csv(cal_all,     "output/06c_recal_calibration.csv")
readr::write_csv(summary_tbl |> select(-w_mean_abs_delta), "output/06c_recal_summary.csv")
readr::write_csv(map_grid,    "output/06c_recal_map_grid.csv")

cli_alert_success("output/06c_recal_probabilities.csv ({nrow(recal)} rows)")
cli_alert_success("output/06c_recal_calibration.csv")
cli_alert_success("output/06c_recal_summary.csv")
cli_alert_success("output/06c_recal_map_grid.csv")
cli_alert_success("data/fp_recal_maps.rds (deployment maps)")

cli_h1("Step 6c complete")
