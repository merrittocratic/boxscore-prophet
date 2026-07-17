# R/06f_rb3c_recalibration.R
# Step 6f: Walk-forward recalibration of the RB 3C upside chain + the
# pre-committed two-product veto.
#
# Same conditional bake-off as 6c/9b (raw | platt | iso | strat_platt |
# strat_iso | platt_vol; stratified judge; Brier sanity; weekly walk-forward
# refits, 2014-15 burn-in). Strata on 3C pred_vol at the roster cuts locked
# in 06e: low <10 / mid 10-14 / high >=14.
#
# VETO (pre-committed in 06e, graded here): the 3C chain ships for the RB
# streamer roster (3C pred_vol < 10) only if BOTH
#   (a) recalibrated probabilities honest on the roster: |mean delta| <= 2pp
#       at 15+ and 20+, AND
#   (b) roster Brier not worse than the SHIPPED 3A-v2 chain (6c strat_iso
#       output) on the same player-weeks.
# Fail -> single-engine RB (3A-v2 powers both rosters, as WR does) and 3C
# stays retired for deployment.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

EVAL_START_SEASON <- 2016L
P_EPS             <- 1e-4
MIN_STRAT_N       <- 300L

STRATA_BREAKS <- c(-Inf, 10, 14, Inf)
STRATA_LABELS <- c("exante_low", "exante_mid", "exante_high")

stratum_of <- function(pred_vol) {
  cut(pred_vol, STRATA_BREAKS, labels = STRATA_LABELS, right = FALSE)
}

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)

# ===========================================================================
# RECALIBRATION FITTERS -- identical to 6c/9b (locally-bound closures,
# base-R newdata; deployment maps must load in fresh sessions)
# ===========================================================================

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
  fun <- tryCatch(
    approxfun(sort(iso$x), iso$yf, method = "linear", rule = 2, ties = mean),
    error = function(e) NULL
  )
  if (is.null(fun)) return(identity)
  function(pnew) pmin(pmax(fun(pnew), 0), 1)
}

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
# LOAD 3C CHAIN PROBABILITIES
# ===========================================================================

cli_h1("Step 6f: RB 3C walk-forward recalibration + two-product veto")

probs <- readr::read_csv("output/06e_rb3c_fp_sim_probabilities.csv",
                         show_col_types = FALSE) |>
  filter(!is.na(fantasy_points_ppr)) |>
  select(player_id, season, week, pred_vol,
         p_start = p_start_sim, p_boom = p_boom_sim, hit_start, hit_boom) |>
  mutate(stratum = stratum_of(pred_vol)) |>
  arrange(season, week)

cli_alert_success("{nrow(probs)} scored RB-weeks | seasons {min(probs$season)}-{max(probs$season)}")
print(probs |> count(stratum), n = Inf)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward weekly refits")

eval_weeks <- probs |>
  filter(season >= EVAL_START_SEASON) |>
  distinct(season, week) |>
  arrange(season, week)

recal_one <- function(df, p_col, hit_col) {
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

d_sorted <- probs |> arrange(season, week)
recal <- bind_cols(
  d_sorted,
  recal_one(d_sorted, "p_start", "hit_start") |>
    rename_with(~ paste0("p_start_", .x)),
  recal_one(d_sorted, "p_boom", "hit_boom") |>
    rename_with(~ paste0("p_boom_", .x))
) |>
  filter(season >= EVAL_START_SEASON)

for (m in CAND_METHODS) {
  recal[[paste0("p_boom_", m)]] <-
    pmin(recal[[paste0("p_boom_", m)]], recal[[paste0("p_start_", m)]])
}

cli_alert_success("Recalibrated {nrow(recal)} eval-window rows")

# ===========================================================================
# EVALUATION AND PICKS
# ===========================================================================

cli_h1("Evaluation on {EVAL_START_SEASON}+ (out-of-time)")

col_for <- function(thresh, method) {
  stem <- if (thresh == "15+") "p_start" else "p_boom"
  if (method == "raw") stem else paste0(stem, "_", method)
}

calibrate <- function(df, prob_col, hit_col, method, thresh, by_stratum = FALSE) {
  g <- df |>
    filter(!is.na(.data[[prob_col]])) |>
    mutate(bin = cut(.data[[prob_col]], seq(0, 1, 0.1),
                     include.lowest = TRUE, right = FALSE))
  g <- if (by_stratum) group_by(g, stratum, bin) else group_by(g, bin)
  g |>
    summarise(n = n(), pred = mean(.data[[prob_col]]),
              emp = mean(.data[[hit_col]]), .groups = "drop") |>
    mutate(delta = emp - pred, method = method, threshold = thresh,
           position = "RB_3C")
}

grid <- expand_grid(thresh = c("15+", "20+"), method = c("raw", CAND_METHODS))

cal_all <- pmap(grid, function(thresh, method) {
  calibrate(recal, col_for(thresh, method),
            if (thresh == "15+") "hit_start" else "hit_boom", method, thresh)
}) |> list_rbind()

cal_strat <- pmap(grid, function(thresh, method) {
  calibrate(recal, col_for(thresh, method),
            if (thresh == "15+") "hit_start" else "hit_boom", method, thresh,
            by_stratum = TRUE)
}) |> list_rbind()

brier <- pmap(grid, function(thresh, method) {
  p <- recal[[col_for(thresh, method)]]
  h <- as.numeric(recal[[if (thresh == "15+") "hit_start" else "hit_boom"]])
  tibble(position = "RB_3C", threshold = thresh, method = method,
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
        select(threshold, method, strat_pp, pooled_pp, brier) |>
        arrange(threshold, strat_pp), n = Inf)

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

# ===========================================================================
# TWO-PRODUCT VETO: 3C CHAIN VS SHIPPED 3A-V2 CHAIN ON THE STREAMER ROSTER
# ===========================================================================

cli_h1("Two-product veto: streamer roster (3C pred_vol < 10), 2016+")

# Shipped incumbent: 6c strat_iso recalibrated probabilities (3A-v2 chain,
# post re-cut RB strata) -- the strat_iso columns, i.e. the deployed pick
incumbent <- readr::read_csv("output/06c_recal_probabilities.csv",
                             show_col_types = FALSE) |>
  filter(position == "RB") |>
  select(player_id, season, week,
         inc_p_start = p_start_strat_iso, inc_p_boom = p_boom_strat_iso)

roster <- recal |>
  filter(stratum == "exante_low") |>
  inner_join(incumbent, by = c("player_id", "season", "week"))

cli_alert_info("Streamer roster rows (joined to incumbent): {nrow(roster)}")

veto_row <- function(thresh) {
  pick <- picks$pick[picks$threshold == thresh]
  pcol <- col_for(thresh, pick)
  icol <- if (thresh == "15+") "inc_p_start" else "inc_p_boom"
  hcol <- if (thresh == "15+") "hit_start" else "hit_boom"
  p3c  <- roster[[pcol]]; pinc <- roster[[icol]]
  h    <- as.numeric(roster[[hcol]])
  tibble(
    threshold    = thresh,
    pick_3c      = pick,
    delta_3c_pp  = 100 * (mean(h) - mean(p3c)),
    delta_inc_pp = 100 * (mean(h) - mean(pinc)),
    brier_3c     = mean((p3c - h)^2),
    brier_inc    = mean((pinc - h)^2),
    honest_ok    = abs(mean(h) - mean(p3c)) <= 0.02,
    brier_ok     = mean((p3c - h)^2) <= mean((pinc - h)^2) + 1e-5
  )
}

veto_tbl <- bind_rows(veto_row("15+"), veto_row("20+")) |>
  mutate(pass = honest_ok & brier_ok)

cli_h2("Veto table (honest_ok: |delta| <= 2pp; brier_ok: <= incumbent)")
print(veto_tbl |>
        mutate(across(c(delta_3c_pp, delta_inc_pp), ~ sprintf("%+.2f", .x)),
              across(c(brier_3c, brier_inc), ~ sprintf("%.5f", .x))), n = Inf)

verdict <- if (all(veto_tbl$pass)) "VETO PASSED" else "VETO FAILED"
cli_h1("{verdict}: 3C chain {if (all(veto_tbl$pass)) 'ships for' else 'does NOT ship for'} the RB streamer roster")

# ===========================================================================
# DEPLOYMENT MAPS + SAVE (maps saved regardless; ship decision is Steve's,
# informed by the veto verdict above)
# ===========================================================================

cli_h1("Deployment maps (refit on all seasons)")

all_rows <- probs

deploy_maps <- pmap(picks, function(position, threshold, pick, reason) {
  p <- all_rows[[if (threshold == "15+") "p_start" else "p_boom"]]
  h <- all_rows[[if (threshold == "15+") "hit_start" else "hit_boom"]]
  s <- all_rows$stratum
  v <- all_rows$pred_vol
  breaks <- STRATA_BREAKS
  labs   <- STRATA_LABELS
  fn <- switch(pick,
    raw         = function(pnew, vnew) pnew,
    platt       = { f <- fit_platt(p, h);    function(pnew, vnew) f(pnew) },
    iso         = { f <- fit_isotonic(p, h); function(pnew, vnew) f(pnew) },
    strat_platt = { f <- fit_strat(p, h, s, fit_platt)
                    function(pnew, vnew)
                      f(pnew, cut(vnew, breaks, labels = labs, right = FALSE)) },
    strat_iso   = { f <- fit_strat(p, h, s, fit_isotonic)
                    function(pnew, vnew)
                      f(pnew, cut(vnew, breaks, labels = labs, right = FALSE)) },
    platt_vol   = { f <- fit_platt_vol(p, h, v) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew"))
                      function(pnew, vnew) f(pnew)
                    else f }
  )
  list(position = "RB_3C", threshold = threshold, method = pick,
       strata_breaks = breaks, map = fn)
})
names(deploy_maps) <- paste("RB3C", picks$threshold, sep = "_")

saveRDS(deploy_maps, "data/rb3c_fp_recal_maps.rds")

readr::write_csv(recal,     "output/06f_rb3c_recal_probabilities.csv")
readr::write_csv(cal_all,   "output/06f_rb3c_recal_calibration.csv")
readr::write_csv(cal_strat, "output/06f_rb3c_recal_calibration_strat.csv")
readr::write_csv(summary_tbl |>
                   mutate(across(c(w_mean_abs_delta, strat_w_mean_abs_delta),
                                 ~ sprintf("%.4f", 100 * .x), .names = "{.col}_pp"),
                          brier = sprintf("%.5f", brier)) |>
                   select(position, threshold, method,
                          strat_w_mean_abs_delta_pp, w_mean_abs_delta_pp, brier),
                 "output/06f_rb3c_recal_summary.csv")
readr::write_csv(veto_tbl,  "output/06f_rb3c_veto.csv")

cli_alert_success("output/06f_rb3c_recal_probabilities.csv ({nrow(recal)} rows)")
cli_alert_success("output/06f_rb3c_recal_summary.csv")
cli_alert_success("output/06f_rb3c_veto.csv")
cli_alert_success("data/rb3c_fp_recal_maps.rds")

cli_h1("Step 6f complete")
