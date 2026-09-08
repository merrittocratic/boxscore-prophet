# R/21e_fp_recalibration.R
# Stage C, step 2 of the D29 single-stage rebuild: walk-forward
# recalibration of P(FP >= 15), P(FP >= 20), and P(FP < BUST_THRESH) --
# p_bust folded in here per Steve's 2026-09-07 decision (probability has
# to mean something verified, not just rank correctly; SHAP/uncertainty
# framing is additive to that, not a replacement for it).
#
# This is a clone of R/06c_recalibration.R -- all 7 fitters (platt, iso,
# strat_platt, strat_iso, platt_vol, platt_vegas, platt_vol_vegas) plus
# R/18e's star_platt fitter port UNCHANGED (probability-space, model-
# architecture-agnostic machinery). What's different from 06c:
#   - INPUT: R/21d's single-stage fold predictions (pred_fp/pred_vol/
#     p_start_raw/p_boom_raw/q02..q98), not 06b's two-stage sim output.
#     Which arm's predictions to recalibrate is an env seam (RECAL_ARM,
#     default "base") -- this script is meant to be built once and
#     pointed at whichever arm eventually ships, not re-derived per arm.
#   - THIRD THRESHOLD, p_bust: derived from the K=11 signed-conformal
#     quantile grid R/21d already saves per row (q02..q98) via the exact
#     same CDF-inversion logic as p_at_least() -- no rerun of R/21d
#     needed, no new modeling, just reading a column that already exists.
#     BUST_THRESH = 5 FP for both positions (Steve's own example
#     threshold) -- a product decision, easy to change, not a modeling
#     one.
#   - NO COHERENCE CAP between p_bust and p_start/p_boom: p_boom<=p_start
#     is capped because those are NESTED thresholds (boom implies start,
#     so boom's probability can never legitimately exceed start's) --
#     that relationship doesn't exist between bust (a low-tail event) and
#     start/boom (high-tail events). They're read off the same monotonic
#     quantile grid, so p_bust and p_start can never overlap or invert by
#     construction; no extra cap is needed or applied.
#   - STRATA_BREAKS/MIN_STRAT_N/EVAL_START_SEASON preserved verbatim per
#     the plan's explicit instruction, even though the single-stage
#     training tables have different row counts than the old two-stage
#     slate universe -- a disclosed, not silently "fixed", carry-over.
#
# Usage: Rscript R/21e_fp_recalibration.R
#   Env RECAL_ARM: which R/21d arm's fold_predictions.csv to recalibrate
#     (default "base")

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})
source("R/18e_star_bucket_fns.R")

EVAL_START_SEASON <- 2016L
P_EPS              <- 1e-4
MIN_STRAT_N        <- 300L
BUST_THRESH        <- c(RB = 5, WR = 5)
QLEVELS            <- c(0.02, 0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.80, 0.90, 0.95, 0.98)
QCOLS              <- paste0("q", gsub("0\\.", "", sprintf("%.2f", QLEVELS)))

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

`%||%` <- function(a, b) if (is.null(a)) b else a

# ===========================================================================
# RECALIBRATION FITTERS -- ported UNCHANGED from R/06c_recalibration.R.
# Probability-space maps that only ever see (p, hit, [covariates]) --
# nothing here knows or cares that the model behind p is now single-stage.
# ===========================================================================

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

fit_platt_vegas <- function(p, hit, sp, it, it_center) {
  clamp <- local({ eps <- P_EPS; function(q) pmin(pmax(q, eps), 1 - eps) })
  ctr   <- it_center
  df  <- tibble(x = qlogis(clamp(p)),
                sp_c = coalesce(sp, 0),
                asp_c = abs(coalesce(sp, 0)),
                it_c = coalesce(it, ctr) - ctr,
                y = hit)
  fit <- tryCatch(glm(y ~ x + sp_c + asp_c + it_c, family = binomial, data = df),
                  error = function(e) NULL, warning = function(w) {
                    suppressWarnings(glm(y ~ x + sp_c + asp_c + it_c, family = binomial, data = df))
                  })
  if (is.null(fit)) return(NULL)
  function(pnew, spnew, itnew) {
    as.numeric(predict(fit, newdata = data.frame(
      x = qlogis(clamp(pnew)),
      sp_c = coalesce(spnew, 0),
      asp_c = abs(coalesce(spnew, 0)),
      it_c = coalesce(itnew, ctr) - ctr), type = "response"))
  }
}

fit_platt_vol_vegas <- function(p, hit, vol, sp, it, it_center) {
  clamp <- local({ eps <- P_EPS; function(q) pmin(pmax(q, eps), 1 - eps) })
  ctr   <- it_center
  df  <- tibble(x = qlogis(clamp(p)), v = vol,
                sp_c = coalesce(sp, 0),
                asp_c = abs(coalesce(sp, 0)),
                it_c = coalesce(it, ctr) - ctr,
                y = hit)
  fit <- tryCatch(glm(y ~ x + v + sp_c + asp_c + it_c, family = binomial, data = df),
                  error = function(e) NULL, warning = function(w) {
                    suppressWarnings(glm(y ~ x + v + sp_c + asp_c + it_c, family = binomial, data = df))
                  })
  if (is.null(fit)) return(NULL)
  function(pnew, vnew, spnew, itnew) {
    as.numeric(predict(fit, newdata = data.frame(
      x = qlogis(clamp(pnew)), v = vnew,
      sp_c = coalesce(spnew, 0),
      asp_c = abs(coalesce(spnew, 0)),
      it_c = coalesce(itnew, ctr) - ctr), type = "response"))
  }
}

# 8th fitter, ported from R/18e_star_bucket_fns.R (fit_star_platt_map,
# star_platt_closure) -- conditions on trailing REALIZED performance tier
# (b1 = last-17-game top-12 by FP/game, b2 = 13-24, b3 = rest/no history),
# not an ex-ante covariate. star_trailing_fp()/star_assign_buckets() are
# already leakage-safe by construction (trailing FP only looks at
# strictly-prior games), so bucket assignment happens ONCE outside the
# walk-forward loop, same as 18e's own usage pattern.
CAND_METHODS <- c("platt", "iso", "strat_platt", "strat_iso", "platt_vol",
                  "platt_vegas", "platt_vol_vegas", "star_platt")
NEEDS_BUCKET <- c(star_platt = TRUE)

# ===========================================================================
# LOAD SINGLE-STAGE PREDICTIONS (R/21d output)
# ===========================================================================

cli_h1("Step 21e: FP recalibration (single-stage input, p_bust folded in)")

RECAL_ARM <- Sys.getenv("RECAL_ARM", "base")
cli_alert_info("Recalibrating arm: {RECAL_ARM}")

THRESH_START <- c(RB = 15, WR = 15)
THRESH_BOOM  <- c(RB = 20, WR = 20)

# p_at_least(): identical CDF-inversion logic to R/21d's own function --
# copied here (self-contained-script convention) rather than sourced,
# since R/21d is a backtest driver, not a shared-function file. Returns
# P(FP >= t) per row from that row's saved K=11 quantile grid.
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

load_position <- function(position) {
  path <- sprintf("output/21d_%s_%s_fold_predictions.csv", tolower(position), RECAL_ARM)
  ft   <- read_csv(path, show_col_types = FALSE)
  cli_alert_info("{position}: {nrow(ft)} rows from {path}")

  Qmat <- as.matrix(ft[, QCOLS])
  p_bust <- 1 - p_at_least(Qmat, QLEVELS, BUST_THRESH[[position]])

  ft |>
    mutate(
      player_id = as.character(player_id),
      position  = position,
      p_start   = p_start_raw,
      p_boom    = p_boom_raw,
      p_bust    = p_bust,
      hit_start = as.integer(fantasy_points_ppr >= THRESH_START[[.env$position]]),
      hit_boom  = as.integer(fantasy_points_ppr >= THRESH_BOOM[[.env$position]]),
      hit_bust  = as.integer(fantasy_points_ppr <  BUST_THRESH[[.env$position]])
    ) |>
    select(player_id, season, week, position, pred_vol,
           p_start, p_boom, p_bust, hit_start, hit_boom, hit_bust)
}

probs <- bind_rows(load_position("RB"), load_position("WR")) |>
  mutate(stratum = stratum_of(position, pred_vol)) |>
  arrange(season, week)

# Star buckets: trailing realized FP rank, computed ONCE per position
# (rank is within-position, within-week -- the universe 18e expects).
SEASONS <- sort(unique(probs$season))
trailing <- star_trailing_fp(SEASONS)
probs <- probs |>
  group_by(position) |>
  group_modify(~ star_assign_buckets(.x, trailing)) |>
  ungroup()
cli_alert_info("Star buckets assigned: {probs |> count(position, star_bucket) |> nrow()} position x bucket cells")

# Opener Vegas covariates -- same join pattern as R/06c.
vegas_open <- readRDS("data/vegas_open_lines.rds")
IT_CENTER  <- median(vegas_open$implied_total, na.rm = TRUE)
vkeys <- bind_rows(
  readRDS("data/rb_feature_table.rds") |> filter(!is.na(player_id)) |>
    distinct(player_id, season, week, game_id, posteam) |> mutate(position = "RB"),
  readRDS("data/wr_feature_table.rds") |> filter(!is.na(player_id)) |>
    distinct(player_id, season, week, game_id, posteam) |> mutate(position = "WR")
) |> mutate(player_id = as.character(player_id))
probs <- probs |>
  left_join(vkeys, by = c("position", "player_id", "season", "week")) |>
  left_join(vegas_open, by = c("game_id", "posteam")) |>
  select(-game_id, -posteam)
cli_alert_info("Opener covariates: {round(100 * mean(!is.na(probs$team_spread)), 1)}% coverage | it center {round(IT_CENTER, 2)}")

spread_bucket <- function(s) cut(s, c(-Inf, -6.5, -2.5, 2.5, 6.5, Inf),
  labels = c("big_dog", "dog", "close", "fav", "big_fav"))
itotal_bucket <- function(it) cut(it, c(-Inf, 20, 26, Inf),
  labels = c("low_implied", "mid_implied", "high_implied"))
probs <- probs |> mutate(sb = spread_bucket(team_spread), ib = itotal_bucket(implied_total))

cli_alert_success("{nrow(probs)} scored player-weeks | seasons {min(probs$season)}-{max(probs$season)}")
cli_alert_info("Burn-in: seasons < {EVAL_START_SEASON} | eval window: {EVAL_START_SEASON}+")

strat_counts <- probs |> count(position, stratum)
cli_h2("Ex-ante stratum counts (all seasons)")
print(strat_counts, n = Inf)

# ===========================================================================
# WALK-FORWARD LOOP -- three thresholds (start/boom/bust), 8 methods each
# ===========================================================================

cli_h1("Walk-forward weekly refits")

eval_weeks <- probs |> filter(season >= EVAL_START_SEASON) |>
  distinct(season, week) |> arrange(season, week)
cli_alert_info("{nrow(eval_weeks)} evaluation season-weeks x 2 positions x 3 thresholds x {length(CAND_METHODS)} methods")

recal_one <- function(df, p_col, hit_col) {
  n <- nrow(df)
  out <- map(set_names(CAND_METHODS), ~ rep(NA_real_, n))
  for (i in seq_len(nrow(eval_weeks))) {
    s <- eval_weeks$season[i]; w <- eval_weeks$week[i]
    idx_test  <- which(df$season == s & df$week == w)
    if (!length(idx_test)) next
    idx_train <- which(df$season < s | (df$season == s & df$week < w))
    p_tr <- df[[p_col]][idx_train]; h_tr <- df[[hit_col]][idx_train]
    s_tr <- df$stratum[idx_train];  v_tr <- df$pred_vol[idx_train]
    sp_tr <- df$team_spread[idx_train]; it_tr <- df$implied_total[idx_train]
    b_tr  <- df$star_bucket[idx_train]
    p_te <- df[[p_col]][idx_test]
    s_te <- df$stratum[idx_test];   v_te <- df$pred_vol[idx_test]
    sp_te <- df$team_spread[idx_test]; it_te <- df$implied_total[idx_test]
    b_te  <- df$star_bucket[idx_test]

    f_platt <- fit_platt(p_tr, h_tr)
    out$platt[idx_test] <- f_platt(p_te)
    out$iso[idx_test]   <- fit_isotonic(p_tr, h_tr)(p_te)
    out$strat_platt[idx_test] <- fit_strat(p_tr, h_tr, s_tr, fit_platt)(p_te, s_te)
    out$strat_iso[idx_test]   <- fit_strat(p_tr, h_tr, s_tr, fit_isotonic)(p_te, s_te)
    f_pv <- fit_platt_vol(p_tr, h_tr, v_tr)
    out$platt_vol[idx_test] <- if (is.null(f_pv)) f_platt(p_te) else f_pv(p_te, v_te)
    f_pg <- fit_platt_vegas(p_tr, h_tr, sp_tr, it_tr, IT_CENTER)
    out$platt_vegas[idx_test] <- if (is.null(f_pg)) f_platt(p_te) else f_pg(p_te, sp_te, it_te)
    f_pvg <- fit_platt_vol_vegas(p_tr, h_tr, v_tr, sp_tr, it_tr, IT_CENTER)
    out$platt_vol_vegas[idx_test] <- if (is.null(f_pvg)) f_platt(p_te) else f_pvg(p_te, v_te, sp_te, it_te)
    sp_fit <- tryCatch(fit_star_platt_map(p_tr, h_tr, b_tr), error = function(e) NULL)
    out$star_platt[idx_test] <- if (is.null(sp_fit)) f_platt(p_te) else
      sp_fit$map(p_te, v_te, sp_te, it_te, b_te)
  }
  as_tibble(out)
}

recal <- probs |>
  group_by(position) |>
  group_modify(~ {
    d <- .x |> arrange(season, week)
    bind_cols(
      d,
      recal_one(d, "p_start", "hit_start") |> rename_with(~ paste0("p_start_", .x)),
      recal_one(d, "p_boom",  "hit_boom")  |> rename_with(~ paste0("p_boom_", .x)),
      recal_one(d, "p_bust",  "hit_bust")  |> rename_with(~ paste0("p_bust_", .x))
    )
  }) |>
  ungroup() |>
  filter(season >= EVAL_START_SEASON)

# Coherence: P(20+) <= P(15+) within each method (NESTED thresholds only --
# p_bust is NOT capped against these, see header note on why).
for (m in CAND_METHODS) {
  recal[[paste0("p_boom_", m)]] <- pmin(recal[[paste0("p_boom_", m)]], recal[[paste0("p_start_", m)]])
}

cli_alert_success("Recalibrated {nrow(recal)} eval-window rows")

# ===========================================================================
# EVALUATION
# ===========================================================================

cli_h1("Evaluation on {EVAL_START_SEASON}+ (out-of-time)")

col_for <- function(thresh, method) {
  stem <- switch(thresh, "15+" = "p_start", "20+" = "p_boom", "bust" = "p_bust")
  if (method == "raw") stem else paste0(stem, "_", method)
}
hit_for <- function(thresh) switch(thresh, "15+" = "hit_start", "20+" = "hit_boom", "bust" = "hit_bust")

calibrate <- function(df, prob_col, hit_col, method, thresh, pos, group_col = NULL) {
  g <- df |> filter(position == pos, !is.na(.data[[prob_col]])) |>
    mutate(bin = cut(.data[[prob_col]], seq(0, 1, 0.1), include.lowest = TRUE, right = FALSE))
  if (!is.null(group_col)) g <- g |> filter(!is.na(.data[[group_col]]))
  g <- if (!is.null(group_col)) group_by(g, cell = .data[[group_col]], bin) else group_by(g, bin)
  g |> summarise(n = n(), pred = mean(.data[[prob_col]]), emp = mean(.data[[hit_col]]), .groups = "drop") |>
    mutate(delta = emp - pred, method = method, threshold = thresh, position = pos)
}

grid <- expand_grid(pos = c("RB", "WR"), thresh = c("15+", "20+", "bust"), method = c("raw", CAND_METHODS))

cal_all <- pmap(grid, function(pos, thresh, method)
  calibrate(recal, col_for(thresh, method), hit_for(thresh), method, thresh, pos)) |> list_rbind()

cal_by <- function(group_col) {
  pmap(grid, function(pos, thresh, method)
    calibrate(recal, col_for(thresh, method), hit_for(thresh), method, thresh, pos, group_col = group_col)) |> list_rbind()
}
cal_strat <- cal_by("stratum")
cal_sb    <- cal_by("sb")
cal_ib    <- cal_by("ib")
ext_cells <- bind_rows(cal_strat |> mutate(axis = "volume"), cal_sb |> mutate(axis = "spread"), cal_ib |> mutate(axis = "implied"))

brier <- pmap(grid, function(pos, thresh, method) {
  d <- recal |> filter(position == pos)
  p <- d[[col_for(thresh, method)]]
  h <- as.numeric(d[[hit_for(thresh)]])
  tibble(position = pos, threshold = thresh, method = method, brier = mean((p - h)^2, na.rm = TRUE))
}) |> list_rbind()

summary_tbl <- cal_all |>
  group_by(position, threshold, method) |>
  summarise(w_mean_abs_delta = weighted.mean(abs(delta), n), .groups = "drop") |>
  left_join(
    ext_cells |> group_by(position, threshold, method) |>
      summarise(ext_w_mean_abs_delta = weighted.mean(abs(delta), n), .groups = "drop"),
    by = c("position", "threshold", "method")) |>
  left_join(brier, by = c("position", "threshold", "method"))

cli_h2("Judge metric (vol + spread + implied cells) + pooled |delta| (pp) + Brier")
print(summary_tbl |>
        mutate(pooled_pp = sprintf("%.2f", 100 * w_mean_abs_delta),
               ext_pp    = sprintf("%.2f", 100 * ext_w_mean_abs_delta),
               brier     = sprintf("%.5f", brier)) |>
        select(position, threshold, method, ext_pp, pooled_pp, brier) |>
        arrange(position, threshold, ext_pp), n = Inf)

picks <- summary_tbl |>
  group_by(position, threshold) |>
  group_modify(~ {
    raw_brier <- .x$brier[.x$method == "raw"]
    cand <- .x |> filter(method != "raw", brier <= raw_brier + 1e-4)
    if (!nrow(cand)) return(tibble(pick = "raw", reason = "no method beat raw Brier"))
    best <- cand |> slice_min(ext_w_mean_abs_delta, n = 1, with_ties = FALSE)
    tibble(pick = best$method, reason = "lowest extended weighted |delta|, Brier ok")
  }) |> ungroup()

cli_h2("Method picks (pre-committed rule, same as R/06c)")
print(picks, n = Inf)

# ===========================================================================
# DEPLOYMENT MAPS (winning method, refit on ALL data)
# ===========================================================================

cli_h1("Deployment maps (refit on all seasons)")

all_rows <- probs

deploy_maps <- pmap(picks, function(position, threshold, pick, reason) {
  d <- all_rows |> filter(.data$position == .env$position)
  p_col <- switch(threshold, "15+" = "p_start", "20+" = "p_boom", "bust" = "p_bust")
  h_col <- hit_for(threshold)
  p <- d[[p_col]]; h <- d[[h_col]]
  s <- d$stratum; v <- d$pred_vol; sp <- d$team_spread; it <- d$implied_total; bkt <- d$star_bucket
  breaks <- STRATA_BREAKS[[position]]; labs <- STRATA_LABELS
  fn <- switch(pick,
    raw         = function(pnew, vnew, spnew, itnew, bnew = NULL) pnew,
    platt       = { f <- fit_platt(p, h); function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew) },
    iso         = { f <- fit_isotonic(p, h); function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew) },
    strat_platt = { f <- fit_strat(p, h, s, fit_platt)
                    function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew, cut(vnew, breaks, labels = labs)) },
    strat_iso   = { f <- fit_strat(p, h, s, fit_isotonic)
                    function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew, cut(vnew, breaks, labels = labs)) },
    platt_vol   = { f <- fit_platt_vol(p, h, v) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew")) function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew)
                    else function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew, vnew) },
    platt_vegas = { f <- fit_platt_vegas(p, h, sp, it, IT_CENTER) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew")) function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew)
                    else function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew, spnew, itnew) },
    platt_vol_vegas = { f <- fit_platt_vol_vegas(p, h, v, sp, it, IT_CENTER) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew")) function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew)
                    else function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew, vnew, spnew, itnew) },
    star_platt  = { sp_fit <- tryCatch(fit_star_platt_map(p, h, bkt), error = function(e) NULL)
                    if (is.null(sp_fit)) { f <- fit_platt(p, h); function(pnew, vnew, spnew, itnew, bnew = NULL) f(pnew) }
                    else function(pnew, vnew, spnew, itnew, bnew) sp_fit$map(pnew, vnew, spnew, itnew, bnew) }
  )
  list(position = position, threshold = threshold, method = pick,
       needs_bucket = isTRUE(NEEDS_BUCKET[pick]), strata_breaks = breaks, map = fn)
})
names(deploy_maps) <- paste(picks$position, picks$threshold, sep = "_")

saveRDS(deploy_maps, "data/fp_recal_maps.rds")

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")

readr::write_csv(recal,     "output/21e_recal_probabilities.csv")
readr::write_csv(cal_all,   "output/21e_recal_calibration.csv")
readr::write_csv(cal_strat, "output/21e_recal_calibration_strat.csv")
readr::write_csv(summary_tbl |>
                   mutate(across(c(w_mean_abs_delta, ext_w_mean_abs_delta),
                                 ~ sprintf("%.4f", 100 * .x), .names = "{.col}_pp"),
                          brier = sprintf("%.5f", brier)) |>
                   select(position, threshold, method, ext_w_mean_abs_delta_pp, w_mean_abs_delta_pp, brier),
                 "output/21e_recal_summary.csv")
readr::write_csv(picks, "output/21e_recal_picks.csv")

cli_alert_success("output/21e_recal_probabilities.csv ({nrow(recal)} rows)")
cli_alert_success("output/21e_recal_calibration.csv")
cli_alert_success("output/21e_recal_calibration_strat.csv")
cli_alert_success("output/21e_recal_summary.csv")
cli_alert_success("output/21e_recal_picks.csv")
cli_alert_success("data/fp_recal_maps.rds (deployment maps -- SINGLE-WRITER: do not commit outside a coordinated ship pass, per feedback_single_writer_artifacts)")

cli_h1("Step 21e complete")
