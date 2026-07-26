# R/09b_qb_recalibration.R
# Step 9b: Walk-forward recalibration of QB P(FP >= 20) / P(FP >= 25).
#
# 06c analog for the QB chain, built directly on the volume-conditional
# design that won for RB/WR (2026-07-17): pooled maps are honest on average
# but blind to conditional miscalibration, so conditional candidates enter
# the bake-off alongside the pooled incumbents.
#
# QB conditioning axis: EX-ANTE predicted carries (pred_carry, the 08c rush
# volume model output). Rushing identity is where QB probability is likely
# to be conditionally mispriced (08c watch item: rush-component shrinkage
# +0.092 EPA per prior carry, t=5.4, partially offset by the pass side --
# whether any of it survives translation is exactly what this stratified
# judge measures). pred_carry rather than prior_carries_pg so the deployed
# map signature matches RB/WR: function(p, pred_vol) with pred_vol = the
# model's own ex-ante volume prediction. Strata reuse the statue/mover/
# scrambler breaks (4/8) on pred_carry. Scrambler stratum is thin (~7% of
# rows) -- MIN_STRAT_N pooled fallback protects the early walk-forward.
#
# Methods (same as 06c): raw | platt | iso | strat_platt | strat_iso |
# platt_vol. PRE-COMMITTED judge: n-weighted mean |delta| over (stratum x
# probability bin) cells on the eval window; Brier must not degrade vs raw.
# Walk-forward: 2014-2015 burn-in; from 2016 on, each season-week scored by
# maps fit on all rows strictly before it, refit weekly.
# Coherence: recalibrated P(25+) capped at recalibrated P(20+) per row.
# Deployment: data/qb_fp_recal_maps.rds, self-contained closures with
# uniform signature function(p, pred_vol).

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

EVAL_START_SEASON <- 2016L
P_EPS             <- 1e-4
MIN_STRAT_N       <- 300L

STRATA_BREAKS <- c(-Inf, 4, 8, Inf)   # statue/mover/scrambler on pred_carry
STRATA_LABELS <- c("exante_statue", "exante_mover", "exante_scrambler")

stratum_of <- function(pred_carry) {
  cut(pred_carry, STRATA_BREAKS, labels = STRATA_LABELS, right = FALSE)
}

fmt_pp <- function(x) sprintf("%+.1f", 100 * x)

# ===========================================================================
# RECALIBRATION FITTERS -- identical to 06c; every returned prediction
# function binds what it needs LOCALLY (deployment closures are saveRDS'd
# and loaded in fresh sessions; globalenv bindings do not serialize)
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

# Smooth Vegas-conditional maps (2026-07-26 extension; judge rule
# pre-committed before the run -- see 06c header for the full statement).
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

`%||%` <- function(a, b) if (is.null(a)) b else a

# ===========================================================================
# LOAD QB PROBABILITIES
# ===========================================================================

cli_h1("Step 9b: QB walk-forward recalibration (rush-conditional bake-off)")

probs <- readr::read_csv("output/09a_qb_fp_sim_probabilities.csv",
                         show_col_types = FALSE) |>
  filter(!is.na(fantasy_points)) |>
  select(player_id, season, week, pred_carry,
         p_start = p_start_sim, p_boom = p_boom_sim, hit_start, hit_boom) |>
  mutate(stratum = stratum_of(pred_carry)) |>
  arrange(season, week)

# Opener Vegas covariates (2026-07-26 extension) via feature-table keys
vegas_open <- readRDS("data/vegas_open_lines.rds")
IT_CENTER  <- median(vegas_open$implied_total, na.rm = TRUE)
vkeys <- readRDS("data/qb_feature_table.rds") |> filter(!is.na(player_id)) |>
  distinct(player_id, season, week, game_id, posteam)
probs <- probs |>
  left_join(vkeys, by = c("player_id", "season", "week")) |>
  left_join(vegas_open, by = c("game_id", "posteam")) |>
  select(-game_id, -posteam)
cli_alert_info("Opener covariates: {round(100 * mean(!is.na(probs$team_spread)), 1)}% coverage")

spread_bucket <- function(s) cut(s, c(-Inf, -6.5, -2.5, 2.5, 6.5, Inf),
  labels = c("big_dog", "dog", "close", "fav", "big_fav"))
itotal_bucket <- function(it) cut(it, c(-Inf, 20, 26, Inf),
  labels = c("low_implied", "mid_implied", "high_implied"))
probs <- probs |>
  mutate(sb = spread_bucket(team_spread), ib = itotal_bucket(implied_total))

cli_alert_success("{nrow(probs)} scored QB-weeks | seasons {min(probs$season)}-{max(probs$season)}")

cli_h2("Ex-ante stratum counts (all seasons)")
print(probs |> count(stratum), n = Inf)

# ===========================================================================
# WALK-FORWARD LOOP
# ===========================================================================

cli_h1("Walk-forward weekly refits")

eval_weeks <- probs |>
  filter(season >= EVAL_START_SEASON) |>
  distinct(season, week) |>
  arrange(season, week)

cli_alert_info("{nrow(eval_weeks)} evaluation season-weeks x 2 thresholds x 5 methods")

recal_one <- function(df, p_col, hit_col) {
  n <- nrow(df)
  out <- list(platt = rep(NA_real_, n), iso = rep(NA_real_, n),
              strat_platt = rep(NA_real_, n), strat_iso = rep(NA_real_, n),
              platt_vol = rep(NA_real_, n),
              platt_vegas = rep(NA_real_, n), platt_vol_vegas = rep(NA_real_, n))
  for (i in seq_len(nrow(eval_weeks))) {
    s <- eval_weeks$season[i]; w <- eval_weeks$week[i]
    idx_test  <- which(df$season == s & df$week == w)
    if (!length(idx_test)) next
    idx_train <- which(df$season < s | (df$season == s & df$week < w))
    p_tr <- df[[p_col]][idx_train]; h_tr <- df[[hit_col]][idx_train]
    s_tr <- df$stratum[idx_train];  v_tr <- df$pred_carry[idx_train]
    sp_tr <- df$team_spread[idx_train]; it_tr <- df$implied_total[idx_train]
    p_te <- df[[p_col]][idx_test]
    s_te <- df$stratum[idx_test];   v_te <- df$pred_carry[idx_test]
    sp_te <- df$team_spread[idx_test]; it_te <- df$implied_total[idx_test]

    f_platt <- fit_platt(p_tr, h_tr)
    out$platt[idx_test] <- f_platt(p_te)
    out$iso[idx_test]   <- fit_isotonic(p_tr, h_tr)(p_te)
    out$strat_platt[idx_test] <- fit_strat(p_tr, h_tr, s_tr, fit_platt)(p_te, s_te)
    out$strat_iso[idx_test]   <- fit_strat(p_tr, h_tr, s_tr, fit_isotonic)(p_te, s_te)
    f_pv <- fit_platt_vol(p_tr, h_tr, v_tr)
    out$platt_vol[idx_test] <-
      if (is.null(f_pv)) f_platt(p_te) else f_pv(p_te, v_te)
    f_pg <- fit_platt_vegas(p_tr, h_tr, sp_tr, it_tr, IT_CENTER)
    out$platt_vegas[idx_test] <-
      if (is.null(f_pg)) f_platt(p_te) else f_pg(p_te, sp_te, it_te)
    f_pvg <- fit_platt_vol_vegas(p_tr, h_tr, v_tr, sp_tr, it_tr, IT_CENTER)
    out$platt_vol_vegas[idx_test] <-
      if (is.null(f_pvg)) f_platt(p_te) else f_pvg(p_te, v_te, sp_te, it_te)
  }
  as_tibble(out)
}

CAND_METHODS <- c("platt", "iso", "strat_platt", "strat_iso", "platt_vol",
                  "platt_vegas", "platt_vol_vegas")

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
# EVALUATION
# ===========================================================================

cli_h1("Evaluation on {EVAL_START_SEASON}+ (out-of-time)")

col_for <- function(thresh, method) {
  stem <- if (thresh == "20+") "p_start" else "p_boom"
  if (method == "raw") stem else paste0(stem, "_", method)
}

calibrate <- function(df, prob_col, hit_col, method, thresh, group_col = NULL) {
  g <- df |>
    filter(!is.na(.data[[prob_col]])) |>
    mutate(bin = cut(.data[[prob_col]], seq(0, 1, 0.1),
                     include.lowest = TRUE, right = FALSE))
  if (!is.null(group_col)) g <- g |> filter(!is.na(.data[[group_col]]))
  g <- if (!is.null(group_col)) group_by(g, cell = .data[[group_col]], bin) else group_by(g, bin)
  g |>
    summarise(n = n(), pred = mean(.data[[prob_col]]),
              emp = mean(.data[[hit_col]]), .groups = "drop") |>
    mutate(delta = emp - pred, method = method, threshold = thresh,
           position = "QB")
}

grid <- expand_grid(thresh = c("20+", "25+"), method = c("raw", CAND_METHODS))

cal_all <- pmap(grid, function(thresh, method) {
  calibrate(recal, col_for(thresh, method),
            if (thresh == "20+") "hit_start" else "hit_boom", method, thresh)
}) |> list_rbind()

cal_by <- function(group_col) {
  pmap(grid, function(thresh, method) {
    calibrate(recal, col_for(thresh, method),
              if (thresh == "20+") "hit_start" else "hit_boom", method, thresh,
              group_col = group_col)
  }) |> list_rbind()
}
cal_strat <- cal_by("stratum")
cal_sb    <- cal_by("sb")
cal_ib    <- cal_by("ib")
ext_cells <- bind_rows(cal_strat |> mutate(axis = "volume"),
                       cal_sb    |> mutate(axis = "spread"),
                       cal_ib    |> mutate(axis = "implied"))

brier <- pmap(grid, function(thresh, method) {
  p <- recal[[col_for(thresh, method)]]
  h <- as.numeric(recal[[if (thresh == "20+") "hit_start" else "hit_boom"]])
  tibble(position = "QB", threshold = thresh, method = method,
         brier = mean((p - h)^2, na.rm = TRUE))
}) |> list_rbind()

summary_tbl <- cal_all |>
  group_by(position, threshold, method) |>
  summarise(w_mean_abs_delta = weighted.mean(abs(delta), n), .groups = "drop") |>
  left_join(
    ext_cells |>
      group_by(position, threshold, method) |>
      summarise(ext_w_mean_abs_delta = weighted.mean(abs(delta), n),
                .groups = "drop"),
    by = c("position", "threshold", "method")
  ) |>
  left_join(brier, by = c("position", "threshold", "method"))

cli_h2("Judge metric (EXTENDED: vol + spread + implied cells) + pooled |delta| (pp) + Brier")
print(summary_tbl |>
        mutate(pooled_pp = sprintf("%.2f", 100 * w_mean_abs_delta),
               ext_pp    = sprintf("%.2f", 100 * ext_w_mean_abs_delta),
               brier     = sprintf("%.5f", brier)) |>
        select(threshold, method, ext_pp, pooled_pp, brier) |>
        arrange(threshold, ext_pp), n = Inf)

picks <- summary_tbl |>
  group_by(position, threshold) |>
  group_modify(~ {
    raw_brier <- .x$brier[.x$method == "raw"]
    cand <- .x |> filter(method != "raw", brier <= raw_brier + 1e-4)
    if (!nrow(cand)) return(tibble(pick = "raw", reason = "no method beat raw Brier"))
    best <- cand |> slice_min(ext_w_mean_abs_delta, n = 1, with_ties = FALSE)
    tibble(pick = best$method, reason = "lowest extended weighted |delta|, Brier ok")
  }) |>
  ungroup()

cli_h2("Method picks (pre-committed rule)")
print(picks, n = Inf)

cli_h2("Stratum-level honesty (mean stated vs empirical, pp delta)")
strata_tbl <- pmap(picks, function(position, threshold, pick, reason) {
  hit_col <- if (threshold == "20+") "hit_start" else "hit_boom"
  methods <- unique(c("raw", "iso", pick))
  map(methods, function(m) {
    recal |>
      filter(!is.na(.data[[col_for(threshold, m)]])) |>
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
        select(threshold, method, stratum, n, pred, emp, delta_pp),
      n = Inf)

# ===========================================================================
# DEPLOYMENT MAPS (winning method, refit on ALL data)
# ===========================================================================

cli_h1("Deployment maps (refit on all seasons)")

all_rows <- probs

deploy_maps <- pmap(picks, function(position, threshold, pick, reason) {
  p <- all_rows[[if (threshold == "20+") "p_start" else "p_boom"]]
  h <- all_rows[[if (threshold == "20+") "hit_start" else "hit_boom"]]
  s <- all_rows$stratum
  v <- all_rows$pred_carry
  sp <- all_rows$team_spread
  it <- all_rows$implied_total
  breaks <- STRATA_BREAKS
  labs   <- STRATA_LABELS
  # Uniform signature (widened 2026-07-26): function(p, pred_vol,
  # team_spread, implied_total); non-Vegas methods ignore the extras.
  fn <- switch(pick,
    raw         = function(pnew, vnew, spnew, itnew) pnew,
    platt       = { f <- fit_platt(p, h)
                    function(pnew, vnew, spnew, itnew) f(pnew) },
    iso         = { f <- fit_isotonic(p, h)
                    function(pnew, vnew, spnew, itnew) f(pnew) },
    strat_platt = { f <- fit_strat(p, h, s, fit_platt)
                    function(pnew, vnew, spnew, itnew)
                      f(pnew, cut(vnew, breaks, labels = labs, right = FALSE)) },
    strat_iso   = { f <- fit_strat(p, h, s, fit_isotonic)
                    function(pnew, vnew, spnew, itnew)
                      f(pnew, cut(vnew, breaks, labels = labs, right = FALSE)) },
    platt_vol   = { f <- fit_platt_vol(p, h, v) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew"))
                      function(pnew, vnew, spnew, itnew) f(pnew)
                    else function(pnew, vnew, spnew, itnew) f(pnew, vnew) },
    platt_vegas = { f <- fit_platt_vegas(p, h, sp, it, IT_CENTER) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew"))
                      function(pnew, vnew, spnew, itnew) f(pnew)
                    else function(pnew, vnew, spnew, itnew) f(pnew, spnew, itnew) },
    platt_vol_vegas = { f <- fit_platt_vol_vegas(p, h, v, sp, it, IT_CENTER) %||% fit_platt(p, h)
                    if (identical(names(formals(f)), "pnew"))
                      function(pnew, vnew, spnew, itnew) f(pnew)
                    else function(pnew, vnew, spnew, itnew) f(pnew, vnew, spnew, itnew) }
  )
  list(position = position, threshold = threshold, method = pick,
       strata_breaks = breaks, map = fn)
})
names(deploy_maps) <- paste("QB", picks$threshold, sep = "_")

saveRDS(deploy_maps, "data/qb_fp_recal_maps.rds")

vol_refs <- all_rows |>
  group_by(stratum) |>
  summarise(pred_vol_ref = median(pred_carry), .groups = "drop")

map_grid <- map(deploy_maps, function(m) {
  pmap(vol_refs, function(stratum, pred_vol_ref) {
    p_seq   <- seq(0, 1, 0.01)
    p_recal <- m$map(p_seq, rep(pred_vol_ref, length(p_seq)),
                     rep(0, length(p_seq)), rep(NA_real_, length(p_seq)))
    tibble(position = "QB", threshold = m$threshold, method = m$method,
           stratum = as.character(stratum), pred_vol_ref = pred_vol_ref,
           p_raw = p_seq, p_recal = p_recal)
  }) |> list_rbind()
}) |> list_rbind()

# ===========================================================================
# SAVE
# ===========================================================================

cli_h1("Save outputs")

readr::write_csv(recal,     "output/09b_qb_recal_probabilities.csv")
readr::write_csv(cal_all,   "output/09b_qb_recal_calibration.csv")
readr::write_csv(cal_strat, "output/09b_qb_recal_calibration_strat.csv")
readr::write_csv(summary_tbl |>
                   mutate(across(c(w_mean_abs_delta, ext_w_mean_abs_delta),
                                 ~ sprintf("%.4f", 100 * .x), .names = "{.col}_pp"),
                          brier = sprintf("%.5f", brier)) |>
                   select(position, threshold, method,
                          ext_w_mean_abs_delta_pp, w_mean_abs_delta_pp, brier),
                 "output/09b_qb_recal_summary.csv")
readr::write_csv(strata_tbl, "output/09b_qb_recal_strata.csv")
readr::write_csv(map_grid,   "output/09b_qb_recal_map_grid.csv")

cli_alert_success("output/09b_qb_recal_probabilities.csv ({nrow(recal)} rows)")
cli_alert_success("output/09b_qb_recal_calibration.csv")
cli_alert_success("output/09b_qb_recal_calibration_strat.csv")
cli_alert_success("output/09b_qb_recal_summary.csv")
cli_alert_success("output/09b_qb_recal_strata.csv")
cli_alert_success("output/09b_qb_recal_map_grid.csv")
cli_alert_success("data/qb_fp_recal_maps.rds (deployment maps)")

cli_h1("Step 9b complete")
