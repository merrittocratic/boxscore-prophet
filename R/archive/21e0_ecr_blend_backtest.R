# R/21e0_ecr_blend_backtest.R
# Stage C ablation, arm A7: does the single-stage model add information ON
# TOP of ECR, even in the flex band where standalone discrimination (R/21f)
# came back NEUTRAL for both A1 and A1b? A model that loses head-to-head can
# still carry incremental information -- that's a different, still-real
# product claim ("the model improves on the consensus"), and it's the
# planned fallback if standalone discrimination never clears the ship gate.
#
# Machinery is lifted from R/18a_market_edge_backtest.R (fit_iso_rank,
# predict_baseline -- the walk-forward isotonic pos_rank->P(hit) ECR
# baseline) and R/18b_market_edge_diagnostics.R (blend_walkforward,
# boot_diff), each stage script self-contained per repo convention. R/21a's
# ecr_join()/band_universe() supply the crosswalk and flex-band universe so
# this evaluates the EXACT SAME rows R/21f already graded -- this is the
# only way "the model beats ECR standalone: NEUTRAL, but adds info in a
# blend: PASS" is a coherent pair of claims rather than two different
# populations.
#
# NOTE: fit_iso_rank/predict_baseline need a full-support baseline table
# (ALL ranked+played players, not just the flex band) so the isotonic curve
# has real data outside the band to interpolate against -- exactly 18a's
# design. band-restriction happens only when JOINING the candidate's
# predictions onto it, not when fitting the baseline itself.
#
# Usage: Rscript R/21e0_ecr_blend_backtest.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/21a_discrimination_fns.R")

set.seed(42)
B_BOOT <- 2000L
FULL_SEASONS <- 2016:2025
THRESH <- list(RB = c(start = 15, boom = 20), WR = c(start = 15, boom = 20))
POSITIONS <- c("RB", "WR")
ARMS <- c("base", "floorfree")

cli_h1("21e0: ECR+model blend backtest (A7) -- flex band, matched to R/21f's universe")

ql <- function(p) qlogis(pmin(pmax(p, 0.001), 0.999))

# ---------------------------------------------------------------------------
# Full-support ECR baseline fitting table: ALL ranked+played RB/WR players,
# not just the flex band -- same construction as R/18a:176-186.
# ---------------------------------------------------------------------------
stats <- load_player_stats(FULL_SEASONS) |>
  filter(season_type == "REG", !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points_ppr)

ecr_all <- ecr_join(FULL_SEASONS) |> filter(position %in% POSITIONS, valid, !is.na(gsis_id))

base_tbl <- ecr_all |>
  inner_join(stats, by = c("gsis_id" = "player_id", "season", "week")) |>
  rowwise() |>
  mutate(
    th_start = THRESH[[position]]["start"], th_boom = THRESH[[position]]["boom"],
    hit_start = as.integer(fantasy_points_ppr >= th_start),
    hit_boom  = as.integer(fantasy_points_ppr >= th_boom)
  ) |>
  ungroup() |>
  select(season, week, position, gsis_id, pos_rank, hit_start, hit_boom)

cli_alert_info("Baseline fitting table: {nrow(base_tbl)} ranked+played RB/WR player-weeks")

fit_iso_rank <- function(rank, hit) {
  o <- order(-rank)
  fit <- isoreg(x = (-rank)[o], y = hit[o])
  xs <- fit$x; ys <- fit$yf
  function(newrank) approx(xs, ys, xout = -newrank, rule = 2, ties = mean)$y
}

predict_baseline <- function(df, hit_col, this_position) {
  df$p_ecr <- NA_real_
  for (s in sort(unique(df$season))) {
    train <- base_tbl |> filter(position == this_position, season < s)
    n_weeks <- n_distinct(paste(train$season, train$week))
    if (n_weeks < 12) next
    f <- fit_iso_rank(train$pos_rank, train[[hit_col]])
    idx <- which(df$season == s)
    df$p_ecr[idx] <- f(df$pos_rank[idx])
  }
  df
}

# ---------------------------------------------------------------------------
# Walk-forward blend (R/18b:173-193) + week-clustered paired bootstrap
# (R/18b:195-205), unchanged.
# ---------------------------------------------------------------------------
blend_walkforward <- function(d, outcome) {
  hit <- paste0("hit_", outcome); pm <- paste0("p_model_", outcome); pe <- paste0("p_ecr_", outcome)
  d$p_blend <- NA_real_
  for (s in sort(unique(d$season))) {
    tr <- d |> filter(season < s)
    if (n_distinct(paste(tr$season, tr$week)) < 10) next
    fit <- glm(tr[[hit]] ~ ql(tr[[pe]]) + ql(tr[[pm]]), family = binomial())
    idx <- which(d$season == s)
    eta <- coef(fit)[1] + coef(fit)[2] * ql(d[[pe]][idx]) + coef(fit)[3] * ql(d[[pm]][idx])
    d$p_blend[idx] <- plogis(eta)
  }
  d |> filter(!is.na(p_blend))
}

boot_diff <- function(d, p_a, p_b, hit_col) {
  wk <- paste(d$season, d$week)
  ibw <- split(seq_along(wk), wk); uw <- names(ibw)
  hit <- d[[hit_col]]; pa <- d[[p_a]]; pb <- d[[p_b]]
  diffs <- map_dbl(seq_len(B_BOOT), function(b) {
    i <- unlist(ibw[sample(uw, length(uw), TRUE)], use.names = FALSE)
    mean((pa[i] - hit[i])^2) - mean((pb[i] - hit[i])^2)
  })
  c(lo = unname(quantile(diffs, 0.025)), hi = unname(quantile(diffs, 0.975)))
}

# ---------------------------------------------------------------------------
# Per position x arm: the SAME flex-band paired universe R/21f graded,
# ECR probability baseline applied on top, then blended against pred_fp
# converted to a probability via p_start_raw/p_boom_raw (R/21d's direct
# conformal-inversion probabilities -- raw, pre-recalibration, same
# caveat R/21f already carries).
# ---------------------------------------------------------------------------
all_results <- list()

for (pos in POSITIONS) {
  th <- THRESH[[pos]]
  band_d <- band_universe(pos, FULL_SEASONS, th["start"], th["boom"])

  for (arm in ARMS) {
    path <- sprintf("output/21d_%s_%s_fold_predictions.csv", tolower(pos), arm)
    if (!file.exists(path)) { cli_alert_warning("Missing {path} -- skipped"); next }
    cand <- read_csv(path, show_col_types = FALSE) |> mutate(player_id = as.character(player_id))

    paired <- band_d |>
      inner_join(cand |> select(season, week, player_id, p_model_start = p_start_raw, p_model_boom = p_boom_raw),
                by = c("season", "week", "gsis_id" = "player_id"))

    paired <- paired |>
      predict_baseline("hit_start", pos) |> rename(p_ecr_start = p_ecr) |>
      predict_baseline("hit_boom", pos) |> rename(p_ecr_boom = p_ecr) |>
      filter(!is.na(p_ecr_start), !is.na(p_ecr_boom))

    cli_alert_info("{pos} [{arm}]: {nrow(paired)} paired in-band rows with a valid ECR baseline ({n_distinct(paste(paired$season,paired$week))} season-weeks)")
    if (nrow(paired) < 50) { cli_alert_warning("Too few rows -- skipped"); next }

    res <- map(c("start", "boom"), function(oc) {
      b <- blend_walkforward(paired, oc)
      hit <- b[[paste0("hit_", oc)]]
      be <- mean((b[[paste0("p_ecr_", oc)]] - hit)^2)
      bm <- mean((b[[paste0("p_model_", oc)]] - hit)^2)
      bb <- mean((b$p_blend - hit)^2)
      ci <- boot_diff(b, paste0("p_ecr_", oc), "p_blend", paste0("hit_", oc))
      tibble(position = pos, arm = arm, outcome = oc, n = nrow(b),
             weeks = n_distinct(paste(b$season, b$week)),
             brier_ecr = be, brier_model = bm, brier_blend = bb,
             blend_gain_vs_ecr = be - bb, ci_lo = ci["lo"], ci_hi = ci["hi"],
             model_gain_vs_ecr = be - bm,
             verdict = case_when(ci["lo"] > 0 ~ "PASS", ci["hi"] < 0 ~ "FAIL", .default = "NEUTRAL"))
    }) |> list_rbind()

    all_results[[paste(pos, arm)]] <- res
  }
}

results <- list_rbind(all_results)

cli_h1("A7 RESULTS -- blend gain vs ECR (positive = blend beats ECR alone), positive verdict = PASS")
print(results |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)

dir.create("output", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/21e0_ecr_blend_results.csv")
cli_alert_success("output/21e0_ecr_blend_results.csv")
cli_h1("21e0 complete")
