# R/21a_discrimination_fns.R
# Discrimination (ranking-skill) metric library -- Stage A of the D29
# single-stage rebuild. Sourced by R/21b_discrimination_baseline.R and by
# R/21f_fp_grade.R; not run directly.
#
# WHY THIS FILE EXISTS: no discrimination/ranking metric existed anywhere
# in this repo before 2026-09-06 (grep -rn "AUC|roc_auc|pROC" R/ returns
# nothing computed, only three prose mentions in headers/comments). The
# entire validation apparatus was calibration-first -- interval coverage
# primary, sharpness only a tiebreak -- which is exactly how a model with
# a near-zero-information point estimate (the RB/WR/TE efficiency
# component) passed every gate this project has used and shipped. This is
# the ruler. R/21b freezes what it measures on the incumbent model and on
# ECR BEFORE any single-stage candidate model exists, per the D29
# pre-registration discipline: the ruler is built and frozen before the
# thing it measures.
#
# Bands are always defined on ECR pos_rank, never on model output -- a
# model-defined band would let the model choose its own exam, and the
# paired bootstrap in disc_compare() requires an identical row set for
# both rankers being compared.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10d_name_helpers.R")

# ===========================================================================
# Flex-band definitions (on ECR pos_rank)
# ===========================================================================

FLEX_BANDS <- list(WR = 20:35, RB = 15:30, TE = 8:16)

# Vectorized membership test, for callers with a mixed-position table.
flex_band <- function(position, pos_rank) {
  lo <- vapply(position, function(p) {
    b <- FLEX_BANDS[[p]]; if (is.null(b)) NA_integer_ else min(b)
  }, numeric(1))
  hi <- vapply(position, function(p) {
    b <- FLEX_BANDS[[p]]; if (is.null(b)) NA_integer_ else max(b)
  }, numeric(1))
  !is.na(lo) & pos_rank >= lo & pos_rank <= hi
}

# ===========================================================================
# ECR crosswalk + timing validity
# ===========================================================================
# Lifted from R/18a_market_edge_backtest.R:97-172 (the D25 market-edge
# backtest), factored into one reusable function so the copy that R/18a and
# R/18b currently duplicate inline has a single home going forward. Behavior
# is unchanged: same aliases, same team-fix map, same "team-kick, fallback
# to the week's first kick" timing rule.

ascii_norm <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

ECR_ALIASES <- c(
  "mitch trubisky"  = "mitchell trubisky",
  "bam knight"      = "zonovan knight",
  "josh palmer"     = "joshua palmer"
)

ECR_TEAM_FIX <- c("JAC" = "JAX", "LA" = "LAR", "WSH" = "WAS", "ARZ" = "ARI",
                  "HST" = "HOU", "BLT" = "BAL", "CLV" = "CLE", "SL" = "STL",
                  "OAK" = "OAK", "SD" = "SD")

# Reads every data/ecr_history/ecr_hist_*.csv, crosswalks player_name_norm
# to gsis_id via roster name matching, and flags each row's timing validity
# against that player's own kickoff (falling back to the week's first
# kickoff when the team can't be matched). `seasons` must cover every
# season the caller intends to query -- load_rosters()/load_schedules() are
# both re-pulled for exactly this range.
ecr_join <- function(seasons = 2016:2025) {
  ecr <- list.files("data/ecr_history", "^ecr_hist_.*\\.csv$", full.names = TRUE) |>
    map(read_csv, show_col_types = FALSE) |>
    list_rbind() |>
    mutate(capture_utc = as.POSIXct(as.character(wayback_ts),
                                    format = "%Y%m%d%H%M%S", tz = "UTC")) |>
    filter(season %in% seasons)

  rosters <- load_rosters(seasons) |>
    filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(gsis_id)) |>
    mutate(nm = ascii_norm(normalize_player_name(full_name))) |>
    distinct(season, position, nm, gsis_id)

  xw_pos <- rosters |>
    add_count(season, position, nm) |>
    filter(n == 1) |>
    select(season, position, nm, gsis_id)

  # Fallback for WR/TE-style position flips: name unique within season
  # across all four positions.
  xw_any <- rosters |>
    add_count(season, nm) |>
    filter(n == 1) |>
    select(season, nm, gsis_id_any = gsis_id)

  ecr <- ecr |>
    mutate(nm = ascii_norm(player_name_norm),
           nm = if_else(nm %in% names(ECR_ALIASES), unname(ECR_ALIASES[nm]), nm)) |>
    left_join(xw_pos, by = c("season", "position", "nm")) |>
    left_join(xw_any, by = c("season", "nm")) |>
    mutate(gsis_id = coalesce(gsis_id, gsis_id_any)) |>
    select(-gsis_id_any)

  sched <- load_schedules(seasons) |>
    filter(game_type == "REG") |>
    mutate(kick = as.POSIXct(paste(gameday, coalesce(gametime, "13:00")),
                             format = "%Y-%m-%d %H:%M",
                             tz = "America/New_York"))

  kicks <- bind_rows(
    sched |> select(season, week, team = home_team, kick),
    sched |> select(season, week, team = away_team, kick)
  )

  first_kick <- sched |>
    group_by(season, week) |>
    summarise(first_kick = min(kick), .groups = "drop")

  ecr |>
    mutate(team_std = coalesce(ECR_TEAM_FIX[team], team)) |>
    left_join(kicks, by = c("season", "week", "team_std" = "team")) |>
    left_join(first_kick, by = c("season", "week")) |>
    mutate(
      kick_eff  = coalesce(kick, first_kick),
      valid     = capture_utc < kick_eff,
      lag_days  = as.numeric(difftime(kick_eff, capture_utc, units = "days")),
      strict_ok = capture_utc < first_kick
    )
}

# Builds the in-band scoring universe for one position: ECR-valid rows
# whose pos_rank falls in that position's flex band, joined to realized
# fantasy_points_ppr and both hit_start/hit_boom outcomes. Returns the ECR
# side only (season, week, position, gsis_id, pos_rank, fp, hit_start,
# hit_boom, timing columns) -- callers left-join their own model score
# column(s) on (season, week, gsis_id) afterward, since different callers
# (21b's incumbent model vs 21f's candidate arms) have different score
# columns and different available season windows.
band_universe <- function(position, seasons, thresh_start, thresh_boom,
                          fp_col = "fantasy_points_ppr") {
  band <- FLEX_BANDS[[position]]
  if (is.null(band)) cli_abort("No flex band defined for position {position}")

  ecr <- ecr_join(seasons) |>
    filter(position == !!position, valid, !is.na(gsis_id), pos_rank %in% band)

  stats <- load_player_stats(seasons) |>
    filter(season_type == "REG", !is.na(player_id)) |>
    transmute(player_id, season, week, fp = .data[[fp_col]])

  ecr |>
    inner_join(stats, by = c("gsis_id" = "player_id", "season", "week")) |>
    filter(!is.na(fp)) |>
    mutate(hit_start = as.integer(fp >= thresh_start),
           hit_boom  = as.integer(fp >= thresh_boom)) |>
    select(season, week, position, gsis_id, pos_rank, fp, hit_start, hit_boom,
           lag_days, strict_ok)
}

# ===========================================================================
# Per-week sufficient statistics (precomputed ONCE, then cheaply recombined
# across bootstrap replicates -- avoids recomputing pairwise/rank stats
# thousands of times).
# ===========================================================================

# Fisher-z components per season-week: z = atanh(rho), weight = n-3. Weeks
# with n < 4 or an undefined rho (e.g. a constant score/outcome) get w = 0
# so they drop out of any pooled sum without breaking match()-based lookup
# in the bootstrap (every week key present in the input is still a row).
per_week_spearman <- function(score, outcome, wk) {
  tibble(score = score, outcome = outcome, wk = wk) |>
    group_by(wk) |>
    summarise(
      n   = n(),
      rho = suppressWarnings(cor(score, outcome, method = "spearman")),
      .groups = "drop"
    ) |>
    mutate(
      ok = n >= 4 & is.finite(rho),
      z  = if_else(ok, atanh(pmin(pmax(rho, -0.999999), 0.999999)), 0),
      w  = if_else(ok, n - 3, 0)
    ) |>
    select(wk, z, w)
}

pooled_spearman_take <- function(pw, take) {
  m <- pw[match(take, pw$wk), ]
  if (sum(m$w) == 0) return(NA_real_)
  tanh(sum(m$w * m$z) / sum(m$w))
}

pooled_spearman <- function(pw) pooled_spearman_take(pw, pw$wk)

# In-band pairwise concordance: for every within-week pair, does the score
# ordering agree with the outcome ordering? Ties in either the score or the
# outcome count as 0.5. Pooled by summing concordant/pairs counts across
# weeks (not averaging per-week rates), so pairs from busier weeks
# naturally carry more weight.
concordant_sum <- function(score, outcome) {
  n <- length(score)
  if (n < 2) return(0)
  idx <- combn(n, 2)
  s_sign <- sign(score[idx[1, ]] - score[idx[2, ]])
  o_sign <- sign(outcome[idx[1, ]] - outcome[idx[2, ]])
  sum(case_when(
    s_sign == 0        ~ 0.5,
    o_sign == 0        ~ 0.5,
    s_sign == o_sign   ~ 1,
    .default           = 0
  ))
}

per_week_concordance <- function(score, outcome, wk) {
  tibble(score = score, outcome = outcome, wk = wk) |>
    group_by(wk) |>
    summarise(
      pairs      = n() * (n() - 1) / 2,
      concordant = concordant_sum(score, outcome),
      .groups = "drop"
    )
}

pooled_concordance_take <- function(pw, take) {
  m <- pw[match(take, pw$wk), ]
  if (sum(m$pairs) == 0) return(NA_real_)
  sum(m$concordant) / sum(m$pairs)
}

pooled_concordance <- function(pw) pooled_concordance_take(pw, pw$wk)

# Pooled AUC via the Wilcoxon rank-sum identity -- not decomposed per week
# (AUC is SECONDARY/reported, not one of the two primaries, so a simpler,
# less-optimized implementation is an acceptable tradeoff).
pooled_auc <- function(score, hit) {
  hit <- as.integer(hit)
  n1 <- sum(hit == 1); n0 <- sum(hit == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score, ties.method = "average")
  (sum(r[hit == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ===========================================================================
# disc_cell(): one ranker's point estimate + week-clustered bootstrap CI,
# for all three statistics, over one band's data. No comparison, no
# verdict -- this is what freezes a standalone number (e.g. "ECR Spearman
# 0.26"). disc_compare() below is what grades a candidate against a frozen
# cell like this one.
# ===========================================================================

disc_cell <- function(d, score_col, outcome_col = "fp", hit_col = "hit_start",
                       week_col = "week", season_col = "season",
                       B = 2000L, seed = 42L) {
  wk <- paste(d[[season_col]], d[[week_col]])
  uw <- unique(wk)
  idx_by_week <- split(seq_along(wk), wk)
  score <- d[[score_col]]; outcome <- d[[outcome_col]]; hit <- d[[hit_col]]

  pw_s <- per_week_spearman(score, outcome, wk)
  pw_c <- per_week_concordance(score, outcome, wk)

  point <- tibble(
    n = nrow(d), weeks = length(uw),
    spearman    = pooled_spearman(pw_s),
    concordance = pooled_concordance(pw_c),
    auc         = pooled_auc(score, hit)
  )

  set.seed(seed)
  boot <- map_dfr(seq_len(B), function(i) {
    take <- sample(uw, length(uw), replace = TRUE)
    idx  <- unlist(idx_by_week[take], use.names = FALSE)
    tibble(
      spearman    = pooled_spearman_take(pw_s, take),
      concordance = pooled_concordance_take(pw_c, take),
      auc         = pooled_auc(score[idx], hit[idx])
    )
  })

  point |>
    mutate(
      spearman_lo    = quantile(boot$spearman, 0.025, na.rm = TRUE),
      spearman_hi    = quantile(boot$spearman, 0.975, na.rm = TRUE),
      concordance_lo = quantile(boot$concordance, 0.025, na.rm = TRUE),
      concordance_hi = quantile(boot$concordance, 0.975, na.rm = TRUE),
      auc_lo         = quantile(boot$auc, 0.025, na.rm = TRUE),
      auc_hi         = quantile(boot$auc, 0.975, na.rm = TRUE)
    )
}

# ===========================================================================
# disc_compare(): paired week-clustered bootstrap of (score_b - score_a) on
# one statistic. Paired, not two independent CIs -- both rankers share the
# same rows, so the difference has far less variance than either marginal.
# Mirrors the week-cluster resampling in R/18a::brier_cell and
# R/18b::boot_diff.
# ===========================================================================

disc_compare <- function(d, score_a_col, score_b_col, outcome_col = "fp",
                         hit_col = "hit_start", week_col = "week",
                         season_col = "season",
                         stat = c("spearman", "concordance", "auc"),
                         B = 2000L, seed = 42L) {
  stat <- match.arg(stat)
  wk <- paste(d[[season_col]], d[[week_col]])
  uw <- unique(wk)
  idx_by_week <- split(seq_along(wk), wk)

  score_a <- d[[score_a_col]]; score_b <- d[[score_b_col]]
  outcome <- d[[outcome_col]]; hit <- d[[hit_col]]

  if (stat == "spearman") {
    pw_a <- per_week_spearman(score_a, outcome, wk)
    pw_b <- per_week_spearman(score_b, outcome, wk)
    stat_a <- pooled_spearman(pw_a); stat_b <- pooled_spearman(pw_b)
  } else if (stat == "concordance") {
    pw_a <- per_week_concordance(score_a, outcome, wk)
    pw_b <- per_week_concordance(score_b, outcome, wk)
    stat_a <- pooled_concordance(pw_a); stat_b <- pooled_concordance(pw_b)
  } else {
    stat_a <- pooled_auc(score_a, hit); stat_b <- pooled_auc(score_b, hit)
  }

  set.seed(seed)
  diffs <- map_dbl(seq_len(B), function(i) {
    take <- sample(uw, length(uw), replace = TRUE)
    if (stat == "spearman") {
      pooled_spearman_take(pw_b, take) - pooled_spearman_take(pw_a, take)
    } else if (stat == "concordance") {
      pooled_concordance_take(pw_b, take) - pooled_concordance_take(pw_a, take)
    } else {
      idx <- unlist(idx_by_week[take], use.names = FALSE)
      pooled_auc(score_b[idx], hit[idx]) - pooled_auc(score_a[idx], hit[idx])
    }
  })

  tibble(
    n = nrow(d), weeks = length(uw), stat = stat,
    stat_a = stat_a, stat_b = stat_b, delta = stat_b - stat_a,
    ci_lo = quantile(diffs, 0.025), ci_hi = quantile(diffs, 0.975)
  )
}
