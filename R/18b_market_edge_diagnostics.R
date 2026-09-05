# R/18b_market_edge_diagnostics.R
# POST-HOC DIAGNOSTICS for the D25 market-edge backtest (18a). Labeled
# as such: nothing here re-grades the pre-registered verdicts (QB
# NEUTRAL x2, RB/WR/TE pooled FAIL x2). Purpose is troubleshooting --
# locate WHERE the model loses to ECR and whether any orthogonal signal
# survives, to steer the roadmap.
#
# Expectations stated before running (Steve session 2026-09-05):
#   Blend: modest positive at QB, ~zero at RB/WR.
#   Disagreement: market wins disagreement rows at skill positions,
#     worst where the model is BULLISH (role-change blindness).
#   Injury slice: losses concentrate in injury-listed weeks at WR/TE
#     (no shipped injury features), less at RB (rung 1 shipped).
#   Early season: weeks 1-4 worse than mid-season despite volfix.
#
# A. Walk-forward blend test: glm(hit ~ qlogis(p_ecr) + qlogis(p_model)),
#    trained on scored seasons strictly before the eval season, evaluated
#    forward. Blend beating ECR-only = the model carries incremental
#    information even where it loses head-to-head.
# B. Cohort slices of per-row skill (Brier_ecr - Brier_model):
#    disagreement direction, ECR rank bucket, injury-listed, season phase.
#
# Universe/join/baseline identical to 18a (code intentionally mirrored --
# each stage script stays self-contained, repo convention).
#
# Usage: Rscript R/18b_market_edge_diagnostics.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10d_name_helpers.R")

set.seed(42)
B_BOOT <- 2000L

cli_h1("18b: market-edge diagnostics (post-hoc, non-binding)")

# ================================================= rebuild 18a universe --
ecr <- list.files("data/ecr_history", "^ecr_hist_.*\\.csv$", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  list_rbind() |>
  mutate(capture_utc = as.POSIXct(as.character(wayback_ts),
                                  format = "%Y%m%d%H%M%S", tz = "UTC"))

qb <- read_csv("output/09b_qb_recal_probabilities.csv", show_col_types = FALSE) |>
  transmute(position = "QB", player_id, season, week,
            p_model_start = p_start_platt_vol_vegas,
            p_model_boom  = p_boom_platt)
rbwr <- read_csv("output/06c_volfix_10acand_recal_probabilities.csv",
                 show_col_types = FALSE) |>
  transmute(position, player_id, season, week,
            p_model_start = p_start,
            p_model_boom  = if_else(position == "WR", p_boom_iso, p_boom))
te <- read_csv("output/12e_te_volfix_10acand_recal_probabilities.csv",
               show_col_types = FALSE) |>
  transmute(position = "TE", player_id, season, week,
            p_model_start = p_start, p_model_boom = p_boom)
model <- bind_rows(qb, rbwr, te) |>
  filter(!is.na(p_model_start), !is.na(p_model_boom))

THRESH <- tribble(
  ~position, ~start_thresh, ~boom_thresh, ~fp_col,
  "QB", 20, 25, "fantasy_points",
  "RB", 15, 20, "fantasy_points_ppr",
  "WR", 15, 20, "fantasy_points_ppr",
  "TE", 12, 17, "fantasy_points_ppr"
)
EVAL_START <- c(QB = 2018L, RB = 2023L, WR = 2023L, TE = 2023L)

stats <- load_player_stats(2016:2025) |>
  filter(season_type == "REG", !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points, fantasy_points_ppr)

ascii_norm <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
ALIASES <- c("mitch trubisky" = "mitchell trubisky",
             "bam knight" = "zonovan knight",
             "josh palmer" = "joshua palmer")

rosters <- load_rosters(2016:2025) |>
  filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(gsis_id)) |>
  mutate(nm = ascii_norm(normalize_player_name(full_name))) |>
  distinct(season, position, nm, gsis_id)
xw_pos <- rosters |> add_count(season, position, nm) |> filter(n == 1) |>
  select(season, position, nm, gsis_id)
xw_any <- rosters |> add_count(season, nm) |> filter(n == 1) |>
  select(season, nm, gsis_id_any = gsis_id)

ecr <- ecr |>
  mutate(nm = ascii_norm(player_name_norm),
         nm = if_else(nm %in% names(ALIASES), unname(ALIASES[nm]), nm)) |>
  left_join(xw_pos, by = c("season", "position", "nm")) |>
  left_join(xw_any, by = c("season", "nm")) |>
  mutate(gsis_id = coalesce(gsis_id, gsis_id_any)) |>
  select(-gsis_id_any)

TEAM_FIX <- c("JAC" = "JAX", "LA" = "LAR", "WSH" = "WAS", "ARZ" = "ARI",
              "HST" = "HOU", "BLT" = "BAL", "CLV" = "CLE", "SL" = "STL")
sched <- load_schedules(2016:2025) |>
  filter(game_type == "REG") |>
  mutate(kick = as.POSIXct(paste(gameday, coalesce(gametime, "13:00")),
                           format = "%Y-%m-%d %H:%M", tz = "America/New_York"))
kicks <- bind_rows(
  sched |> select(season, week, team = home_team, kick),
  sched |> select(season, week, team = away_team, kick))
first_kick <- sched |> group_by(season, week) |>
  summarise(first_kick = min(kick), .groups = "drop")

ecr <- ecr |>
  mutate(team_std = coalesce(TEAM_FIX[team], team)) |>
  left_join(kicks, by = c("season", "week", "team_std" = "team")) |>
  left_join(first_kick, by = c("season", "week")) |>
  mutate(kick_eff = coalesce(kick, first_kick),
         valid = capture_utc < kick_eff)

base_tbl <- ecr |>
  filter(valid, !is.na(gsis_id)) |>
  inner_join(stats, by = c("gsis_id" = "player_id", "season", "week")) |>
  inner_join(THRESH, by = "position") |>
  mutate(fp = if_else(fp_col == "fantasy_points", fantasy_points,
                      fantasy_points_ppr),
         hit_start = as.integer(fp >= start_thresh),
         hit_boom  = as.integer(fp >= boom_thresh)) |>
  filter(!is.na(fp)) |>
  select(season, week, position, gsis_id, pos_rank, hit_start, hit_boom)

fit_iso_rank <- function(rank, hit) {
  o <- order(-rank)
  fit <- isoreg(x = (-rank)[o], y = hit[o])
  function(newrank) approx(fit$x, fit$yf, xout = -newrank,
                           rule = 2, ties = mean)$y
}
predict_baseline <- function(df, hit_col) {
  df$p_ecr <- NA_real_
  for (s in sort(unique(df$season))) {
    train <- base_tbl |> filter(position == df$position[1], season < s)
    if (n_distinct(paste(train$season, train$week)) < 12) next
    f <- fit_iso_rank(train$pos_rank, train[[hit_col]])
    idx <- which(df$season == s)
    df$p_ecr[idx] <- f(df$pos_rank[idx])
  }
  df
}

scored <- model |>
  inner_join(ecr |> filter(valid, !is.na(gsis_id)) |>
               select(season, week, position, gsis_id, pos_rank),
             by = c("season", "week", "position", "player_id" = "gsis_id")) |>
  inner_join(stats, by = c("player_id", "season", "week")) |>
  inner_join(THRESH, by = "position") |>
  mutate(fp = if_else(fp_col == "fantasy_points", fantasy_points,
                      fantasy_points_ppr),
         hit_start = as.integer(fp >= start_thresh),
         hit_boom  = as.integer(fp >= boom_thresh)) |>
  filter(!is.na(fp), season >= EVAL_START[position]) |>
  group_split(position) |>
  map(function(d) {
    d |> predict_baseline("hit_start") |> rename(p_ecr_start = p_ecr) |>
      predict_baseline("hit_boom") |> rename(p_ecr_boom = p_ecr)
  }) |>
  list_rbind() |>
  filter(!is.na(p_ecr_start), !is.na(p_ecr_boom))

cli_alert_info("Universe rebuilt: {nrow(scored)} rows (18a had the same construction)")

# =============================================================== A. blend --
cli_h1("A. Walk-forward blend test (does the model add info on top of ECR?)")

ql <- function(p) qlogis(pmin(pmax(p, 0.001), 0.999))

blend_walkforward <- function(d, outcome) {
  hit <- paste0("hit_", outcome)
  pm  <- paste0("p_model_", outcome)
  pe  <- paste0("p_ecr_", outcome)
  d$p_blend <- NA_real_
  coefs <- list()
  for (s in sort(unique(d$season))) {
    tr <- d |> filter(season < s)
    if (n_distinct(paste(tr$season, tr$week)) < 10) next
    fit <- glm(tr[[hit]] ~ ql(tr[[pe]]) + ql(tr[[pm]]),
               family = binomial())
    idx <- which(d$season == s)
    eta <- coef(fit)[1] + coef(fit)[2] * ql(d[[pe]][idx]) +
      coef(fit)[3] * ql(d[[pm]][idx])
    d$p_blend[idx] <- plogis(eta)
    coefs[[as.character(s)]] <- tibble(season = s,
                                       b_ecr = coef(fit)[2],
                                       b_model = coef(fit)[3])
  }
  list(d = d |> filter(!is.na(p_blend)), coefs = list_rbind(coefs))
}

boot_diff <- function(d, p_a, p_b, hit_col) {
  # positive = p_b better than p_a
  wk <- paste(d$season, d$week)
  ibw <- split(seq_along(wk), wk); uw <- names(ibw)
  hit <- d[[hit_col]]; pa <- d[[p_a]]; pb <- d[[p_b]]
  diffs <- map_dbl(seq_len(B_BOOT), function(b) {
    i <- unlist(ibw[sample(uw, length(uw), TRUE)], use.names = FALSE)
    mean((pa[i] - hit[i])^2) - mean((pb[i] - hit[i])^2)
  })
  c(lo = unname(quantile(diffs, 0.025)), hi = unname(quantile(diffs, 0.975)))
}

blend_results <- map(list(
  list(name = "QB", rows = scored |> filter(position == "QB")),
  list(name = "RB/WR/TE pooled", rows = scored |> filter(position != "QB"))
), function(grp) {
  map(c("start", "boom"), function(oc) {
    b <- blend_walkforward(grp$rows, oc)
    d <- b$d
    hit <- d[[paste0("hit_", oc)]]
    be <- mean((d[[paste0("p_ecr_", oc)]] - hit)^2)
    bm <- mean((d[[paste0("p_model_", oc)]] - hit)^2)
    bb <- mean((d$p_blend - hit)^2)
    ci <- boot_diff(d, paste0("p_ecr_", oc), "p_blend", paste0("hit_", oc))
    tibble(group = grp$name, outcome = oc, n = nrow(d),
           weeks = n_distinct(paste(d$season, d$week)),
           brier_ecr = be, brier_model = bm, brier_blend = bb,
           blend_gain_vs_ecr = be - bb, ci_lo = ci["lo"], ci_hi = ci["hi"],
           mean_b_model = mean(b$coefs$b_model),
           mean_b_ecr = mean(b$coefs$b_ecr))
  }) |> list_rbind()
}) |> list_rbind()

print(blend_results |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

# ============================================================ B. cohorts --
cli_h1("B. Cohort slices of skill (positive = model better)")

slice_skill <- function(d, label_col) {
  d |>
    group_by(grp = .data[[label_col]], is_qb = position == "QB") |>
    summarise(
      n = n(), weeks = n_distinct(paste(season, week)),
      hit_rate_start = mean(hit_start),
      skill_start = mean((p_ecr_start - hit_start)^2) -
        mean((p_model_start - hit_start)^2),
      skill_boom = mean((p_ecr_boom - hit_boom)^2) -
        mean((p_model_boom - hit_boom)^2),
      .groups = "drop"
    ) |>
    mutate(position_group = if_else(is_qb, "QB", "RB/WR/TE")) |>
    select(-is_qb)
}

# B1: disagreement direction (start probability, 10pp threshold)
scored <- scored |>
  mutate(
    delta_start = p_model_start - p_ecr_start,
    disagreement = case_when(
      delta_start > 0.10 ~ "model_bullish",
      delta_start < -0.10 ~ "model_bearish",
      .default = "agree_within_10pp"
    ),
    rank_bucket = cut(pos_rank, c(0, 6, 12, 24, Inf),
                      labels = c("1-6", "7-12", "13-24", "25+")),
    phase = case_when(week <= 4 ~ "wk 1-4", week <= 13 ~ "wk 5-13",
                      .default = "wk 14+")
  )

cli_h2("B1. Disagreement direction (start prob, +/-10pp)")
b1 <- slice_skill(scored, "disagreement")
print(b1 |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        arrange(position_group, grp) |> as.data.frame(), row.names = FALSE)

cli_h2("B2. ECR rank bucket")
b2 <- slice_skill(scored, "rank_bucket")
print(b2 |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        arrange(position_group, grp) |> as.data.frame(), row.names = FALSE)

cli_h2("B3. Injury-listed that week (official report, any status)")
inj <- load_injuries(2016:2025) |>
  filter(!is.na(gsis_id)) |>
  distinct(season, week, gsis_id) |>
  mutate(listed = "listed")
scored <- scored |>
  left_join(inj, by = c("season", "week", "player_id" = "gsis_id")) |>
  mutate(listed = replace_na(listed, "not_listed"))
b3 <- scored |>
  group_by(listed, position) |>
  summarise(
    n = n(),
    skill_start = mean((p_ecr_start - hit_start)^2) -
      mean((p_model_start - hit_start)^2),
    skill_boom = mean((p_ecr_boom - hit_boom)^2) -
      mean((p_model_boom - hit_boom)^2),
    .groups = "drop"
  )
print(b3 |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        arrange(position, listed) |> as.data.frame(), row.names = FALSE)

cli_h2("B4. Season phase")
b4 <- slice_skill(scored, "phase")
print(b4 |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        arrange(position_group, grp) |> as.data.frame(), row.names = FALSE)

write_csv(blend_results, "output/18b_blend_results.csv")
write_csv(b1, "output/18b_cohort_disagreement.csv")
write_csv(b2, "output/18b_cohort_rank_bucket.csv")
write_csv(b3, "output/18b_cohort_injury.csv")
write_csv(b4, "output/18b_cohort_phase.csv")

cli_h1("18b complete -- diagnostics only, D25 verdicts unchanged")
