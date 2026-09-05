# R/18d_rb_star_recal.R
# PRE-REGISTERED D27 (bars approved by Steve 2026-09-05 before this
# script was written): RB star-dispersion recalibration fix.
#
# Problem (measured in 18c): shipped RB probabilities (method "raw",
# no map at all) are under-dispersed vs trailing-FP status -- top-12
# +10.2pp/+8.4pp underconfident (start/boom), ranks 13-24 -4.5pp and
# non-stars -3.9pp overconfident.
#
# CANDIDATES (walk-forward weekly refits, train = strictly earlier
# rows, exactly the 06c machinery pattern; RB rows of the shipped
# volfix probability file, 2023-2025):
#   star_platt      glm(hit ~ qlogis(p) + bucket)
#   star_platt_int  glm(hit ~ qlogis(p) * bucket)
#   star_iso        isotonic per bucket (>=40 train rows in bucket,
#                   else pooled isotonic fallback)
# Bucket = trailing-FP rank (last 17 games played, >=6 games, ranked
# within RB per week, the exact 18c definition): b1 = 1-12,
# b2 = 13-24, b3 = 25+/insufficient history. Live-path integration
# note: this metric must be added to 10a/10b2 on any ship (see
# feedback_ship_pass_all_live_paths).
#
# BARS (locked, per cell RB 15+ start and RB 20+ boom, graded on the
# common post-burn-in support, all three simultaneously):
#   1. |gap(b1)| <= 4pp   (gap = realized hit rate - mean predicted)
#   2. |gap(b2)| <= 4pp AND |gap(b3)| <= 4pp
#   3. pooled Brier <= raw pooled Brier + 0.0005
# SELECTION (locked, anti-variant-shopping): simplest passing
# candidate, order star_platt > star_platt_int > star_iso,
# independently per cell.
# EXPECTATION (stated before running): star_platt passes both cells,
# pooled Brier ~unchanged.
#
# REPORTED (not a bar): 18a-style re-check vs the ECR baseline with
# the selected probabilities substituted for RB -- how much of the
# -0.0084 RB market deficit closes. Expectation ~half. Ship decision
# is Steve's, after this report.
#
# Usage: Rscript R/18d_rb_star_recal.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10d_name_helpers.R")

set.seed(42)
B_BOOT <- 2000L
GAP_BAR <- 0.04
BRIER_TOL <- 0.0005
MIN_BUCKET_TRAIN <- 20L
MIN_ISO_BUCKET <- 40L

cli_h1("18d: RB star-dispersion recal (pre-registered D27)")

# ============================================================== inputs --
rb <- read_csv("output/06c_volfix_10acand_recal_probabilities.csv",
               show_col_types = FALSE) |>
  filter(position == "RB") |>
  select(player_id, season, week, p_start, p_boom, hit_start, hit_boom,
         pred_vol, team_spread, implied_total) |>
  arrange(season, week)

cli_alert_info("{nrow(rb)} shipped RB player-weeks (2023-2025)")

# ------------------------------------------------ trailing-FP buckets --
game_log <- load_player_stats(2016:2025) |>
  filter(season_type == "REG", !is.na(player_id),
         !is.na(fantasy_points_ppr)) |>
  select(player_id, season, week, fantasy_points_ppr) |>
  arrange(player_id, season, week)

trailing <- game_log |>
  group_by(player_id) |>
  mutate(
    g = row_number(),
    cum = cumsum(fantasy_points_ppr),
    prior_games = g - 1L,
    trail_n = pmin(prior_games, 17L),
    trail_fp = if_else(
      prior_games > 0,
      (lag(cum, 1, default = 0) - lag(cum, 18, default = 0)) / trail_n,
      NA_real_)
  ) |>
  ungroup() |>
  select(player_id, season, week, trail_fp, trail_n)

rb <- rb |>
  left_join(trailing, by = c("player_id", "season", "week")) |>
  group_by(season, week) |>
  mutate(
    trail_rank = ifelse(!is.na(trail_fp) & trail_n >= 6,
                        rank(-ifelse(!is.na(trail_fp) & trail_n >= 6,
                                     trail_fp, -Inf),
                             ties.method = "first"),
                        NA_integer_),
    bucket = factor(case_when(
      !is.na(trail_rank) & trail_rank <= 12 ~ "b1",
      !is.na(trail_rank) & trail_rank <= 24 ~ "b2",
      .default = "b3"
    ), levels = c("b3", "b1", "b2"))  # b3 reference level
  ) |>
  ungroup()

cli_alert_info("Bucket rows: {paste(count(rb, bucket)$n, collapse = '/')} (b3/b1/b2)")

# ======================================================== walk-forward --
ql <- function(p) qlogis(pmin(pmax(p, 0.001), 0.999))
clamp <- function(p) pmin(pmax(p, 0.001), 0.999)

fit_iso_pooled <- function(p, h) {
  o <- order(p)
  f <- isoreg(p[o], h[o])
  function(pn) clamp(approx(f$x, f$yf, xout = pn, rule = 2, ties = mean)$y)
}

eval_weeks <- rb |> distinct(season, week) |> arrange(season, week)

run_candidates <- function(p_col, hit_col) {
  n <- nrow(rb)
  out <- list(star_platt = rep(NA_real_, n),
              star_platt_int = rep(NA_real_, n),
              star_iso = rep(NA_real_, n))
  for (i in seq_len(nrow(eval_weeks))) {
    s <- eval_weeks$season[i]; w <- eval_weeks$week[i]
    test <- which(rb$season == s & rb$week == w)
    train <- which(rb$season < s | (rb$season == s & rb$week < w))
    if (!length(test) || !length(train)) next
    tr <- rb[train, ]
    if (any(table(tr$bucket) < MIN_BUCKET_TRAIN) ||
        n_distinct(tr$bucket) < 3) next
    te <- rb[test, ]
    lp_tr <- ql(tr[[p_col]]); h_tr <- tr[[hit_col]]
    lp_te <- ql(te[[p_col]])

    f1 <- glm(h_tr ~ lp_tr + bucket, data = tr, family = binomial())
    out$star_platt[test] <- clamp(predict(
      f1, newdata = tibble(lp_tr = lp_te, bucket = te$bucket),
      type = "response"))

    f2 <- glm(h_tr ~ lp_tr * bucket, data = tr, family = binomial())
    out$star_platt_int[test] <- clamp(predict(
      f2, newdata = tibble(lp_tr = lp_te, bucket = te$bucket),
      type = "response"))

    pooled_iso <- fit_iso_pooled(tr[[p_col]], h_tr)
    iso_by_bucket <- map(set_names(levels(tr$bucket)), function(b) {
      bi <- tr$bucket == b
      if (sum(bi) >= MIN_ISO_BUCKET)
        fit_iso_pooled(tr[[p_col]][bi], h_tr[bi]) else pooled_iso
    })
    out$star_iso[test] <- map_dbl(seq_along(test), function(j) {
      iso_by_bucket[[as.character(te$bucket[j])]](te[[p_col]][j])
    })
  }
  as_tibble(out)
}

cli_h2("Walk-forward refits: start (15+)")
cand_start <- run_candidates("p_start", "hit_start") |>
  rename_with(~ paste0("s_", .x))
cli_h2("Walk-forward refits: boom (20+)")
cand_boom <- run_candidates("p_boom", "hit_boom") |>
  rename_with(~ paste0("b_", .x))

rb <- bind_cols(rb, cand_start, cand_boom)

# ============================================================= grading --
common <- rb |> filter(!is.na(s_star_platt), !is.na(b_star_platt))
cli_alert_info("Common post-burn-in support: {nrow(common)} rows, {n_distinct(paste(common$season, common$week))} weeks")

grade_cell <- function(d, p_col, hit_col, raw_col, cand_name) {
  gaps <- d |>
    group_by(bucket) |>
    summarise(gap = mean(.data[[hit_col]]) - mean(.data[[p_col]]),
              n = n(), .groups = "drop")
  g <- set_names(gaps$gap, gaps$bucket)
  brier <- mean((d[[p_col]] - d[[hit_col]])^2)
  brier_raw <- mean((d[[raw_col]] - d[[hit_col]])^2)
  tibble(
    candidate = cand_name,
    gap_b1 = g["b1"], gap_b2 = g["b2"], gap_b3 = g["b3"],
    brier = brier, brier_raw = brier_raw,
    bar1 = abs(g["b1"]) <= GAP_BAR,
    bar2 = abs(g["b2"]) <= GAP_BAR & abs(g["b3"]) <= GAP_BAR,
    bar3 = brier <= brier_raw + BRIER_TOL,
    pass = bar1 & bar2 & bar3
  )
}

CAND_ORDER <- c("star_platt", "star_platt_int", "star_iso")

grade_outcome <- function(prefix, p_raw, hit_col, label) {
  g <- map(CAND_ORDER, function(cn) {
    grade_cell(common, paste0(prefix, cn), hit_col, p_raw, cn)
  }) |> list_rbind() |>
    mutate(cell = label, .before = 1)
  raw_gaps <- common |> group_by(bucket) |>
    summarise(gap = mean(.data[[hit_col]]) - mean(.data[[p_raw]]),
              .groups = "drop")
  list(grades = g, raw_gaps = raw_gaps,
       selected = CAND_ORDER[which(g$pass)[1]])
}

res_start <- grade_outcome("s_", "p_start", "hit_start", "RB 15+ start")
res_boom  <- grade_outcome("b_", "p_boom", "hit_boom", "RB 20+ boom")

grades <- bind_rows(res_start$grades, res_boom$grades)

cli_h1("PRE-REGISTERED GRADES (bars: |gaps| <= 4pp, Brier <= raw + 0.0005)")
cli_alert_info("Raw gaps on same support -- start: {paste(sprintf('%s %+.3f', res_start$raw_gaps$bucket, res_start$raw_gaps$gap), collapse = ', ')}")
cli_alert_info("Raw gaps on same support -- boom:  {paste(sprintf('%s %+.3f', res_boom$raw_gaps$bucket, res_boom$raw_gaps$gap), collapse = ', ')}")
print(grades |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

sel_start <- res_start$selected
sel_boom  <- res_boom$selected
cli_alert_success("Selected (simplest passing): start = {coalesce(sel_start, 'NONE PASSED')}, boom = {coalesce(sel_boom, 'NONE PASSED')}")

write_csv(grades, "output/18d_rb_star_recal_grades.csv")

if (is.na(sel_start) || is.na(sel_boom)) {
  cli_alert_warning("A cell has no passing candidate -- D27 records a null for that cell; stopping before the ECR recheck.")
  quit(save = "no", status = 0)
}

# Selected probabilities, with 06c-style coherence (boom <= start).
selected <- common |>
  transmute(player_id, season, week,
            p_start_new = .data[[paste0("s_", sel_start)]],
            p_boom_new = pmin(.data[[paste0("b_", sel_boom)]],
                              .data[[paste0("s_", sel_start)]]),
            p_start_raw = p_start, p_boom_raw = p_boom,
            hit_start, hit_boom, bucket)
write_csv(selected, "output/18d_rb_star_recal_probabilities.csv")

# ================================================= REPORTED: ECR recheck --
cli_h1("REPORTED (not a bar): 18a-style market recheck with new RB probs")

ecr <- list.files("data/ecr_history", "^ecr_hist_.*\\.csv$", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |> list_rbind() |>
  mutate(capture_utc = as.POSIXct(as.character(wayback_ts),
                                  format = "%Y%m%d%H%M%S", tz = "UTC"))
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
sched <- load_schedules(2016:2025) |> filter(game_type == "REG") |>
  mutate(kick = as.POSIXct(paste(gameday, coalesce(gametime, "13:00")),
                           format = "%Y-%m-%d %H:%M", tz = "America/New_York"))
kicks <- bind_rows(sched |> select(season, week, team = home_team, kick),
                   sched |> select(season, week, team = away_team, kick))
first_kick <- sched |> group_by(season, week) |>
  summarise(first_kick = min(kick), .groups = "drop")
ecr <- ecr |>
  mutate(team_std = coalesce(TEAM_FIX[team], team)) |>
  left_join(kicks, by = c("season", "week", "team_std" = "team")) |>
  left_join(first_kick, by = c("season", "week")) |>
  mutate(valid = capture_utc < coalesce(kick, first_kick))

stats <- load_player_stats(2016:2025) |>
  filter(season_type == "REG", !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points, fantasy_points_ppr)
THRESH <- tribble(
  ~position, ~start_thresh, ~boom_thresh, ~fp_col,
  "RB", 15, 20, "fantasy_points_ppr",
  "WR", 15, 20, "fantasy_points_ppr",
  "TE", 12, 17, "fantasy_points_ppr")

wrte <- bind_rows(
  read_csv("output/06c_volfix_10acand_recal_probabilities.csv",
           show_col_types = FALSE) |>
    filter(position == "WR") |>
    transmute(position, player_id, season, week,
              p_model_start = p_start, p_model_boom = p_boom_iso),
  read_csv("output/12e_te_volfix_10acand_recal_probabilities.csv",
           show_col_types = FALSE) |>
    transmute(position = "TE", player_id, season, week,
              p_model_start = p_start, p_model_boom = p_boom))

model_old <- bind_rows(
  selected |> transmute(position = "RB", player_id, season, week,
                        p_model_start = p_start_raw,
                        p_model_boom = p_boom_raw),
  wrte)
model_new <- bind_rows(
  selected |> transmute(position = "RB", player_id, season, week,
                        p_model_start = p_start_new,
                        p_model_boom = p_boom_new),
  wrte)

base_tbl <- ecr |>
  filter(valid, !is.na(gsis_id), position %in% c("RB", "WR", "TE")) |>
  inner_join(stats, by = c("gsis_id" = "player_id", "season", "week")) |>
  inner_join(THRESH, by = "position") |>
  mutate(hit_start = as.integer(fantasy_points_ppr >= start_thresh),
         hit_boom  = as.integer(fantasy_points_ppr >= boom_thresh)) |>
  filter(!is.na(fantasy_points_ppr)) |>
  select(season, week, position, pos_rank, hit_start, hit_boom)

fit_iso_rank <- function(rank, hit) {
  o <- order(-rank)
  f <- isoreg((-rank)[o], hit[o])
  function(nr) approx(f$x, f$yf, xout = -nr, rule = 2, ties = mean)$y
}

score_universe <- function(model_tbl) {
  d <- model_tbl |>
    inner_join(ecr |> filter(valid, !is.na(gsis_id)) |>
                 select(season, week, position, gsis_id, pos_rank),
               by = c("season", "week", "position",
                      "player_id" = "gsis_id")) |>
    inner_join(stats, by = c("player_id", "season", "week")) |>
    inner_join(THRESH, by = "position") |>
    mutate(hit_start = as.integer(fantasy_points_ppr >= start_thresh),
           hit_boom  = as.integer(fantasy_points_ppr >= boom_thresh)) |>
    filter(!is.na(fantasy_points_ppr), season >= 2023)
  d |> group_split(position) |>
    map(function(g) {
      g$p_ecr_start <- NA_real_; g$p_ecr_boom <- NA_real_
      for (s in sort(unique(g$season))) {
        tr <- base_tbl |> filter(position == g$position[1], season < s)
        if (n_distinct(paste(tr$season, tr$week)) < 12) next
        i <- which(g$season == s)
        g$p_ecr_start[i] <- fit_iso_rank(tr$pos_rank, tr$hit_start)(g$pos_rank[i])
        g$p_ecr_boom[i]  <- fit_iso_rank(tr$pos_rank, tr$hit_boom)(g$pos_rank[i])
      }
      g |> filter(!is.na(p_ecr_start))
    }) |> list_rbind()
}

skill_cell <- function(d, outcome) {
  hit <- d[[paste0("hit_", outcome)]]
  pm <- d[[paste0("p_model_", outcome)]]; pe <- d[[paste0("p_ecr_", outcome)]]
  wk <- paste(d$season, d$week); ibw <- split(seq_along(wk), wk)
  uw <- names(ibw)
  diffs <- map_dbl(seq_len(B_BOOT), function(b) {
    i <- unlist(ibw[sample(uw, length(uw), TRUE)], use.names = FALSE)
    mean((pe[i] - hit[i])^2) - mean((pm[i] - hit[i])^2)
  })
  tibble(n = nrow(d), weeks = length(uw),
         brier_model = mean((pm - hit)^2), brier_ecr = mean((pe - hit)^2),
         skill = mean((pe - hit)^2) - mean((pm - hit)^2),
         ci_lo = quantile(diffs, 0.025), ci_hi = quantile(diffs, 0.975))
}

su_old <- score_universe(model_old)
su_new <- score_universe(model_new)
# identical-support guard: score both on the intersection of keys
key <- function(d) paste(d$player_id, d$season, d$week)
kk <- intersect(key(su_old), key(su_new))
su_old <- su_old |> filter(key(su_old) %in% kk)
su_new <- su_new |> filter(key(su_new) %in% kk)

recheck <- map(list(
  list(lab = "RB start", f = \(d) d |> filter(position == "RB"), oc = "start"),
  list(lab = "RB boom",  f = \(d) d |> filter(position == "RB"), oc = "boom"),
  list(lab = "pooled start", f = identity, oc = "start"),
  list(lab = "pooled boom",  f = identity, oc = "boom")
), function(cc) {
  bind_rows(
    skill_cell(cc$f(su_old), cc$oc) |> mutate(model = "shipped_raw"),
    skill_cell(cc$f(su_new), cc$oc) |> mutate(model = "star_recal")
  ) |> mutate(cell = cc$lab, .before = 1)
}) |> list_rbind()

print(recheck |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)
write_csv(recheck, "output/18d_market_recheck.csv")

cli_h1("18d complete -- grades locked; recheck is reported context for the ship call")
