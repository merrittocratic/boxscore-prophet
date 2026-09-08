# R/21o_market_edge_backtest_fp1.R
# Re-run of D25's PRE-REGISTERED market-edge backtest (R/18a_market_edge_
# backtest.R), single-stage (fp1) architecture substituted for RB/WR only.
#
# WHY THIS SCRIPT EXISTS: D25 (2026-09-05) found the market beats the
# (then two-stage) model on RB/WR/TE start/boom -- a real, documented FAIL,
# not an unmeasured gap (see CLAUDE.md, corrected 2026-09-08). That result
# is part of what motivated the D29 single-stage rebuild. The open
# question the rebuild was FOR is whether the new architecture closes that
# gap. This script answers it, using data already on disk -- no new
# harvesting, no new hindcast slate scoring, no waiting on live weeks.
#
# WHAT CHANGED FROM 18a, AND WHAT DID NOT (per CLAUDE.md: "Bars never move
# after data arrives; overrides are signed, not laundered" -- this is a
# re-test of the same locked design, not a new experiment with new bars):
#   - RB/WR "model" input: was output/06c_volfix_10acand_recal_probabilities
#     .csv (two-stage, eff x vol). NOW output/21e_recal_probabilities.csv
#     (single-stage, RB=floor-free/WR=base -- Steve's 2026-09-08 explicit
#     ship decision), using each position x threshold's ACTUAL WINNING
#     RECAL METHOD from output/21e_recal_picks.csv (RB 15+=strat_iso,
#     RB 20+=platt_vol_vegas, WR 15+=platt, WR 20+=platt) -- hardcoded here
#     exactly as 18a hardcoded the two-stage model's own shipped variants,
#     not joined dynamically, so this script's inputs are locked the same
#     way 18a's were.
#   - TE, QB: UNTOUCHED. TE has no single-stage arm (confirmed 2026-09-08,
#     stays two-stage) and QB was never in scope for the rebuild. Both
#     read the exact same files 18a did.
#   - EVERYTHING ELSE byte-for-byte identical to 18a: ECR archive, name
#     crosswalk, timing validity, walk-forward isotonic ECR baseline
#     (fit on strictly-prior seasons), outcome definitions, EVAL_START
#     (QB=2018, RB/WR/TE=2023), Brier skill metric, week-clustered
#     bootstrap (B=2000, seed=42), and the LOCKED bars (PASS = CI excludes
#     0 AND relative improvement >= 2%; FAIL = CI excludes 0 the other
#     way; else NEUTRAL).
#
# Usage: Rscript R/21o_market_edge_backtest_fp1.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10d_name_helpers.R")

set.seed(42)
B_BOOT <- 2000L

cli_h1("21o: model-vs-ECR market edge backtest, fp1 re-test of D25's locked design")

# ============================================================== inputs --
ecr <- list.files("data/ecr_history", "^ecr_hist_.*\\.csv$", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  list_rbind() |>
  mutate(capture_utc = as.POSIXct(as.character(wayback_ts),
                                  format = "%Y%m%d%H%M%S", tz = "UTC"))

qb <- read_csv("output/09b_qb_recal_probabilities.csv", show_col_types = FALSE) |>
  transmute(position = "QB", player_id, season, week,
            p_model_start = p_start_platt_vol_vegas,
            p_model_boom  = p_boom_platt)

# fp1 swap: single-stage RB=floor-free/WR=base recal output, each
# position x threshold's actual winning method from 21e_recal_picks.csv.
fp1_probs <- read_csv("output/21e_recal_probabilities.csv", show_col_types = FALSE)
rbwr <- fp1_probs |>
  transmute(position, player_id, season, week,
            p_model_start = if_else(position == "RB", p_start_strat_iso, p_start_platt),
            p_model_boom  = if_else(position == "RB", p_boom_platt_vol_vegas, p_boom_platt))

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

# ============================================================ outcomes --
cli_h2("Outcomes from nflreadr player stats (same defs as 18a/06/09a)")
stats <- load_player_stats(2016:2025) |>
  filter(season_type == "REG", !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points, fantasy_points_ppr)

# =========================================================== crosswalk --
cli_h2("ECR name -> gsis crosswalk")
ascii_norm <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

ALIASES <- c(
  "mitch trubisky"  = "mitchell trubisky",
  "bam knight"      = "zonovan knight",
  "josh palmer"     = "joshua palmer"
)

rosters <- load_rosters(2016:2025) |>
  filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(gsis_id)) |>
  mutate(nm = ascii_norm(normalize_player_name(full_name))) |>
  distinct(season, position, nm, gsis_id)

xw_pos <- rosters |>
  add_count(season, position, nm) |>
  filter(n == 1) |>
  select(season, position, nm, gsis_id)

xw_any <- rosters |>
  add_count(season, nm) |>
  filter(n == 1) |>
  select(season, nm, gsis_id_any = gsis_id)

ecr <- ecr |>
  mutate(nm = ascii_norm(player_name_norm),
         nm = if_else(nm %in% names(ALIASES), unname(ALIASES[nm]), nm)) |>
  left_join(xw_pos, by = c("season", "position", "nm")) |>
  left_join(xw_any, by = c("season", "nm")) |>
  mutate(gsis_id = coalesce(gsis_id, gsis_id_any)) |>
  select(-gsis_id_any)

cli_alert_info("ECR rows with a gsis id: {sum(!is.na(ecr$gsis_id))}/{nrow(ecr)}")

# ====================================================== timing validity --
cli_h2("Per-player kickoff validity")
TEAM_FIX <- c("JAC" = "JAX", "LA" = "LAR", "WSH" = "WAS", "ARZ" = "ARI",
              "HST" = "HOU", "BLT" = "BAL", "CLV" = "CLE", "SL" = "STL",
              "OAK" = "OAK", "SD" = "SD")

sched <- load_schedules(2016:2025) |>
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

ecr <- ecr |>
  mutate(team_std = coalesce(TEAM_FIX[team], team)) |>
  left_join(kicks, by = c("season", "week", "team_std" = "team")) |>
  left_join(first_kick, by = c("season", "week")) |>
  mutate(
    kick_eff   = coalesce(kick, first_kick),
    valid      = capture_utc < kick_eff,
    lag_days   = as.numeric(difftime(kick_eff, capture_utc, units = "days")),
    strict_ok  = capture_utc < first_kick,
    scoring_ok = case_when(
      position == "QB" ~ source_page == "qb",
      .default = source_page %in% c("ppr-rb", "ppr-wr", "ppr-te", "ppr-flex")
    )
  )

cli_alert_info("Team-kick matched: {sum(!is.na(ecr$kick))}/{nrow(ecr)} rows; valid (pre-own-kick): {sum(ecr$valid)}")

# ============================================== baseline fitting table --
base_tbl <- ecr |>
  filter(valid, !is.na(gsis_id)) |>
  inner_join(stats, by = c("gsis_id" = "player_id", "season", "week")) |>
  inner_join(THRESH, by = "position") |>
  mutate(fp = if_else(fp_col == "fantasy_points", fantasy_points,
                      fantasy_points_ppr),
         hit_start = as.integer(fp >= start_thresh),
         hit_boom  = as.integer(fp >= boom_thresh)) |>
  filter(!is.na(fp)) |>
  select(season, week, position, gsis_id, pos_rank, hit_start, hit_boom,
         lag_days, strict_ok, scoring_ok)

fit_iso_rank <- function(rank, hit) {
  o <- order(-rank)
  fit <- isoreg(x = (-rank)[o], y = hit[o])
  xs <- fit$x; ys <- fit$yf
  function(newrank) approx(xs, ys, xout = -newrank, rule = 2, ties = mean)$y
}

predict_baseline <- function(df, hit_col) {
  df$p_ecr <- NA_real_
  for (s in sort(unique(df$season))) {
    train <- base_tbl |>
      filter(position == df$position[1], season < s)
    n_weeks <- n_distinct(paste(train$season, train$week))
    if (n_weeks < 12) next
    f <- fit_iso_rank(train$pos_rank, train[[hit_col]])
    idx <- which(df$season == s)
    df$p_ecr[idx] <- f(df$pos_rank[idx])
  }
  df
}

# ================================================ scored eval universe --
scored <- model |>
  inner_join(ecr |> filter(valid, !is.na(gsis_id)) |>
               select(season, week, position, gsis_id, pos_rank,
                      lag_days, strict_ok, scoring_ok),
             by = c("season", "week", "position", "player_id" = "gsis_id")) |>
  inner_join(stats, by = c("player_id", "season", "week")) |>
  inner_join(THRESH, by = "position") |>
  mutate(fp = if_else(fp_col == "fantasy_points", fantasy_points,
                      fantasy_points_ppr),
         hit_start = as.integer(fp >= start_thresh),
         hit_boom  = as.integer(fp >= boom_thresh)) |>
  filter(!is.na(fp), season >= EVAL_START[position])

scored <- scored |>
  group_split(position) |>
  map(function(d) {
    d |> predict_baseline("hit_start") |> rename(p_ecr_start = p_ecr) |>
      predict_baseline("hit_boom") |> rename(p_ecr_boom = p_ecr)
  }) |>
  list_rbind() |>
  filter(!is.na(p_ecr_start), !is.na(p_ecr_boom))

cli_alert_info("Scored universe: {nrow(scored)} player-weeks, {n_distinct(paste(scored$season, scored$week))} season-weeks")

# ============================================================= scoring --
brier_cell <- function(d, outcome) {
  hit <- d[[paste0("hit_", outcome)]]
  pm  <- d[[paste0("p_model_", outcome)]]
  pe  <- d[[paste0("p_ecr_", outcome)]]
  wk  <- paste(d$season, d$week)
  uw  <- unique(wk)
  idx_by_week <- split(seq_along(wk), wk)
  bm <- mean((pm - hit)^2); be <- mean((pe - hit)^2)
  diffs <- map_dbl(seq_len(B_BOOT), function(b) {
    take <- sample(uw, length(uw), replace = TRUE)
    idx <- unlist(idx_by_week[take], use.names = FALSE)
    mean((pe[idx] - hit[idx])^2) - mean((pm[idx] - hit[idx])^2)
  })
  tibble(
    n = nrow(d), weeks = length(uw),
    brier_model = bm, brier_ecr = be,
    skill = be - bm, rel_improve = (be - bm) / be,
    ci_lo = quantile(diffs, 0.025), ci_hi = quantile(diffs, 0.975)
  ) |>
    mutate(verdict = case_when(
      ci_lo > 0 & rel_improve >= 0.02 ~ "PASS",
      ci_hi < 0 ~ "FAIL",
      .default = "NEUTRAL"
    ))
}

CELLS <- tribble(
  ~cell, ~tier, ~positions, ~outcome,
  "QB start (20+)",          "primary",   list("QB"), "start",
  "QB boom (25+)",           "primary",   list("QB"), "boom",
  "RB/WR/TE pooled start",   "secondary", list(c("RB", "WR", "TE")), "start",
  "RB/WR/TE pooled boom",    "secondary", list(c("RB", "WR", "TE")), "boom",
  "RB start (15+)",          "reported",  list("RB"), "start",
  "RB boom (20+)",           "reported",  list("RB"), "boom",
  "WR start (15+)",          "reported",  list("WR"), "start",
  "WR boom (20+)",           "reported",  list("WR"), "boom",
  "TE start (12+)",          "reported",  list("TE"), "start",
  "TE boom (17+)",           "reported",  list("TE"), "boom"
)

results <- CELLS |>
  pmap(function(cell, tier, positions, outcome) {
    d <- scored |> filter(position %in% positions[[1]])
    brier_cell(d, outcome) |> mutate(cell = cell, tier = tier, .before = 1)
  }) |>
  list_rbind()

cli_h1("RE-TEST RESULTS (fp1 RB/WR, locked D25 bars)")
print(results |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

# ======================================================= sensitivities --
sens <- list(
  strict_first_kick = scored |> filter(strict_ok),
  lag_within_3d     = scored |> filter(lag_days <= 3),
  scoring_matched   = scored |> filter(scoring_ok)
) |>
  imap(function(d, nm) {
    CELLS |> filter(tier != "reported") |>
      pmap(function(cell, tier, positions, outcome) {
        dd <- d |> filter(position %in% positions[[1]])
        if (nrow(dd) < 50) return(NULL)
        brier_cell(dd, outcome) |>
          mutate(sensitivity = nm, cell = cell, .before = 1)
      }) |>
      list_rbind()
  }) |>
  list_rbind()

cli_h2("Sensitivities (non-binding)")
print(sens |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

by_season <- scored |>
  group_by(position, season) |>
  summarise(
    n = n(), weeks = n_distinct(week),
    skill_start = mean((p_ecr_start - hit_start)^2) -
                  mean((p_model_start - hit_start)^2),
    skill_boom  = mean((p_ecr_boom - hit_boom)^2) -
                  mean((p_model_boom - hit_boom)^2),
    .groups = "drop"
  )
cli_h2("Per-season skill (positive = model better)")
print(by_season |> mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

write_csv(results, "output/21o_market_edge_results.csv")
write_csv(sens, "output/21o_market_edge_sensitivity.csv")
write_csv(by_season, "output/21o_market_edge_by_season.csv")

cli_h1("21o complete -- fp1 re-test verdicts above, graded against D25's locked bars")

# ======================================== side-by-side vs D25 (18a) -----
cli_h2("Side-by-side vs D25 (two-stage) where 18a's output exists")
d25_path <- "output/18a_market_edge_results.csv"
if (file.exists(d25_path)) {
  d25 <- read_csv(d25_path, show_col_types = FALSE) |>
    select(cell, d25_skill = skill, d25_rel = rel_improve, d25_verdict = verdict)
  cmp <- results |> select(cell, fp1_skill = skill, fp1_rel = rel_improve, fp1_verdict = verdict) |>
    left_join(d25, by = "cell")
  print(cmp |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(),
        row.names = FALSE)
} else {
  cli_alert_warning("{d25_path} not found -- run R/18a_market_edge_backtest.R first for a side-by-side")
}
