# R/18a_market_edge_backtest.R
# PRE-REGISTERED (D25, bars signed by Steve 2026-09-05 before this script
# was written): does the shipped model carry information beyond FantasyPros
# ECR, measured against point-in-time Wayback-archived consensus
# (data/ecr_history/, built by R/oneoff/ecr_wayback_harvest.R)?
#
# DESIGN (locked before running):
#   Model: shipped recal variants only --
#     QB  20+ p_start_platt_vol_vegas / 25+ p_boom_platt  (09b, 2016-2025)
#     RB  15+ p_start (raw)           / 20+ p_boom (raw)  (06c volfix)
#     WR  15+ p_start (raw)           / 20+ p_boom_iso    (06c volfix)
#     TE  12+ p_start (raw)           / 17+ p_boom (raw)  (12e volfix)
#     All variant columns are walk-forward weekly refits inside their
#     source scripts -- no leakage from using them here.
#   Baseline: walk-forward isotonic map pos_rank -> P(hit), fit per
#     position x threshold on ECR-covered weeks from seasons STRICTLY
#     before the eval season, using ALL ranked+played players (the
#     market gets its best shot, including pre-model years).
#   Outcomes: fantasy_points_ppr >= 15/20 (RB/WR), >= 12/17 (TE);
#     standard fantasy_points >= 20/25 (QB, 4pt pass TD) -- identical to
#     06 / 09a definitions, from nflreadr::load_player_stats, REG only.
#   Universe: model row AND ECR-ranked AND valid timing. Valid timing =
#     ECR captured before THAT PLAYER'S kickoff (falls back to the
#     week's first kickoff when the team can't be matched). The stricter
#     capture-before-week's-first-kick cut is reported as a sensitivity.
#   Cells: PRIMARY QB-start, QB-boom (eval 2018-2025; 2016-17 burn-in).
#     SECONDARY RB/WR/TE pooled start, pooled boom (2023-2025).
#     REPORTED per-position RB/WR/TE.
#   Metric: Brier. skill = brier_ecr - brier_model (positive = model
#     better). Week-clustered bootstrap, B = 2000, seed = 42, percentile
#     95% CI.
#   BARS (locked): PASS = CI excludes 0 AND relative improvement
#     (skill / brier_ecr) >= 2%. FAIL = CI excludes 0 the other way.
#     Otherwise NEUTRAL. A cell with name-match rate < 85% is flagged
#     unreliable, not graded.
#   Sensitivities (reported, non-binding): strict before-first-kick;
#     capture within 3 days of kickoff; scoring-matched source pages
#     only; QB all recal variants; per-season skill.
#
# Usage: Rscript R/18a_market_edge_backtest.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10d_name_helpers.R")

set.seed(42)
B_BOOT <- 2000L

cli_h1("18a: model-vs-ECR market edge backtest (pre-registered D25)")

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

# ============================================================ outcomes --
cli_h2("Outcomes from nflreadr player stats (same defs as 06/09a)")
stats <- load_player_stats(2016:2025) |>
  filter(season_type == "REG", !is.na(player_id)) |>
  select(player_id, season, week, fantasy_points, fantasy_points_ppr)

# =========================================================== crosswalk --
cli_h2("ECR name -> gsis crosswalk")
ascii_norm <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

# Aliases found in the harvest QA (2026-09-05): ECR-side names whose
# normalized form differs from the roster's.
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

# Fallback for WR/TE-style position flips: name unique within season
# across all four positions.
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
# All ranked + played players (not just model rows), per pre-registration.
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

# Monotone-decreasing isotonic in pos_rank, linear interpolation between
# knots, clamped outside the training range.
fit_iso_rank <- function(rank, hit) {
  o <- order(-rank)
  fit <- isoreg(x = (-rank)[o], y = hit[o])
  xs <- fit$x; ys <- fit$yf
  function(newrank) approx(xs, ys, xout = -newrank, rule = 2, ties = mean)$y
}

# Walk-forward: baseline for season S fit on covered weeks of seasons < S.
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

cli_h1("PRE-REGISTERED RESULTS")
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

# QB variant table (variant-selection-optimism check, non-binding)
qb_all <- read_csv("output/09b_qb_recal_probabilities.csv",
                   show_col_types = FALSE)
variant_cols <- grep("^p_(start|boom)_", names(qb_all), value = TRUE)
qb_var <- scored |> filter(position == "QB") |>
  select(-starts_with("p_model_")) |>
  inner_join(qb_all |> select(player_id, season, week, all_of(variant_cols),
                              p_start_raw = p_start, p_boom_raw = p_boom),
             by = c("player_id", "season", "week"))
variants <- map(c(paste0("p_start_", c("raw", "platt", "iso", "strat_platt",
                                       "strat_iso", "platt_vol", "platt_vegas",
                                       "platt_vol_vegas"))), function(v) {
  hit <- qb_var$hit_start
  tibble(variant = v, brier = mean((qb_var[[v]] - hit)^2, na.rm = TRUE))
}) |> list_rbind() |>
  bind_rows(
    map(paste0("p_boom_", c("raw", "platt", "iso", "strat_platt", "strat_iso",
                            "platt_vol", "platt_vegas", "platt_vol_vegas")),
        function(v) {
          hit <- qb_var$hit_boom
          tibble(variant = v, brier = mean((qb_var[[v]] - hit)^2, na.rm = TRUE))
        }) |> list_rbind()
  )
cli_h2("QB all-variant Briers (optimism check)")
print(variants |> mutate(brier = round(brier, 4)) |> as.data.frame(),
      row.names = FALSE)

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

write_csv(results, "output/18a_market_edge_results.csv")
write_csv(sens, "output/18a_market_edge_sensitivity.csv")
write_csv(by_season, "output/18a_market_edge_by_season.csv")
write_csv(variants, "output/18a_qb_variant_briers.csv")

cli_h1("18a complete -- verdicts above are graded against the locked bars")
