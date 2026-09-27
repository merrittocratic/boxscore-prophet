# D30 Step 0 (pre-registered 2026-09-27, see README D30). Run from repo root.
# Does the RB model underproject touches after an early injury exit?
# Stop bar: |pooled net bias| < 1 touch. Graded on both the live twostage
# RB vol model and the fp1 shadow (21d base), 204-fold walk-forward.
suppressMessages({library(dplyr); library(nflreadr); library(purrr)})
source("R/11b_injury_state_fns.R")
SEASONS <- 2013:2025          # 2013 only feeds cross-season baselines

players <- load_players() |> filter(!is.na(pfr_id), !is.na(gsis_id)) |>
  distinct(pfr_id, .keep_all = TRUE) |> select(pfr_player_id = pfr_id, gsis_id)

snaps <- load_snap_counts(SEASONS) |>
  filter(game_type == "REG", position == "RB") |>
  inner_join(players, by = "pfr_player_id") |>
  distinct(gsis_id, season, week, .keep_all = TRUE) |>
  arrange(gsis_id, season, week) |>
  group_by(gsis_id) |>                       # baseline may span seasons
  mutate(g = row_number(),
         base = (lag(offense_pct, 1) + lag(offense_pct, 2) + lag(offense_pct, 3)) / 3,
         nxt_season = lead(season), nxt_week = lead(week)) |>
  ungroup()

# Friday-lock-masked injury reports: "on report" = any row for that week
inj_raw <- map(SEASONS, \(s) tryCatch(load_injuries(s), error = \(e) NULL)) |> compact() |> list_rbind()
locks <- build_lock_table(SEASONS)
inj <- clean_injury_reports(inj_raw, locks, mask = TRUE) |>
  filter(!is.na(report_std) | !is.na(practice_int)) |>
  distinct(season, week, gsis_id = player_id) |> mutate(on_report = TRUE)

exits <- snaps |>
  filter(!is.na(base), base >= 0.40, offense_pct < 0.5 * base, !is.na(nxt_week)) |>
  left_join(inj, by = c("gsis_id", "nxt_season" = "season", "nxt_week" = "week")) |>
  filter(coalesce(on_report, FALSE)) |>
  select(gsis_id, exit_g = g, exit_season = season, exit_week = week)
cat("Flagged early exits (cross-season baseline, report-confirmed, lock-masked):",
    nrow(exits), "\n")

# post-exit rows = next 4 games played, excluding games that are themselves exits
post <- exits |>
  inner_join(snaps |> select(gsis_id, g, season, week), by = "gsis_id",
             relationship = "many-to-many") |>
  filter(g > exit_g, g <= exit_g + 4) |>
  mutate(k = g - exit_g) |>
  anti_join(exits, by = c("gsis_id", "season" = "exit_season", "week" = "exit_week")) |>
  group_by(gsis_id, season, week) |> slice_min(k, n = 1, with_ties = FALSE) |> ungroup()


PRED_FILES <- c(twostage_live = "output/11c_rb_injury_fold_predictions_volfix.csv",
                fp1_shadow    = "output/21d_rb_base_fold_predictions.csv")
for (arch in names(PRED_FILES)) {
  d <- readr::read_csv(PRED_FILES[[arch]], show_col_types = FALSE) |>
    mutate(err = pred_vol - opportunities) |>     # negative = underprojected
    left_join(post |> select(player_id = gsis_id, season, week, k),
              by = c("player_id", "season", "week"))
  overall <- mean(d$err)
  cat(sprintf("\n== %s: all rows n=%d, bias %+.2f\n", arch, nrow(d), overall))
  print(d |> filter(!is.na(k)) |> group_by(k) |>
          summarise(n = n(), net_bias = round(mean(err) - overall, 2),
                    se = round(sd(err) / sqrt(n()), 2), .groups = "drop"))
  p <- d |> filter(!is.na(k)) |>
    summarise(n = n(), nb = mean(err) - overall, se = sd(err) / sqrt(n()))
  cat(sprintf("POOLED n=%d net %+.2f (95%% CI %+.2f to %+.2f) -> %s\n",
              p$n, p$nb, p$nb - 1.96 * p$se, p$nb + 1.96 * p$se,
              ifelse(abs(p$nb) >= 1, "PROCEED", "STOP")))
}
