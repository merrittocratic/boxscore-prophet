# R/oneoff/16a_rb_r1_per_player.R
#
# Per-player drill-down of 16a's Cell 2 (shipped-probability honesty check),
# filtered to RB / first-round rookies. 16a's cell2 (R/16a_rookie_prior_diagnostic.R)
# aggregates stated-vs-empirical P(15+ PPR) up to cohort/tier/phase level; this
# script reruns the same join at player granularity to see whether the
# rookie-cohort miss is uniform across draft slot or concentrated by role
# (bell-cow vs. committee), against the CURRENT shipped model (06c refit on
# the volfix baseline, commit 1ffd67d).
#
# Reproducibility note: this replaces an ad-hoc 2026-08-28 analysis
# (rb_r1_rookie_calibration.csv, since deleted) whose numbers were recorded
# only in memory, not in a saved script. This version is meant to be re-run,
# not re-typed.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

MIN_GAMES <- 4  # exclude players with too few weeks to read a rate off of

cli_h1("16a RB-R1 per-player: stated vs empirical P(15+ PPR)")

rookie_key <- nflreadr::load_players() |>
  transmute(player_id = gsis_id, player_name = display_name,
            rookie_season, draft_round)

cal_rb <- readr::read_csv("output/06c_recal_probabilities.csv", show_col_types = FALSE) |>
  filter(position == "RB") |>
  transmute(player_id, season, week,
            p_start = p_start_platt_vegas, hit_start) |>
  left_join(rookie_key, by = "player_id") |>
  filter(!is.na(rookie_season), season == rookie_season, draft_round == 1)

per_player <- cal_rb |>
  group_by(player_id, player_name, rookie_season) |>
  summarise(n_weeks = n(),
            stated = mean(p_start),
            emp = mean(as.numeric(hit_start)),
            .groups = "drop") |>
  mutate(delta_pp = 100 * (emp - stated)) |>
  arrange(desc(delta_pp))

cli_h2("All first-round rookie RB player-seasons in the shipped model's eval window")
print(per_player |>
        mutate(stated = round(stated, 3), emp = round(emp, 3), delta_pp = round(delta_pp, 1)) |>
        as.data.frame(), row.names = FALSE)

below_floor <- per_player |> filter(n_weeks < MIN_GAMES)
if (nrow(below_floor) > 0) {
  cli_alert_warning("{nrow(below_floor)} player-season{?s} below the {MIN_GAMES}-week floor (excluded from the chart, listed for transparency):")
  print(below_floor |> select(player_name, rookie_season, n_weeks) |> as.data.frame(), row.names = FALSE)
}

readr::write_csv(per_player, "output/16a_rb_r1_per_player.csv")
cli_alert_success("output/16a_rb_r1_per_player.csv ({nrow(per_player)} player-seasons)")
cli_h1("16a RB-R1 per-player complete")
