# D30 descriptive count (2026-09-27): RB early-exit games 2014-2025 REG.
# Run from repo root. Same-season 3-game baseline; confirmation = next-week
# injury report (unmasked) or 'injured' in pbp desc. See README D30.
suppressMessages({library(dplyr); library(nflreadr); library(stringr)})
SEASONS <- 2014:2025

players <- load_players() |> filter(!is.na(pfr_id), !is.na(gsis_id)) |>
  distinct(pfr_id, .keep_all = TRUE) |> select(pfr_player_id = pfr_id, gsis_id)

snaps <- load_snap_counts(SEASONS) |>
  filter(game_type == "REG", position == "RB") |>
  inner_join(players, by = "pfr_player_id") |>
  select(season, week, game_id, team, gsis_id, player, offense_pct) |>
  arrange(gsis_id, season, week) |>
  group_by(gsis_id, season) |>
  mutate(base = (lag(offense_pct, 1) + lag(offense_pct, 2) + lag(offense_pct, 3)) / 3,
         next_pct = lead(offense_pct), next_week = lead(week)) |>
  ungroup()

cand <- snaps |> filter(!is.na(base), base >= 0.40, offense_pct < 0.5 * base)

inj <- load_injuries(SEASONS) |> filter(game_type == "REG") |>
  distinct(season, week, gsis_id)
nxt <- bind_rows(inj |> mutate(week = week - 1L), inj |> mutate(week = week - 2L)) |>
  distinct() |> mutate(on_next_report = TRUE)
cand <- cand |> left_join(nxt, by = c("season", "week", "gsis_id")) |>
  mutate(on_next_report = coalesce(on_next_report, FALSE))

# pbp injury text: "<abbr name> was injured" / "injured" near the abbr name
pbp <- load_pbp(SEASONS) |> filter(season_type == "REG") |>
  select(game_id, desc, rusher_player_id, rusher_player_name,
         receiver_player_id, receiver_player_name)
names_map <- bind_rows(
  pbp |> filter(!is.na(rusher_player_id)) |> count(gsis_id = rusher_player_id, nm = rusher_player_name),
  pbp |> filter(!is.na(receiver_player_id)) |> count(gsis_id = receiver_player_id, nm = receiver_player_name)) |>
  group_by(gsis_id) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |> select(gsis_id, nm)
inj_desc <- pbp |> filter(str_detect(desc, "injured")) |> select(game_id, desc)
cand <- cand |> left_join(names_map, by = "gsis_id")
pbp_hit <- cand |> filter(!is.na(nm)) |> select(game_id, gsis_id, nm) |>
  inner_join(inj_desc, by = "game_id", relationship = "many-to-many") |>
  filter(str_detect(desc, fixed(paste0(nm, " was injured"))) |
         str_detect(desc, fixed(paste0(nm, " injured")))) |>
  distinct(game_id, gsis_id) |> mutate(pbp_injury = TRUE)
cand <- cand |> left_join(pbp_hit, by = c("game_id", "gsis_id")) |>
  mutate(pbp_injury = coalesce(pbp_injury, FALSE),
         flagged = pbp_injury | on_next_report)

eligible <- snaps |> filter(!is.na(base), base >= 0.40)
cat("\nEstablished-role RB games (base >= 40%):", nrow(eligible), "\n")
cat("Snap collapse (< half of baseline):", nrow(cand), "\n")
print(cand |> count(pbp_injury, on_next_report))
fl <- cand |> filter(flagged)
cat("\nFLAGGED early exits:", nrow(fl), " = ", round(100*nrow(fl)/nrow(eligible),2), "% of eligible games;",
    round(nrow(fl)/length(SEASONS),1), "per season\n")
print(fl |> count(season), n = Inf)
cat("\nNext-game snap share vs baseline (flagged, played next game within 2 wks):\n")
nx <- fl |> filter(!is.na(next_pct), next_week - week <= 2)
print(summary(nx$next_pct / nx$base))
cat("n =", nrow(nx), "| share of flagged who missed the next game or more:",
    round(100*mean(is.na(fl$next_pct) | fl$next_week - fl$week > 1),1), "%\n")
cat("\nUnflagged collapses (benching/game script candidates):", sum(!cand$flagged), "\n")
cat("\nSanity: Barkley 2026 W2 not in range; 2025 examples:\n")
print(fl |> filter(season == 2025) |> select(week, player, team, base, offense_pct, next_pct, pbp_injury, on_next_report) |> head(12))
