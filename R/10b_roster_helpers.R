# R/10b_roster_helpers.R
# Shared ex-ante roster construction for the 10b slate builders.
#
# The ex-ante roster for a target week is the UNION of:
#   (a) players with a played game this season (rolling features exist);
#       team = current roster team when available (trade handling),
#       else most recent posteam
#   (b) players on the POINT-IN-TIME weekly roster at the position with
#       active status but no played game yet (rookies, new signings,
#       week 1) -- these get cold-start rows: rolling features NA,
#       baselines from the prior/tier machinery
# restricted to teams playing in the target week, with the depth-chart
# override hook (data/overrides/depth_overrides.csv) applied last.
#
# Weekly rosters (load_rosters_weekly) are historically archived, so the
# same construction is point-in-time honest in hindcast validation. For a
# future week beyond the archive, the latest available week is the
# current roster -- which is exactly the as-of-lock state.

# known_ids: player_ids that belong to this position under the FROZEN
# layer's definition ("ever rostered at pos, any season") -- catches
# position converts (e.g. a WR-table player currently listed TE) whose
# current-week roster position no longer matches.
build_exante_roster <- function(pos, target_season, target_week,
                                games_long, season_hist,
                                known_ids = character(),
                                overrides_file = "data/overrides/depth_overrides.csv") {

  played <- season_hist |>
    arrange(player_id, week) |>
    group_by(player_id) |>
    summarise(posteam_hist = last(posteam), .groups = "drop")

  wk_rosters <- tryCatch(
    nflreadr::load_rosters_weekly(seasons = target_season),
    error = function(e) NULL
  )

  roster_now <- NULL
  if (!is.null(wk_rosters) && nrow(wk_rosters)) {
    wks <- sort(unique(wk_rosters$week))
    use_wk <- if (any(wks <= target_week)) max(wks[wks <= target_week]) else min(wks)
    roster_now <- wk_rosters |>
      filter(week == use_wk, position == pos | gsis_id %in% known_ids,
             !is.na(gsis_id), status %in% c("ACT", "A01")) |>
      distinct(gsis_id, .keep_all = TRUE) |>
      select(player_id = gsis_id, posteam_now = team)
    cli::cli_alert_info("Point-in-time roster: {target_season} week {use_wk} ({nrow(roster_now)} active {pos}s)")
  } else {
    # Season-level fallback (e.g. weekly archive unavailable offline)
    roster_now <- tryCatch(
      nflreadr::load_rosters(seasons = target_season) |>
        filter(position == pos | gsis_id %in% known_ids, !is.na(gsis_id)) |>
        distinct(gsis_id, .keep_all = TRUE) |>
        select(player_id = gsis_id, posteam_now = team),
      error = function(e) tibble::tibble(player_id = character(),
                                         posteam_now = character())
    )
    cli::cli_alert_warning("Weekly rosters unavailable; season-level roster fallback ({nrow(roster_now)} {pos}s)")
  }

  combined <- played |>
    full_join(roster_now, by = "player_id") |>
    mutate(
      posteam = coalesce(posteam_now, posteam_hist),
      source  = case_when(
        !is.na(posteam_hist) ~ "played",
        TRUE                 ~ "roster_cold"
      )
    ) |>
    select(player_id, posteam, source) |>
    inner_join(games_long, by = "posteam")

  overrides <- if (file.exists(overrides_file)) {
    readr::read_csv(overrides_file, show_col_types = FALSE)
  } else tibble::tibble(player_id = character(), action = character(), note = character())
  if (nrow(overrides)) {
    cli::cli_alert_info("Applying {nrow(overrides)} depth-chart override(s)")
    combined <- combined |>
      filter(!player_id %in% overrides$player_id[overrides$action == "drop"])
    adds <- overrides |> filter(action == "add") |>
      inner_join(games_long, by = "posteam") |>
      mutate(source = "override")
    combined <- bind_rows(combined, adds |> select(any_of(names(combined)))) |>
      distinct(player_id, .keep_all = TRUE)
  }

  combined |> filter(!is.na(player_id))
}
