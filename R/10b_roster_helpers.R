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

# ---------------------------------------------------------------------------
# Team-code normalization (added 2026-08-09).
#
# WHY: the 2026 weekly-roster release codes Arizona "AZ" while schedules
# code it "ARI" (2024 and 2025 rosters both used "ARI", so this is new
# upstream drift, not a longstanding quirk). The roster team feeds
# `posteam`, which inner_joins to schedule-derived games_long -- so a code
# mismatch SILENTLY drops every player on that team from the slate. Found
# when all 28 Arizona skill players vanished from a 2026 week 1 build.
# nflreadr::clean_team_abbrs() does not fix this (it returns "AZ").
#
# The alias map handles known variants; the real protection is the caller's
# unmatched-code warning below, which makes any FUTURE drift loud instead
# of silent. A dropped team is a data bug, never a legitimate empty.
TEAM_CODE_ALIASES <- c(
  AZ = "ARI", ARZ = "ARI",
  LAR = "LA",  STL = "LA",
  JAC = "JAX",
  WSH = "WAS", WFT = "WAS",
  SD  = "LAC", OAK = "LV",  LVR = "LV",
  KAN = "KC",  SFO = "SF",  TAM = "TB",
  NWE = "NE",  NOR = "NO",  GNB = "GB"
)

# Map roster team codes onto the schedule's vocabulary. Aliases are applied
# only when the alias TARGET is actually a valid schedule code and the
# original is not -- so this can never rewrite a code the schedule already
# uses (e.g. seasons where "LAR" is itself the schedule code).
normalize_team_codes <- function(x, valid_codes) {
  hit <- !is.na(x) & !(x %in% valid_codes) & (x %in% names(TEAM_CODE_ALIASES))
  mapped <- TEAM_CODE_ALIASES[x[hit]]
  ok <- !is.na(mapped) & mapped %in% valid_codes
  x[which(hit)[ok]] <- mapped[ok]
  x
}

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

  # Validate against the SEASON's team vocabulary, not this week's games:
  # a team on bye is legitimately absent from games_long and must not trip
  # the drift warning. Falls back to the week's codes if schedules are
  # unavailable (offline hindcast).
  valid_codes <- tryCatch({
    s <- nflreadr::load_schedules(target_season) |>
      dplyr::filter(game_type == "REG")
    unique(c(s$home_team, s$away_team))
  }, error = function(e) unique(games_long$posteam))

  combined <- played |>
    full_join(roster_now, by = "player_id") |>
    mutate(
      posteam = coalesce(posteam_now, posteam_hist),
      posteam = normalize_team_codes(posteam, valid_codes),
      source  = case_when(
        !is.na(posteam_hist) ~ "played",
        TRUE                 ~ "roster_cold"
      )
    ) |>
    select(player_id, posteam, source)

  # Loud on unmatched codes. The inner_join below drops these rows; without
  # this warning that drop is invisible (the 2026 "AZ" regression cost a
  # full team's slate rows and produced no diagnostic at all).
  unmatched <- combined |>
    filter(!is.na(posteam), !posteam %in% valid_codes) |>
    count(posteam, sort = TRUE)
  if (nrow(unmatched)) {
    cli::cli_alert_warning(
      "{sum(unmatched$n)} {pos} row(s) on team code(s) not in this week's schedule: {paste0(unmatched$posteam, ' (', unmatched$n, ')', collapse = ', ')} -- these are being DROPPED. Add to TEAM_CODE_ALIASES if this is upstream drift."
    )
  }

  combined <- combined |>
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

# ---------------------------------------------------------------------------
# Vegas slate lines (rung 2, 2026-07-26). Returns one row per slated
# (game_id, posteam) with team_spread + implied_total.
#
# HINDCAST: read the opener sidecar (data/vegas_open_lines.rds) -- the
# exact source the deployed models trained on; the slate Vegas gate then
# asserts identity trivially and honestly.
# FUTURE (live week): current lines from load_schedules at build time --
# the "Tuesday line". Trained-on-opener vs served-on-Tuesday skew is
# bounded above by the open->close movement (spread sd 1.9 pts, 13d0
# receipt) and sized in output/13g; the recal layer absorbs the residue
# (pred-vol seam precedent). Falls back to the sidecar when schedules
# lines are not yet posted.
# ---------------------------------------------------------------------------
vegas_slate_lines <- function(games_long, target_season, hindcast) {
  sidecar <- readRDS("data/vegas_open_lines.rds")

  if (hindcast) {
    out <- games_long |>
      select(game_id, posteam) |>
      left_join(sidecar, by = c("game_id", "posteam"))
    cli::cli_alert_info("Vegas lines (hindcast, opener sidecar): {sum(!is.na(out$team_spread))}/{nrow(out)} team-games")
    return(out)
  }

  sched <- nflreadr::load_schedules(target_season) |>
    dplyr::filter(game_type == "REG")
  live <- dplyr::bind_rows(
    sched |> dplyr::transmute(game_id, posteam = home_team,
                              team_spread =  spread_line, total_line),
    sched |> dplyr::transmute(game_id, posteam = away_team,
                              team_spread = -spread_line, total_line)
  ) |>
    dplyr::mutate(implied_total = (total_line + team_spread) / 2) |>
    dplyr::select(game_id, posteam, team_spread, implied_total)

  out <- games_long |>
    select(game_id, posteam) |>
    left_join(live, by = c("game_id", "posteam")) |>
    left_join(sidecar, by = c("game_id", "posteam"), suffix = c("", "_sidecar")) |>
    mutate(
      team_spread   = coalesce(team_spread, team_spread_sidecar),
      implied_total = coalesce(implied_total, implied_total_sidecar)
    ) |>
    select(game_id, posteam, team_spread, implied_total)
  n_na <- sum(is.na(out$team_spread))
  cli::cli_alert_info("Vegas lines (live, schedules-at-build): {nrow(out) - n_na}/{nrow(out)} team-games{if (n_na > 0) cli::format_inline(' ({n_na} unposted -> NA features)') else ''}")
  out
}

# Pre-season loader guard (2026 rollover): nflverse per-season loaders
# (snap_counts, injuries) hard-error via `seasons <= most_recent_season()`
# until the target season is served, and can still fail before the release
# asset exists. Return zero rows with the PRIOR season's schema so
# downstream joins stay shape-stable -- a future-week slate has no
# target-season rows anyway (cold-start / no-designation semantics).
load_season_or_empty <- function(loader, season) {
  tryCatch(loader(seasons = season),
           error = function(e) loader(seasons = season - 1L) |> dplyr::filter(FALSE))
}
