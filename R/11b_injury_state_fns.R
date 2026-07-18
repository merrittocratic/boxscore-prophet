# R/11b_injury_state_fns.R
# Shared core for the rung-1 injury state machine. Sourced by BOTH:
#   - R/11b_injury_state_layer.R (historical materialization -> training)
#   - R/10b2_player_slate.R      (ex-ante slate-week computation -> scoring)
# One implementation, two call sites -- the 10b2 gate proves they agree by
# exact match on hindcast weeks. Semantics documented in 11b's header.

`%||%` <- function(a, b) if (is.null(a)) b else a

INJ_GAP_CAP <- 8L

INJ_PRACTICE_MAP <- c(
  "Did Not Participate In Practice"   = 0L,
  "Limited Participation in Practice" = 1L,
  "Full Participation in Practice"    = 2L,
  "Out (Definitely Will Not Play)"    = 0L
)

# Lock timestamps per team-week: Wednesday 23:59 ET before Thursday games,
# Friday 23:59 ET otherwise.
build_lock_table <- function(seasons) {
  nflreadr::load_schedules(seasons) |>
    dplyr::filter(game_type == "REG") |>
    dplyr::select(season, week, gameday, weekday, home_team, away_team) |>
    tidyr::pivot_longer(c(home_team, away_team), values_to = "team") |>
    dplyr::mutate(
      gd = as.Date(gameday),
      lock_date = dplyr::case_when(
        weekday == "Thursday" ~ gd - 1,
        weekday == "Saturday" ~ gd - 1,
        weekday == "Sunday"   ~ gd - 2,
        weekday == "Monday"   ~ gd - 3,
        .default              = gd - 2
      ),
      lock_ts = as.POSIXct(paste(lock_date, "23:59:59"), tz = "America/New_York")
    ) |>
    dplyr::select(season, week, team, lock_ts)
}

# Clean weekly reports -> one row per player-week with standardized fields.
# mask = TRUE applies the Friday-lock mask (training / hindcast gates);
# mask = FALSE is the LIVE path -- a real-time pull is pre-lock by nature.
clean_injury_reports <- function(inj_raw, lock_table = NULL, mask = TRUE) {
  if (!"date_modified" %in% names(inj_raw)) {   # absent in 2025-only loads
    inj_raw$date_modified <- as.POSIXct(NA)
  }
  inj <- inj_raw |>
    dplyr::filter(game_type == "REG", !is.na(gsis_id)) |>
    dplyr::mutate(
      practice_int = INJ_PRACTICE_MAP[practice_status],
      report_std   = dplyr::case_when(
        report_status %in% c("Out")          ~ "out",
        report_status %in% c("Doubtful")     ~ "doubtful",
        report_status %in% c("Questionable") ~ "questionable",
        report_status %in% c("Probable")     ~ "probable",
        .default                             = NA_character_
      )
    ) |>
    dplyr::group_by(season, week, gsis_id) |>
    dplyr::arrange(dplyr::desc(date_modified), .by_group = TRUE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  if (mask) {
    stopifnot(!is.null(lock_table))
    inj <- inj |>
      dplyr::left_join(lock_table, by = c("season", "week", "team")) |>
      dplyr::mutate(
        post_lock = !is.na(date_modified) & !is.na(lock_ts) &
          date_modified > lock_ts,
        report_std = dplyr::if_else(post_lock, NA_character_, report_std)
      )
  }

  inj |> dplyr::select(player_id = gsis_id, season, week, report_std, practice_int)
}

# Core state computation. base: one row per player-week with columns
# player_id, season, week, posteam, and (if above = TRUE) share = the
# position's ex-ante rolling usage share. Rows for ALL relevant weeks must
# be present (the target week's above-features look back at W-1/W-2 rows).
# Returns base keys + the seven injury feature columns.
build_injury_states <- function(base, inj_slim, above = TRUE) {
  played <- base |> dplyr::distinct(player_id, season, week)

  own <- base |>
    dplyr::left_join(inj_slim, by = c("player_id", "season", "week")) |>
    dplyr::group_by(player_id, season) |>
    dplyr::arrange(week, .by_group = TRUE) |>
    dplyr::mutate(prev_week = dplyr::lag(week)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      own_q_int           = as.integer(!is.na(report_std) & report_std == "questionable"),
      own_practice_int    = dplyr::coalesce(practice_int, 3L),
      weeks_missed        = pmin(dplyr::coalesce(week - prev_week - 1L, 0L), INJ_GAP_CAP),
      return_from_absence = as.integer(weeks_missed >= 1L)
    )

  out <- own |> dplyr::select(player_id, season, week,
                              own_q_int, own_practice_int,
                              weeks_missed, return_from_absence)
  if (!above) return(out)

  inj_flags <- inj_slim |>
    dplyr::transmute(season, week, tm_id = player_id,
      tm_out  = !is.na(report_std) & report_std %in% c("out", "doubtful"),
      tm_q    = !is.na(report_std) & report_std == "questionable",
      tm_return_signal = !is.na(practice_int) & practice_int >= 1L)

  played_w <- played |> dplyr::transmute(season, week, tm_id = player_id, flag = TRUE)

  roster_w1 <- base |>
    dplyr::select(season, week, posteam, tm_id = player_id, tm_share = share) |>
    dplyr::mutate(week = week + 1L)
  new_out <- base |>
    dplyr::inner_join(roster_w1, by = c("season", "week", "posteam"),
                      relationship = "many-to-many") |>
    dplyr::filter(tm_id != player_id, !is.na(tm_share), !is.na(share),
                  tm_share > share) |>
    dplyr::left_join(inj_flags, by = c("season", "week", "tm_id")) |>
    dplyr::group_by(player_id, season, week) |>
    dplyr::summarise(
      above_new_out_share = sum(tm_share[dplyr::coalesce(tm_out, FALSE)]),
      above_q_share       = sum(tm_share[dplyr::coalesce(tm_q,  FALSE)]),
      .groups = "drop"
    )

  roster_w2 <- base |>
    dplyr::select(season, week, posteam, tm_id = player_id, tm_share = share) |>
    dplyr::mutate(week = week + 2L)
  played_w1 <- played |> dplyr::transmute(season, week = week + 1L,
                                          tm_id = player_id, flag1 = TRUE)
  long_out <- base |>
    dplyr::inner_join(roster_w2, by = c("season", "week", "posteam"),
                      relationship = "many-to-many") |>
    dplyr::filter(tm_id != player_id, !is.na(tm_share), !is.na(share),
                  tm_share > share) |>
    dplyr::left_join(played_w1, by = c("season", "week", "tm_id")) |>
    dplyr::left_join(inj_flags, by = c("season", "week", "tm_id")) |>
    dplyr::filter(is.na(flag1)) |>
    dplyr::mutate(still_absent = dplyr::coalesce(tm_out, FALSE) |
                    !dplyr::coalesce(tm_return_signal, FALSE)) |>
    dplyr::group_by(player_id, season, week) |>
    dplyr::summarise(above_long_out_share = sum(tm_share[still_absent]),
                     .groups = "drop")

  out |>
    dplyr::left_join(new_out,  by = c("player_id", "season", "week")) |>
    dplyr::left_join(long_out, by = c("player_id", "season", "week")) |>
    dplyr::mutate(dplyr::across(c(above_new_out_share, above_q_share,
                                  above_long_out_share),
                                ~ dplyr::coalesce(.x, 0)))
}
