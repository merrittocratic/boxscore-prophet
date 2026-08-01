# R/10b_weekly_slate.R
# Step 10b: Weekly slate builder -- game context + weather (PART 1 of 2).
#
# Builds the game-level slate for a target week: schedule, venue, roof,
# and kickoff-hour weather at stadium coordinates. Weather is CONTENT
# plumbing for now (board context, narrative); a modeled weather
# adjustment layer is pre-registered in building_in_public_log.md and
# earns its way in through the fold harness separately.
#
# WEATHER DISCIPLINE (non-negotiable, same as the golf stack): forecasts
# as of prediction lock, never observed weather.
#   - Future weeks: Open-Meteo forecast API (the live forecast IS the
#     as-of-lock forecast when the runner executes at lock time).
#   - Hindcast/backtest: Open-Meteo HISTORICAL FORECAST archive
#     (archived model runs, ~2021+), NOT ERA5 -- ERA5 is reanalysis,
#     i.e. observed weather wearing a raincoat.
#
# PART 2 (next build session): player slate -- active rosters, ex-ante
# rolling features carried forward from played games, cold starts, the
# pred-volume router, injury practice-report state (load_injuries), and
# Earnest's depth-chart override hook. Player rows join to this game
# slate on team.
#
# Usage: Rscript R/10b_weekly_slate.R [season] [week]
#   Defaults to a 2025-W15 hindcast (offseason smoke test).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(jsonlite)
  library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L

WIND_FLAG_KMH <- 24   # ~15 mph sustained: content flag threshold
COLD_FLAG_C   <- -7   # ~20F: content flag threshold

cli_h1("Step 10b (part 1): game slate + weather -- {TARGET_SEASON} week {TARGET_WEEK}")

# ===========================================================================
# 1. SCHEDULE + VENUE
# ===========================================================================

cli_h1("Step 1: Schedule and venues")

sched <- nflreadr::load_schedules(seasons = TARGET_SEASON) |>
  filter(game_type == "REG", week == TARGET_WEEK) |>
  select(game_id, season, week, gameday, weekday, gametime,
         away_team, home_team, location, roof, stadium)

if (!nrow(sched)) cli_abort("No games found for {TARGET_SEASON} week {TARGET_WEEK}")

coords <- readr::read_csv("data/stadium_coords.csv", show_col_types = FALSE)

slate <- sched |>
  left_join(coords, by = c("home_team" = "team")) |>
  mutate(
    # Schedule roof is authoritative when present (covers international
    # venues and retractable open/closed calls); static flag is fallback
    roof_eff  = coalesce(na_if(roof, ""), roof_static),
    is_indoor = roof_eff %in% c("dome", "closed"),
    is_neutral = !is.na(location) & location != "Home"
  )

if (any(slate$is_neutral)) {
  cli_alert_warning(
    "{sum(slate$is_neutral)} neutral-site game(s): using home-team stadium coords as placeholder -- add venue override if international"
  )
}
cli_alert_success("{nrow(slate)} games | {sum(slate$is_indoor)} indoor | {sum(!slate$is_indoor)} outdoor")

# ===========================================================================
# 2. KICKOFF-HOUR WEATHER (Open-Meteo, forecast-at-lock discipline)
# ===========================================================================

cli_h1("Step 2: Kickoff-hour weather at stadium coordinates")

# gametime is US/Eastern. Request the hourly series in America/New_York and
# index by the ET kickoff hour -- venue-local conversion is unnecessary
# when the request timezone matches the index timezone.
fetch_weather <- function(lat, lon, gameday, gametime) {
  kick_hour <- as.integer(substr(gametime, 1, 2))
  is_past   <- as.Date(gameday) < Sys.Date()
  base <- if (is_past) {
    "https://historical-forecast-api.open-meteo.com/v1/forecast"
  } else {
    "https://api.open-meteo.com/v1/forecast"
  }
  url <- paste0(
    base,
    "?latitude=", lat, "&longitude=", lon,
    "&hourly=temperature_2m,wind_speed_10m,wind_gusts_10m,precipitation",
    "&timezone=America%2FNew_York",
    "&start_date=", gameday, "&end_date=", gameday
  )
  out <- tryCatch({
    j   <- jsonlite::fromJSON(url)
    idx <- which(j$hourly$time == paste0(gameday, "T", sprintf("%02d:00", kick_hour)))
    if (!length(idx)) idx <- kick_hour + 1L
    tibble(
      temp_c      = j$hourly$temperature_2m[idx],
      wind_kmh    = j$hourly$wind_speed_10m[idx],
      gust_kmh    = j$hourly$wind_gusts_10m[idx],
      precip_mm   = j$hourly$precipitation[idx],
      weather_src = if (is_past) "historical_forecast" else "forecast"
    )
  }, error = function(e) {
    tibble(temp_c = NA_real_, wind_kmh = NA_real_, gust_kmh = NA_real_,
           precip_mm = NA_real_, weather_src = paste0("FETCH_FAILED: ", conditionMessage(e)))
  })
  Sys.sleep(0.2)   # be polite to the free API
  out
}

weather <- slate |>
  mutate(wx = pmap(list(lat, lon, gameday, gametime), function(lat, lon, gameday, gametime) {
    if (is.na(lat)) return(tibble(temp_c = NA_real_, wind_kmh = NA_real_,
                                  gust_kmh = NA_real_, precip_mm = NA_real_,
                                  weather_src = "NO_COORDS"))
    fetch_weather(lat, lon, gameday, gametime)
  })) |>
  unnest(wx) |>
  mutate(
    # Indoor games: weather fetched for the record, flags suppressed
    flag_wind = !is_indoor & !is.na(wind_kmh) & wind_kmh >= WIND_FLAG_KMH,
    flag_cold = !is_indoor & !is.na(temp_c)   & temp_c   <= COLD_FLAG_C,
    temp_f    = round(temp_c * 9 / 5 + 32),
    wind_mph  = round(wind_kmh / 1.609),
    gust_mph  = round(gust_kmh / 1.609)
  )

n_fail <- sum(startsWith(weather$weather_src, "FETCH_FAILED"))
if (n_fail) cli_alert_warning("{n_fail} weather fetches failed (see weather_src)")

cli_h2("Slate with weather (outdoor flags: wind >= {WIND_FLAG_KMH}km/h, temp <= {COLD_FLAG_C}C)")
print(weather |>
        mutate(matchup = paste(away_team, "@", home_team)) |>
        select(matchup, gameday, gametime, roof_eff, temp_f, wind_mph,
               gust_mph, precip_mm, flag_wind, flag_cold),
      n = Inf)

# ===========================================================================
# 3. SAVE
# ===========================================================================

cli_h1("Step 3: Save")

out_file <- sprintf("output/10b_game_slate_%d_w%02d.csv", TARGET_SEASON, TARGET_WEEK)
readr::write_csv(weather |> select(-roof_static), out_file)
cli_alert_success("{out_file} ({nrow(weather)} games)")

flagged <- weather |> filter(flag_wind | flag_cold)
if (nrow(flagged)) {
  cli_h2("Content flags")
  for (i in seq_len(nrow(flagged))) {
    g <- flagged[i, ]
    cli_alert_info(
      "{g$away_team} @ {g$home_team}: {g$temp_f}F, wind {g$wind_mph} mph (gust {g$gust_mph})"
    )
  }
} else {
  cli_alert_info("No weather flags this slate")
}

cli_h1("Step 10b (part 1) complete -- player slate + features are part 2")
