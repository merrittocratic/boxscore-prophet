# R/14a0_weather_fetch.R
# Ablation ladder rung 3, step 0a: bulk kickoff-hour weather for the
# diagnostic window. Reuses the 10b_weekly_slate fetch discipline verbatim:
# HISTORICAL FORECAST archive (forecast-as-of-lock, never ERA5/observed),
# stadium coords from data/stadium_coords.csv, hourly series requested in
# America/New_York and indexed by the ET kickoff hour.
#
# WINDOW: 2021-2025 REG (the Open-Meteo historical FORECAST archive starts
# 2021 -- the pre-registered constraint; 5 seasons, not 12).
# ROOF: schedule roof authoritative, coords roof_static fallback (10b
# logic); indoor games fetched for the record but flagged is_indoor.
#
# CACHED + RESUMABLE: data/weather_forecast_hist.rds; re-runs fetch only
# missing game_ids (partial saves every 50 games). ~1,360 calls at ~0.5s
# with the polite sleep -> one ~12-minute run, then never again.
# Attribution: weather data by Open-Meteo (CC-BY 4.0).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

SEASONS <- 2021L:2025L
OUT_RDS <- "data/weather_forecast_hist.rds"

cli_h1("14a0: bulk kickoff-hour weather fetch ({min(SEASONS)}-{max(SEASONS)})")

coords <- readr::read_csv("data/stadium_coords.csv", show_col_types = FALSE)

games <- nflreadr::load_schedules(SEASONS) |>
  filter(game_type == "REG") |>
  select(game_id, season, week, gameday, gametime, home_team, away_team,
         roof, stadium) |>
  mutate(gametime = coalesce(gametime, "13:00")) |>
  left_join(coords, by = c("home_team" = "team")) |>
  mutate(
    roof_eff  = coalesce(na_if(roof, ""), roof_static),
    is_indoor = roof_eff %in% c("dome", "closed")
  )

cli_alert_info("{nrow(games)} games | {sum(games$is_indoor)} indoor | {sum(is.na(games$lat))} without coords")

done <- if (file.exists(OUT_RDS)) readRDS(OUT_RDS) else tibble()
todo <- games |> filter(!game_id %in% done$game_id)
cli_alert_info("Cached: {nrow(done)} | to fetch: {nrow(todo)}")

if (nrow(todo) > 0) {
  # fetch_weather: identical mechanics to 10b_weekly_slate (all dates past
  # here, so the historical-forecast archive path always applies)
  fetch_weather <- function(lat, lon, gameday, gametime) {
    kick_hour <- as.integer(substr(gametime, 1, 2))
    url <- paste0(
      "https://historical-forecast-api.open-meteo.com/v1/forecast",
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
        weather_src = "historical_forecast"
      )
    }, error = function(e) {
      tibble(temp_c = NA_real_, wind_kmh = NA_real_, gust_kmh = NA_real_,
             precip_mm = NA_real_, weather_src = paste0("FETCH_FAILED: ", conditionMessage(e)))
    })
    Sys.sleep(0.2)
    out
  }

  acc <- done
  for (i in seq_len(nrow(todo))) {
    g <- todo[i, ]
    wx <- if (is.na(g$lat)) {
      tibble(temp_c = NA_real_, wind_kmh = NA_real_, gust_kmh = NA_real_,
             precip_mm = NA_real_, weather_src = "NO_COORDS")
    } else {
      fetch_weather(g$lat, g$lon, g$gameday, g$gametime)
    }
    acc <- bind_rows(acc, bind_cols(g |> select(game_id, season, week, home_team,
                                                roof_eff, is_indoor), wx))
    if (i %% 50 == 0) {
      saveRDS(acc, OUT_RDS)
      cli_alert_info("{i}/{nrow(todo)} fetched (checkpoint saved)")
    }
  }
  saveRDS(acc, OUT_RDS)
  done <- acc
}

n_fail <- sum(startsWith(done$weather_src, "FETCH_FAILED"))
n_ok   <- sum(done$weather_src == "historical_forecast" & !is.na(done$wind_kmh))
cli_alert_success("{OUT_RDS}: {nrow(done)} games | {n_ok} with weather | {n_fail} failed")
if (n_fail > nrow(done) * 0.02) cli_abort("Fetch failure rate above 2% -- rerun (resumable) before the diagnostic.")

cli_h1("14a0 complete")
