# R/21c2_weather_join.R
# Stage E, arm A5: join game-level weather (R/archive/14a0's output,
# 2021-2025 only -- the Open-Meteo historical-forecast archive's own
# coverage limit) onto the plain fp_train_<pos>.rds tables by game_id.
# Pre-2021 rows get genuine NA on every weather column (LightGBM's native
# missing-value handling, not a 0-fill) -- this is NOT the same population
# problem as A2/A3's crosswalk gaps (a real week with an unmeasured stat);
# it's a real absence of the underlying weather archive before 2021, and
# grading MUST restrict to the matched 2021-2025 window on both A1 and A5
# sides (R/21f2), never compare full-window A1 to partial-coverage A5.
#
# is_indoor is cast to integer (LightGBM has no native logical type).
#
# Usage: Rscript R/21c2_weather_join.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

cli_h1("21c2: join weather onto fp_train tables (arm A5)")

WEATHER_COLS <- c("temp_c", "wind_kmh", "gust_kmh", "precip_mm", "is_indoor")

weather <- readRDS("data/weather_forecast_hist.rds") |>
  mutate(is_indoor = as.integer(is_indoor)) |>
  select(game_id, all_of(WEATHER_COLS))

join_weather <- function(position) {
  ft <- readRDS(sprintf("data/fp_train_%s.rds", tolower(position)))
  out <- ft |> left_join(weather, by = "game_id", relationship = "many-to-one")
  matched <- sum(!is.na(out$temp_c))
  cli_alert_success(
    "{position}: {nrow(ft)} fp_train rows -> {matched} matched a weather row ({round(100 * matched / nrow(ft), 1)}%), rest genuinely NA (pre-2021 or unmatched)"
  )
  out_path <- sprintf("data/fp_train_%s_weather.rds", tolower(position))
  saveRDS(out, out_path)
  cli_alert_success("{out_path} written")
}

join_weather("RB")
join_weather("WR")

cli_h1("21c2 complete -- data/fp_train_<pos>_weather.rds written for RB/WR")
