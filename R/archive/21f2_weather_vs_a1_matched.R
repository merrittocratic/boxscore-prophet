# R/21f2_weather_vs_a1_matched.R
# Arm A5 (weather): the matched-window comparison the plan specifically
# calls out as a risk area -- "weather is 2021+; comparing A1-full to
# A5-2021+ is the easiest way to manufacture fake lift." Both A1 and A5
# are restricted to the SAME 2021-2025 in-band population here, enforced
# by a stopifnot, not left to discipline. Reports three things on that
# identical matched population: A1 vs ECR, A5 vs ECR, and A1 vs A5 head-
# to-head (the number that actually answers "does weather help").
#
# Usage: Rscript R/21f2_weather_vs_a1_matched.R <RB|WR>

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})
source("R/21a_discrimination_fns.R")

args     <- commandArgs(trailingOnly = TRUE)
POSITION <- if (length(args) >= 1) toupper(args[1]) else cli_abort("Usage: Rscript R/21f2_weather_vs_a1_matched.R <RB|WR>")
stopifnot(POSITION %in% c("RB", "WR"))

THRESH <- list(RB = c(start = 15, boom = 20), WR = c(start = 15, boom = 20))
MATCHED_SEASONS <- 2021:2025   # the weather archive's own coverage window -- not a choice, a constraint
th <- THRESH[[POSITION]]

cli_h1("21f2: A5 (weather) vs A1, matched window {min(MATCHED_SEASONS)}-{max(MATCHED_SEASONS)} -- {POSITION}")

a1 <- read_csv(sprintf("output/21d_%s_base_fold_predictions.csv", tolower(POSITION)), show_col_types = FALSE) |>
  mutate(player_id = as.character(player_id)) |>
  select(season, week, player_id, pred_fp_a1 = pred_fp)
a5 <- read_csv(sprintf("output/21d_%s_weather_fold_predictions.csv", tolower(POSITION)), show_col_types = FALSE) |>
  mutate(player_id = as.character(player_id)) |>
  select(season, week, player_id, pred_fp_weather = pred_fp)

band_d <- band_universe(POSITION, MATCHED_SEASONS, th["start"], th["boom"]) |>
  mutate(ecr_score = -pos_rank)

paired <- band_d |>
  inner_join(a1, by = c("season", "week", "gsis_id" = "player_id")) |>
  inner_join(a5, by = c("season", "week", "gsis_id" = "player_id"))

# The actual matched-window guarantee -- not discipline, an assertion.
stopifnot(
  "A1/A5 paired population must be restricted to the weather-covered window" =
    all(paired$season %in% MATCHED_SEASONS)
)
cli_alert_info("Matched in-band population: {nrow(paired)} rows / {n_distinct(paste(paired$season, paired$week))} season-weeks (band universe had {nrow(band_d)} rows)")

if (nrow(paired) < 30) cli_abort("Too few paired in-band rows ({nrow(paired)}) to grade.")

run_stats <- function(score_a, score_b, label) {
  bind_rows(
    disc_compare(paired, score_a, score_b, stat = "spearman"),
    disc_compare(paired, score_a, score_b, stat = "concordance"),
    disc_compare(paired, score_a, score_b, stat = "auc")
  ) |> mutate(comparison = label, .before = 1)
}

cmp_a1_vs_ecr <- run_stats("ecr_score", "pred_fp_a1", "a1_vs_ecr")
cmp_a5_vs_ecr <- run_stats("ecr_score", "pred_fp_weather", "weather_vs_ecr")
cmp_a1_vs_a5  <- run_stats("pred_fp_a1", "pred_fp_weather", "weather_vs_a1")

cli_h2("A1 vs ECR (matched window, context)")
print(cmp_a1_vs_ecr |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)
cli_h2("A5/weather vs ECR (matched window, context)")
print(cmp_a5_vs_ecr |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)
cli_h2("A5/weather vs A1 -- the actual question, positive = weather helps")
print(cmp_a1_vs_a5 |> mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)

DELTA_BAR <- c(spearman = 0.02, concordance = 0.01)
verdicts <- cmp_a1_vs_a5 |>
  filter(stat %in% names(DELTA_BAR)) |>
  mutate(
    bar = DELTA_BAR[stat],
    verdict = case_when(
      ci_hi < 0 ~ "FAIL",
      ci_lo > 0 & delta >= bar ~ "PASS",
      .default = "NEUTRAL"
    )
  )

cli_h1("VERDICT -- {POSITION}: does weather add anything on top of A1?")
print(verdicts |> select(position = comparison, stat, n, weeks, stat_a, stat_b, delta, ci_lo, ci_hi, verdict) |>
        mutate(across(where(is.numeric), ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)

dir.create("output", showWarnings = FALSE)
out_path <- sprintf("output/21f2_%s_weather_vs_a1_verdict.csv", tolower(POSITION))
write_csv(bind_rows(cmp_a1_vs_ecr, cmp_a5_vs_ecr, verdicts), out_path)
cli_alert_success("{out_path}")
cli_h1("21f2 complete -- {POSITION}")
