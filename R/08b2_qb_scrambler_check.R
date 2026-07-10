# R/08b2_qb_scrambler_check.R
# Step 8b2: Is the ex-ante scrambler bias (+0.53 EPA, n=227, t=0.73 in 08b)
# real mispricing masked by small n, or sampling noise? Four higher-power
# tests, pre-stated; run BEFORE committing to 08c (Steve's review call,
# 2026-07-10). Feature lists stay frozen regardless -- this is diagnosis only.
#
# Q1. CONTINUOUS: regress fold-test residual on ex-ante rush identity
#     (wt_carries, prior_carries_pg) over ALL rows -- full-sample power
#     instead of a 227-row bin. Total + pass/rush components.
# Q2. PER-PLAYER: mean residual per QB for established runners (career
#     mean carries >= 5, >= 20 starter games) vs pocket QBs. Real mispricing
#     = famous runners cluster positive; noise = scatter around zero.
# Q3. TAILS: one-sided exceedance above hi_80/hi_90 (const mechanism) for
#     ex-ante runner rows (wt_carries >= 6) vs statues. Boom relevance:
#     content cares about P(FP >= 20/25), i.e. the upper tail.
# Q4. ERA: runner-group bias 2014-2018 vs 2019-2025 (mobile-QB era) --
#     a real effect should be era-stable or growing.
#
# EXPECTED (stated before run): slopes |t| < 2, runner residuals scattered
# around zero, tails near nominal, no era trend. If runners cluster positive,
# the +0.53 is real -> revisit the 08c design before building.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

MIN_GAMES_PLAYER <- 20L
RUNNER_CPG       <- 5
ROW_RUNNER_WT    <- 6

cli_h1("Step 8b2: Scrambler mispricing check (diagnosis only)")

fp <- read_csv("output/08b_qb_fold_predictions.csv", show_col_types = FALSE)
ft <- readRDS("data/qb_feature_table.rds") |>
  select(player_id, player_name, season, week, wt_carries, prior_carries_pg)

d <- fp |>
  left_join(ft, by = c("player_id", "season", "week")) |>
  mutate(
    res_tot  = total_epa - pred_tot,
    res_pass = pass_epa  - pred_pass_eff * pred_db,
    res_rush = rush_epa  - pred_rush
  )

cli_alert_success("{nrow(d)} fold-test rows joined ({sum(is.na(d$wt_carries))} week-1 rows lack wt_carries)")

# ===========================================================================
# Q1: CONTINUOUS -- residual ~ ex-ante rush identity (full-sample power)
# ===========================================================================
cli_h1("Q1: Residual vs ex-ante rush identity (continuous, all rows)")

slope_row <- function(df, res_col, x_col) {
  sub <- df |> filter(!is.na(.data[[x_col]]))
  fit <- lm(reformulate(x_col, res_col), data = sub)
  cf  <- summary(fit)$coefficients
  tibble(
    residual  = res_col,
    predictor = x_col,
    n         = nrow(sub),
    slope     = round(cf[2, 1], 4),
    se        = round(cf[2, 2], 4),
    t         = round(cf[2, 3], 2),
    epa_at_10_carries = round(cf[2, 1] * 10, 3)
  )
}

q1 <- bind_rows(
  slope_row(d, "res_tot",  "wt_carries"),
  slope_row(d, "res_pass", "wt_carries"),
  slope_row(d, "res_rush", "wt_carries"),
  slope_row(d, "res_tot",  "prior_carries_pg"),
  slope_row(d, "res_pass", "prior_carries_pg"),
  slope_row(d, "res_rush", "prior_carries_pg")
)
print(as.data.frame(q1))

# ===========================================================================
# Q2: PER-PLAYER -- do established runners cluster positive?
# ===========================================================================
cli_h1("Q2: Per-player mean residual, established runners vs pocket QBs")

by_player <- d |>
  group_by(player_id, player_name) |>
  summarise(
    games    = n(),
    cpg      = mean(carries),
    bias_tot  = mean(res_tot),
    bias_rush = mean(res_rush),
    t_tot     = mean(res_tot) / (sd(res_tot) / sqrt(n())),
    .groups   = "drop"
  ) |>
  filter(games >= MIN_GAMES_PLAYER)

runners <- by_player |> filter(cpg >= RUNNER_CPG) |> arrange(desc(cpg))
pocket  <- by_player |> filter(cpg <  RUNNER_CPG)

cli_h2("Established runners (career cpg >= {RUNNER_CPG}, >= {MIN_GAMES_PLAYER} games)")
print(as.data.frame(
  runners |> mutate(across(c(cpg, bias_tot, bias_rush, t_tot), ~ round(.x, 2)))
), row.names = FALSE)

n_pos <- sum(runners$bias_tot > 0)
cli_alert_info("Runners with positive total bias: {n_pos}/{nrow(runners)} (noise expectation ~50%)")
cli_alert_info("Runner-group mean bias: {round(mean(runners$bias_tot), 3)} EPA | pocket-group: {round(mean(pocket$bias_tot), 3)} EPA")

# Sign test: under fair pricing each runner is +/- with p=0.5
sign_p <- binom.test(n_pos, nrow(runners))$p.value
cli_alert_info("Sign test p-value (runners positive vs 50/50): {round(sign_p, 3)}")

# ===========================================================================
# Q3: TAILS -- upper exceedance for ex-ante runner rows (const intervals)
# ===========================================================================
cli_h1("Q3: One-sided tail exceedance (nominal above-hi: 10% at 80, 5% at 90)")

q3 <- d |>
  filter(!is.na(wt_carries)) |>
  mutate(grp = if_else(wt_carries >= ROW_RUNNER_WT, "runner (wt_carries >= 6)", "statue-ish (< 6)")) |>
  group_by(grp) |>
  summarise(
    n         = n(),
    above_80  = round(100 * mean(total_epa > hi_80_tot), 1),
    below_80  = round(100 * mean(total_epa < lo_80_tot), 1),
    above_90  = round(100 * mean(total_epa > hi_90_tot), 1),
    below_90  = round(100 * mean(total_epa < lo_90_tot), 1),
    .groups   = "drop"
  )
print(as.data.frame(q3))

# ===========================================================================
# Q4: ERA -- runner-group bias by era
# ===========================================================================
cli_h1("Q4: Runner-row bias by era (real effect should be era-stable/growing)")

q4 <- d |>
  filter(!is.na(wt_carries), wt_carries >= ROW_RUNNER_WT) |>
  mutate(era = if_else(season <= 2018, "2014-2018", "2019-2025")) |>
  group_by(era) |>
  summarise(
    n    = n(),
    bias = round(mean(res_tot), 3),
    t    = round(mean(res_tot) / (sd(res_tot) / sqrt(n())), 2),
    .groups = "drop"
  )
print(as.data.frame(q4))

# ===========================================================================
# SAVE
# ===========================================================================
cli_h1("Save outputs")
readr::write_csv(q1,        "output/08b2_scrambler_slopes.csv")
readr::write_csv(by_player, "output/08b2_player_bias.csv")
readr::write_csv(q3,        "output/08b2_tail_exceedance.csv")
readr::write_csv(q4,        "output/08b2_era_bias.csv")
cli_alert_success("output/08b2_scrambler_slopes.csv / _player_bias / _tail_exceedance / _era_bias")

cli_h1("Step 8b2 complete")
