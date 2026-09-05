# R/19c_wr_context_grade.R
# PRE-REGISTERED GRADER for the WR context rung (bars approved
# 2026-09-05 before any of 19a/19b ran):
#
#   PRIMARY (2023-2025, WR trailing-FP top-12 bucket, shipped-map
#   columns: start = raw p_start, boom = p_boom_iso):
#     PASS iff brier_ctrl - brier_ctx >= 0.002 AND the week-clustered
#     bootstrap 95% CI of that improvement excludes 0 -- graded
#     separately for start and boom.
#   GUARD: pooled WR 2023-2025 brier_ctx <= brier_ctrl + 0.0005.
#   REPORTED (non-binding): full-window (2016+) versions, star-bucket
#   calibration gaps per arm, per-season skill.
#
# Star bucket = 18c trailing-FP definition: last-17-games PPR FP per
# game (>= 6 games), ranked within WR per week among graded rows,
# top 12. Run via scripts/run_19_wr_context_chain.sh after both arms'
# 06-chains complete.

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

set.seed(42)
B_BOOT <- 2000L
BAR_IMPROVE <- 0.002
GUARD_TOL <- 0.0005

cli_h1("19c: WR context rung grading (pre-registered)")

load_arm <- function(arm) {
  read_csv(sprintf("output/19c_%s_recal_probabilities.csv", arm),
           show_col_types = FALSE) |>
    filter(position == "WR") |>
    transmute(player_id, season, week,
              p_start_arm = p_start, p_boom_arm = p_boom_iso,
              hit_start, hit_boom)
}

ctrl <- load_arm("ctrl")
ctx  <- load_arm("ctx")

d <- inner_join(
  ctrl |> rename(p_start_ctrl = p_start_arm, p_boom_ctrl = p_boom_arm),
  ctx |> select(player_id, season, week,
                p_start_ctx = p_start_arm, p_boom_ctx = p_boom_arm),
  by = c("player_id", "season", "week")
) |>
  filter(!is.na(p_start_ctrl), !is.na(p_start_ctx),
         !is.na(p_boom_ctrl), !is.na(p_boom_ctx))

cli_alert_info("Common support: {nrow(d)} WR rows ({nrow(ctrl)} ctrl / {nrow(ctx)} ctx before join)")

# ------------------------------------------------ trailing-FP star bucket --
game_log <- load_player_stats(2014:2025) |>
  filter(season_type == "REG", !is.na(player_id),
         !is.na(fantasy_points_ppr)) |>
  select(player_id, season, week, fantasy_points_ppr) |>
  arrange(player_id, season, week)

trailing <- game_log |>
  group_by(player_id) |>
  mutate(g = row_number(), cum = cumsum(fantasy_points_ppr),
         prior_games = g - 1L, trail_n = pmin(prior_games, 17L),
         trail_fp = if_else(prior_games > 0,
                            (lag(cum, 1, default = 0) -
                               lag(cum, 18, default = 0)) / trail_n,
                            NA_real_)) |>
  ungroup() |>
  select(player_id, season, week, trail_fp, trail_n)

d <- d |>
  left_join(trailing, by = c("player_id", "season", "week")) |>
  group_by(season, week) |>
  mutate(trail_rank = ifelse(!is.na(trail_fp) & trail_n >= 6,
                             rank(-ifelse(!is.na(trail_fp) & trail_n >= 6,
                                          trail_fp, -Inf),
                                  ties.method = "first"),
                             NA_integer_),
         star = !is.na(trail_rank) & trail_rank <= 12) |>
  ungroup()

# ------------------------------------------------------------- machinery --
improve_ci <- function(dd, pc, px, hc) {
  wk <- paste(dd$season, dd$week)
  ibw <- split(seq_along(wk), wk); uw <- names(ibw)
  hit <- dd[[hc]]; a <- dd[[pc]]; b <- dd[[px]]
  imp <- mean((a - hit)^2) - mean((b - hit)^2)
  boots <- map_dbl(seq_len(B_BOOT), function(i) {
    j <- unlist(ibw[sample(uw, length(uw), TRUE)], use.names = FALSE)
    mean((a[j] - hit[j])^2) - mean((b[j] - hit[j])^2)
  })
  tibble(n = nrow(dd), weeks = length(uw),
         brier_ctrl = mean((a - hit)^2), brier_ctx = mean((b - hit)^2),
         improve = imp,
         ci_lo = quantile(boots, 0.025), ci_hi = quantile(boots, 0.975))
}

grade_window <- function(dd, label, graded) {
  star_d <- dd |> filter(star)
  rows <- map2(c("start", "boom"), c("p_start", "p_boom"), function(oc, stem) {
    improve_ci(star_d, paste0(stem, "_ctrl"), paste0(stem, "_ctx"),
               paste0("hit_", oc)) |>
      mutate(window = label, cell = paste("top-12", oc), .before = 1)
  }) |> list_rbind()
  pooled <- map2(c("start", "boom"), c("p_start", "p_boom"), function(oc, stem) {
    improve_ci(dd, paste0(stem, "_ctrl"), paste0(stem, "_ctx"),
               paste0("hit_", oc)) |>
      mutate(window = label, cell = paste("pooled", oc), .before = 1)
  }) |> list_rbind()
  out <- bind_rows(rows, pooled)
  if (graded) {
    out <- out |>
      mutate(verdict = case_when(
        str_starts(cell, "top-12") & improve >= BAR_IMPROVE & ci_lo > 0 ~ "PASS",
        str_starts(cell, "top-12") ~ "FAIL",
        str_starts(cell, "pooled") & brier_ctx <= brier_ctrl + GUARD_TOL ~ "guard ok",
        .default = "GUARD VIOLATED"
      ))
  } else out <- out |> mutate(verdict = "(reported)")
  out
}

primary <- grade_window(d |> filter(season >= 2023), "2023-2025 (graded)", TRUE)
full    <- grade_window(d, "2016-2025 (reported)", FALSE)

cli_h1("PRE-REGISTERED RESULT (bars: top-12 improvement >= 0.002, CI > 0; pooled guard +0.0005)")
print(bind_rows(primary, full) |>
        mutate(across(where(is.numeric), ~round(.x, 4))) |>
        as.data.frame(), row.names = FALSE)

# -------------------------------------------------- reported: calibration --
cli_h2("Reported: top-12 bucket calibration gap per arm (2023-2025)")
d |> filter(season >= 2023, star) |>
  summarise(n = n(),
            gap_start_ctrl = mean(hit_start) - mean(p_start_ctrl),
            gap_start_ctx  = mean(hit_start) - mean(p_start_ctx),
            gap_boom_ctrl  = mean(hit_boom) - mean(p_boom_ctrl),
            gap_boom_ctx   = mean(hit_boom) - mean(p_boom_ctx)) |>
  mutate(across(where(is.numeric), ~round(.x, 4))) |>
  print()

cli_h2("Reported: per-season top-12 improvement (start)")
d |> filter(star) |>
  group_by(season) |>
  summarise(n = n(),
            improve_start = mean((p_start_ctrl - hit_start)^2) -
              mean((p_start_ctx - hit_start)^2),
            improve_boom = mean((p_boom_ctrl - hit_boom)^2) -
              mean((p_boom_ctx - hit_boom)^2), .groups = "drop") |>
  mutate(across(where(is.numeric), ~round(.x, 4))) |>
  print(n = 12)

write_csv(bind_rows(primary, full), "output/19c_wr_context_grades.csv")
cli_h1("19c complete -- graded against the locked bars")
