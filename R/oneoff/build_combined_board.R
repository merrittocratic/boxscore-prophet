# R/oneoff/build_combined_board.R
# ONE-OFF companion to draft_board.R -- not a pipeline stage.
#
# Builds combined_board_all_positions.csv, the flat cross-position sheet
# draft_board_app.html loads, from scratch each run:
#   - player_name, posteam, position, pos_ecr_rank, sched_effect_centered,
#     exp_startable_wks, exp_boom_wks, vs_ecr, bye_week, is_rookie,
#     dm_verdict, dm_p_boom, dm_p_bust  -- all row-bound straight from the
#     four per-position boards draft_board.R writes.
#   - overall_rank -- left-joined from ecr_overall.csv (FantasyPros'
#     cross-position "position=ALL" board, via draft_ecr_overall_fetch.R).
#     This is NOT derivable from the per-position ECR ranks above: two
#     players with pos_ecr_rank 5 at different positions are not equally
#     draftable, and no positional-scarcity model lives in this repo to
#     translate one into the other. If ecr_overall.csv is missing,
#     overall_rank is written as NA and the run is a build, not a silent
#     partial -- a warning is printed either way.
#
# Idempotent and safe to re-run: always rebuilds from the position boards
# and ecr_overall.csv currently on disk, backing up any existing
# combined_board_all_positions.csv to .bak before overwriting.
#
# Usage: Rscript R/oneoff/build_combined_board.R
#   Env: DRAFT_PREP_OUT -- run directory (default ~/ff_draft_prep_2026/<today>)

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

source("R/10d_name_helpers.R")

OUT_DIR <- Sys.getenv(
  "DRAFT_PREP_OUT",
  unset = file.path(path.expand("~/ff_draft_prep_2026"), format(Sys.Date(), "%Y-%m-%d"))
)

POS_FILES <- c(RB = "rb_board.csv", WR = "wr_board.csv",
               TE = "te_board.csv", QB = "qb_board.csv")

cli_h1("Building combined_board_all_positions.csv")

boards <- imap(POS_FILES, function(fname, pos) {
  p <- file.path(OUT_DIR, fname)
  if (!file.exists(p)) cli_abort("{p} not found -- run draft_board.R first")
  read_csv(p, show_col_types = FALSE) |>
    transmute(
      player_name, posteam, position,
      pos_ecr_rank = ecr_rank,
      sched_effect_centered, exp_startable_wks, exp_boom_wks, vs_ecr,
      bye_week, is_rookie, dm_verdict, dm_p_boom, dm_p_bust
    )
}) |> list_rbind()

overall_path <- file.path(OUT_DIR, "ecr_overall.csv")
if (file.exists(overall_path)) {
  overall <- read_csv(overall_path, show_col_types = FALSE) |>
    select(player_name_norm, overall_ecr_rank) |>
    distinct(player_name_norm, .keep_all = TRUE)
  combined <- boards |>
    mutate(player_name_norm = normalize_player_name(player_name)) |>
    left_join(overall, by = "player_name_norm") |>
    rename(overall_rank = overall_ecr_rank) |>
    select(-player_name_norm)
  n_hit <- sum(!is.na(combined$overall_rank))
  cli_alert_info("overall_rank joined: {n_hit}/{nrow(combined)} rows")
} else {
  combined <- boards |> mutate(overall_rank = NA_integer_)
  cli_alert_warning("{overall_path} not found -- run draft_ecr_overall_fetch.R first. overall_rank left blank.")
}

combined <- combined |> arrange(ifelse(is.na(overall_rank), .Machine$integer.max, overall_rank))

n_bye <- sum(!is.na(combined$bye_week))
n_rookie <- sum(combined$is_rookie, na.rm = TRUE)
cli_alert_info("bye_week present: {n_bye}/{nrow(combined)} rows")
cli_alert_info("rookies: {n_rookie} rows carry a dm_verdict")

combined_path <- file.path(OUT_DIR, "combined_board_all_positions.csv")
if (file.exists(combined_path)) {
  file.copy(combined_path, paste0(combined_path, ".bak"), overwrite = TRUE)
}
write_csv(combined, combined_path)
cli_alert_success("{combined_path} ({nrow(combined)} players)")
