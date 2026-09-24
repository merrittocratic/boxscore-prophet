# ecr_lock_backfill.R -- one-off, 2026-09-24
#
# Rebuild output/10d_ecr_lock_<season>_w<week>.csv for weeks played before
# 10d started writing the lock. 10d_ecr_gap_<wtag>.csv is overwritten by
# every rescore (and shrinks to the not-yet-kicked-off games), so the only
# full record of each player's pre-kickoff ECR rank is the git history of
# that file. For each player, take the rank from the LAST committed version
# whose commit time is before that player's kickoff.
#
# Usage: Rscript R/archive/oneoff/ecr_lock_backfill.R 2026 1 2

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(purrr); library(cli)
})
source("R/10d_name_helpers.R")

args   <- commandArgs(trailingOnly = TRUE)
SEASON <- as.integer(args[1])
WEEKS  <- as.integer(args[-1])

for (wk in WEEKS) {
  wtag     <- sprintf("%d_w%02d", SEASON, wk)
  gap_rel  <- sprintf("output/10d_ecr_gap_%s.csv", wtag)
  ledger   <- read_csv(sprintf("output/10c_ledger_%s.csv", wtag), show_col_types = FALSE,
                       col_types = cols(kickoff_et = col_datetime(), .default = col_guess())) |>
    # Older ledger rows stored kickoff as naive ET, newer ones as UTC (the
    # 3df38d3 write-side fix); the latest row per player is the UTC one and
    # is the same row 10d grades, so use it.
    group_by(player_id) |>
    slice_max(run_ts, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(player_id, position, player_name, posteam, kickoff_et) |>
    mutate(player_name_norm = normalize_player_name(player_name))

  commits <- system2("git", c("log", "--format=%h,%ct", "--", gap_rel), stdout = TRUE)
  if (length(commits) == 0) { cli_alert_warning("{wtag}: no history for {gap_rel}"); next }

  versions <- map(commits, function(line) {
    parts <- strsplit(line, ",", fixed = TRUE)[[1]]
    txt   <- system2("git", c("show", sprintf("%s:%s", parts[1], gap_rel)), stdout = TRUE)
    read_csv(I(paste(txt, collapse = "\n")), show_col_types = FALSE) |>
      transmute(position, player_name, posteam, ecr_rank,
                commit_ts = as.POSIXct(as.numeric(parts[2]), origin = "1970-01-01", tz = "UTC"))
  }) |> list_rbind()

  lock <- versions |>
    mutate(player_name_norm = normalize_player_name(player_name)) |>
    inner_join(ledger |> select(player_id, position, posteam, player_name_norm, kickoff_et),
               by = c("player_name_norm", "position", "posteam")) |>
    filter(commit_ts < kickoff_et) |>
    group_by(player_id) |>
    slice_max(commit_ts, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(position, player_id, player_name, posteam, ecr_rank,
              ecr_as_of = format(commit_ts, "%Y-%m-%dT%H:%M:%S"))

  out <- sprintf("output/10d_ecr_lock_%s.csv", wtag)
  write_csv(lock |> arrange(position, ecr_rank), out)
  cli_alert_success("{wtag}: {nrow(lock)} players locked from {length(commits)} versions -> {out}")
}
