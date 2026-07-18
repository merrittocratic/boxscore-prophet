# R/10d0_ecr_fetch.R
# Fetch FantasyPros weekly expert consensus rankings (ECR) -> the 10d
# file-drop feed: data/ecr/ecr_<season>_w<week>.csv
# (columns: player_name, position, ecr_rank [, ecr_best, ecr_worst, team]).
#
# KEY: macOS keychain, never in the repo or environment files:
#   security add-generic-password -a fantasypros -s fantasypros-api-key -w '<key>'
# Missing key or failed request -> EXIT 0 with a skip message: the weekly
# runner must not fall over while API approval is pending (10d already
# skips the gap piece gracefully when the CSV is absent).
#
# TERMS (accepted 2026-07-18, personal free tier): non-commercial use;
# attribution required when publishing derivative analysis (10d bakes a
# FantasyPros credit into the gap-piece markdown); free tier = 50 req/day
# with TRUNCATED responses -- this script logs players-per-position so the
# truncation depth is measured the day the key arrives; player image URLs
# are SportRadar-licensed and are not fetched. Commercial terms needed
# before the Substack monetizes (see memory/paid-data notes).
#
# ENDPOINT (community-documented shape; UNVERIFIED until the key arrives --
# expect to adjust field names on first live run; raw JSON is saved to
# logs/ on parse failure for exactly that purpose):
#   GET https://api.fantasypros.com/public/v2/json/nfl/{season}/consensus-rankings
#       ?type=weekly&scoring={PPR|STD}&position={RB|WR|QB}&week={week}
#       header x-api-key: <key>
#
# Usage: Rscript R/10d0_ecr_fetch.R [season] [week]
# NOTE: the API serves CURRENT rankings -- this is a live-week (production)
# step. Hindcast weeks use whatever file is already dropped in data/ecr/.

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(cli)
})

source("R/10d_name_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2025L
TARGET_WEEK   <- if (length(args) >= 2) as.integer(args[2]) else 15L
WTAG <- sprintf("%d_w%02d", TARGET_SEASON, TARGET_WEEK)

POSITIONS <- tribble(
  ~position, ~scoring,
  "RB", "PPR",
  "WR", "PPR",
  "QB", "STD"
)

cli_h1("Step 10d0: FantasyPros ECR fetch -- {TARGET_SEASON} week {TARGET_WEEK}")

api_key <- tryCatch(
  system2("security", c("find-generic-password", "-s", "fantasypros-api-key", "-w"),
          stdout = TRUE, stderr = FALSE),
  warning = function(w) character(0), error = function(e) character(0)
)
if (length(api_key) == 0 || !nzchar(api_key[1])) {
  cli_alert_info("No fantasypros-api-key in keychain -- ECR fetch skipped (10d gap piece will skip too).")
  quit(save = "no", status = 0)
}
api_key <- api_key[1]

# Candidate field names for the consensus rank -- the exact schema is
# unverified until the free-tier key arrives; take the first present.
RANK_FIELDS <- c("rank_ecr", "ecr", "rank", "rank_ave")

fetch_position <- function(position, scoring) {
  req <- request("https://api.fantasypros.com") |>
    req_url_path("public/v2/json/nfl", TARGET_SEASON, "consensus-rankings") |>
    req_url_query(type = "weekly", scoring = scoring,
                  position = position, week = TARGET_WEEK) |>
    req_headers(`x-api-key` = api_key) |>
    req_user_agent("boxscore-prophet ECR fetch (personal, non-commercial)") |>
    req_timeout(30)

  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    cli_alert_warning("{position}: request failed ({conditionMessage(resp)})")
    return(NULL)
  }

  body <- tryCatch(resp_body_json(resp, simplifyVector = TRUE),
                   error = function(e) NULL)
  players <- body$players
  if (is.null(players) || !is.data.frame(players) || nrow(players) == 0) {
    raw_path <- sprintf("logs/10d0_raw_%s_%s.json", position, WTAG)
    writeLines(tryCatch(resp_body_string(resp), error = function(e) "<unreadable>"),
               raw_path)
    cli_alert_warning("{position}: unexpected response shape -- raw JSON saved to {raw_path} for schema adjustment")
    return(NULL)
  }

  rank_col <- intersect(RANK_FIELDS, names(players))[1]
  name_col <- intersect(c("player_name", "name"), names(players))[1]
  if (is.na(rank_col) || is.na(name_col)) {
    raw_path <- sprintf("logs/10d0_raw_%s_%s.json", position, WTAG)
    writeLines(resp_body_string(resp), raw_path)
    cli_alert_warning("{position}: no rank/name field among [{paste(names(players), collapse = ', ')}] -- raw JSON saved to {raw_path}")
    return(NULL)
  }

  out <- tibble(
    player_name = players[[name_col]],
    position    = position,
    ecr_rank    = as.integer(players[[rank_col]]),
    ecr_best    = if ("rank_min" %in% names(players)) as.integer(players$rank_min) else NA_integer_,
    ecr_worst   = if ("rank_max" %in% names(players)) as.integer(players$rank_max) else NA_integer_,
    team        = if ("player_team_id" %in% names(players)) players$player_team_id else NA_character_
  ) |>
    filter(!is.na(ecr_rank)) |>
    arrange(ecr_rank)

  # Free tier truncates responses: log the depth so we can judge whether
  # the streamer tier survives or the HOF upgrade is warranted.
  cli_alert_success("{position} ({scoring}): {nrow(out)} ranked players (deepest rank {max(out$ecr_rank)})")
  out
}

ecr <- pmap(POSITIONS, function(position, scoring) fetch_position(position, scoring)) |>
  compact() |>
  list_rbind()

if (nrow(ecr) == 0) {
  cli_alert_warning("No ECR rows fetched -- nothing written (10d will skip the gap piece).")
  quit(save = "no", status = 0)
}

ecr <- ecr |> mutate(player_name_norm = normalize_player_name(player_name))

dir.create("data/ecr", showWarnings = FALSE)
out_path <- sprintf("data/ecr/ecr_%s.csv", WTAG)
readr::write_csv(ecr, out_path)
cli_alert_success("{out_path} ({nrow(ecr)} rows: {paste(count(ecr, position)$n, collapse = '/')} by position)")

cli_h1("Step 10d0 complete")
