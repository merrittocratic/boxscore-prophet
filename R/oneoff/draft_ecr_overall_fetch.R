# R/oneoff/draft_ecr_overall_fetch.R
# ONE-OFF (not a pipeline stage, not part of the weekly cadence).
#
# Fetches FantasyPros' cross-position DRAFT board (type=draft, position=ALL)
# -- one flat 1..N ranking across every position, as opposed to the four
# per-position boards draft_ecr_fetch.R pulls. This is the only source in
# the pipeline for combined_board_all_positions.csv's overall_rank column;
# nothing else here computes an overall rank from the per-position ECR
# sheets, and none should be inferred that way (position-specific ECR alone
# does not encode positional scarcity).
#
# Sibling of draft_ecr_fetch.R -- same credential pathway, same terms, same
# "writes outside the repo" discipline. Kept as its own script rather than
# folded into draft_ecr_fetch.R because it serves a different consumer
# (build_combined_board.R, not draft_board.R's per-position join) and one
# extra FantasyPros request is easier to reason about in its own file.
#
# TERMS: personal free tier -- non-commercial, attribution required if ever
# published, 50 req/day. This adds ONE request per run (five total across
# draft_ecr_fetch.R + this script).
#
# Usage: Rscript R/oneoff/draft_ecr_overall_fetch.R [season]
#   Env: DRAFT_PREP_OUT -- run directory (default ~/ff_draft_prep_2026/<today>)

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(cli)
})

source("R/10d_name_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
TARGET_SEASON <- if (length(args) >= 1) as.integer(args[1]) else 2026L

OUT_DIR <- Sys.getenv(
  "DRAFT_PREP_OUT",
  unset = file.path(path.expand("~/ff_draft_prep_2026"), format(Sys.Date(), "%Y-%m-%d"))
)
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
OUT_PATH <- file.path(OUT_DIR, "ecr_overall.csv")

cli_h1("One-off: FantasyPros OVERALL draft ECR fetch -- {TARGET_SEASON}")

# Credential pathway cloned verbatim from draft_ecr_fetch.R / 10d0.
get_key <- function() {
  k <- Sys.getenv("FANTASYPROS_API_KEY", unset = "")
  if (nzchar(k)) return(list(key = k, src = "env"))
  k <- tryCatch(
    system2("security", c("find-generic-password", "-s", "fantasypros-api-key", "-w"),
            stdout = TRUE, stderr = FALSE),
    warning = function(w) character(0), error = function(e) character(0)
  )
  if (length(k) > 0 && nzchar(k[1])) return(list(key = k[1], src = "keychain"))
  k <- tryCatch(
    system2("op", c("item", "get", shQuote("Fantasy Pros API Key"),
                    "--fields", "credential", "--reveal"),
            stdout = TRUE, stderr = FALSE),
    warning = function(w) character(0), error = function(e) character(0)
  )
  if (length(k) > 0 && nzchar(k[1])) return(list(key = k[1], src = "1password"))
  NULL
}

key_hit <- get_key()
if (is.null(key_hit)) {
  cli_alert_info("No FantasyPros key in keychain or 1Password -- overall ECR fetch skipped.")
  cli_alert_info("combined_board_all_positions.csv will build with no overall_rank column.")
  quit(save = "no", status = 0)
}
api_key <- key_hit$key
cli_alert_info("API key loaded from {key_hit$src}")

req <- request("https://api.fantasypros.com") |>
  req_url_path("public/v2/json/nfl", TARGET_SEASON, "consensus-rankings") |>
  req_url_query(type = "draft", scoring = "PPR", position = "ALL") |>
  req_headers(`x-api-key` = api_key) |>
  req_user_agent("boxscore-prophet draft prep (personal, non-commercial)") |>
  req_timeout(30)

resp <- tryCatch(req_perform(req), error = function(e) e)
if (inherits(resp, "error")) {
  cli_abort("overall ECR request failed: {conditionMessage(resp)}")
}

body <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
players <- body$players
if (is.null(players) || !is.data.frame(players) || nrow(players) == 0) {
  raw_path <- file.path(OUT_DIR, "raw_ecr_overall.json")
  writeLines(tryCatch(resp_body_string(resp), error = function(e) "<unreadable>"), raw_path)
  cli_abort("unexpected response shape -- raw JSON saved to {raw_path}")
}

ecr <- tibble(
  player_name       = players$player_name,
  position          = players$player_position_id,
  overall_ecr_rank  = as.integer(players$rank_ecr),
  overall_ecr_best  = if ("rank_min" %in% names(players)) as.integer(players$rank_min) else NA_integer_,
  overall_ecr_worst = if ("rank_max" %in% names(players)) as.integer(players$rank_max) else NA_integer_,
  team              = if ("player_team_id" %in% names(players)) players$player_team_id else NA_character_,
  bye_ecr           = if ("player_bye_week" %in% names(players)) as.integer(players$player_bye_week) else NA_integer_
) |>
  filter(!is.na(overall_ecr_rank)) |>
  arrange(overall_ecr_rank) |>
  mutate(player_name_norm = normalize_player_name(player_name))

# Same never-degrade guard as draft_ecr_fetch.R: a partial/truncated fetch
# must not clobber a more complete earlier drop in the same run directory.
if (file.exists(OUT_PATH)) {
  old <- readr::read_csv(OUT_PATH, show_col_types = FALSE)
  if (nrow(old) > nrow(ecr)) {
    cli_alert_warning(
      "Existing {OUT_PATH} is more complete ({nrow(old)} rows vs fetched {nrow(ecr)}) -- keeping existing."
    )
    quit(save = "no", status = 0)
  }
}

readr::write_csv(ecr, OUT_PATH)
cli_alert_success("{OUT_PATH} ({nrow(ecr)} players, deepest rank {max(ecr$overall_ecr_rank)})")

cli_h1("Draft overall ECR fetch complete")
