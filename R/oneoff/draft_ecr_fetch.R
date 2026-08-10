# R/oneoff/draft_ecr_fetch.R
# ONE-OFF (not a pipeline stage, not part of the weekly cadence).
#
# Fetches FantasyPros SEASON-LONG DRAFT rankings (type=draft) as the ECR
# anchor for the personal draft board. R/10d0_ecr_fetch.R is deliberately
# untouched: it fetches type=weekly for the in-season cadence and is wired
# into weekly_run.sh. This script is its draft-type sibling and writes
# OUTSIDE the repo so no tracked artifact and no production path moves.
#
# Request/parse/credential logic is cloned VERBATIM from 10d0 (same house
# rule the 10b slate clones follow: copy the frozen logic rather than
# reimplement it, so behavior cannot drift).
#
# TERMS: personal free tier -- non-commercial use, attribution required when
# publishing derivative analysis, 50 req/day with TRUNCATED responses. This
# board is personal draft prep and is NOT published, which is inside those
# terms. Four requests per run.
#
# Usage: Rscript R/oneoff/draft_ecr_fetch.R [season]
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
OUT_PATH <- file.path(OUT_DIR, "ecr_draft.csv")

# Scoring per position matches the trained thresholds: RB/WR/TE on PPR
# (full PPR, confirmed by Steve), QB on standard.
POSITIONS <- tribble(
  ~position, ~scoring,
  "RB", "PPR",
  "WR", "PPR",
  "TE", "PPR",
  "QB", "STD"
)

cli_h1("One-off: FantasyPros DRAFT ECR fetch -- {TARGET_SEASON}")

# Parallel credential pathways, cloned from 10d0: keychain (MacMini) then
# 1Password (laptop). Key is never printed, only its source.
get_key <- function() {
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
  cli_alert_info("No FantasyPros key in keychain or 1Password -- ECR fetch skipped.")
  cli_alert_info("The board still builds; it just carries no ECR anchor column.")
  quit(save = "no", status = 0)
}
api_key <- key_hit$key
cli_alert_info("API key loaded from {key_hit$src}")

RANK_FIELDS <- c("rank_ecr", "ecr", "rank", "rank_ave")

fetch_position <- function(position, scoring) {
  req <- request("https://api.fantasypros.com") |>
    req_url_path("public/v2/json/nfl", TARGET_SEASON, "consensus-rankings") |>
    req_url_query(type = "draft", scoring = scoring, position = position) |>
    req_headers(`x-api-key` = api_key) |>
    req_user_agent("boxscore-prophet draft prep (personal, non-commercial)") |>
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
    raw_path <- file.path(OUT_DIR, sprintf("raw_ecr_%s.json", position))
    writeLines(tryCatch(resp_body_string(resp), error = function(e) "<unreadable>"),
               raw_path)
    cli_alert_warning("{position}: unexpected response shape -- raw JSON saved to {raw_path}")
    return(NULL)
  }

  rank_col <- intersect(RANK_FIELDS, names(players))[1]
  name_col <- intersect(c("player_name", "name"), names(players))[1]
  if (is.na(rank_col) || is.na(name_col)) {
    raw_path <- file.path(OUT_DIR, sprintf("raw_ecr_%s.json", position))
    writeLines(resp_body_string(resp), raw_path)
    cli_alert_warning("{position}: no rank/name field among [{paste(names(players), collapse = ', ')}] -- raw saved to {raw_path}")
    return(NULL)
  }

  out <- tibble(
    player_name = players[[name_col]],
    position    = position,
    ecr_rank    = as.integer(players[[rank_col]]),
    ecr_best    = if ("rank_min" %in% names(players)) as.integer(players$rank_min) else NA_integer_,
    ecr_worst   = if ("rank_max" %in% names(players)) as.integer(players$rank_max) else NA_integer_,
    team        = if ("player_team_id" %in% names(players)) players$player_team_id else NA_character_,
    bye_ecr     = if ("player_bye_week" %in% names(players)) as.integer(players$player_bye_week) else NA_integer_
  ) |>
    filter(!is.na(ecr_rank)) |>
    arrange(ecr_rank)

  cli_alert_success("{position} ({scoring}): {nrow(out)} ranked (deepest rank {max(out$ecr_rank)})")
  out
}

ecr <- pmap(POSITIONS, function(position, scoring) fetch_position(position, scoring)) |>
  compact() |>
  list_rbind()

if (nrow(ecr) == 0) {
  cli_alert_warning("No ECR rows fetched -- nothing written. Board will build without the anchor.")
  quit(save = "no", status = 0)
}

ecr <- ecr |> mutate(player_name_norm = normalize_player_name(player_name))

# Same never-degrade guard as 10d0: a partial fetch (transient per-position
# 403) must not clobber a complete earlier drop in the same run directory.
if (file.exists(OUT_PATH)) {
  old <- readr::read_csv(OUT_PATH, show_col_types = FALSE)
  if (n_distinct(old$position) > n_distinct(ecr$position) || nrow(old) > nrow(ecr)) {
    cli_alert_warning(
      "Existing {OUT_PATH} is more complete ({nrow(old)} rows / {n_distinct(old$position)} pos vs fetched {nrow(ecr)} / {n_distinct(ecr$position)}) -- keeping existing."
    )
    quit(save = "no", status = 0)
  }
}

readr::write_csv(ecr, OUT_PATH)
cli_alert_success("{OUT_PATH} ({nrow(ecr)} rows: {paste(count(ecr, position)$n, collapse = '/')} by position)")

cli_h1("Draft ECR fetch complete")
