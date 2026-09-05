# R/oneoff/ecr_wayback_harvest.R
# Harvest historical FantasyPros weekly ECR (expert consensus rankings)
# from Wayback Machine snapshots -> data/ecr_history/.
#
# WHY: FantasyPros' API serves live-week rankings only (see
# R/10d0_ecr_fetch.R), so the model-vs-market backtest (the CLAUDE.md
# "bar the model has to clear") has no first-party historical source.
# Wayback Machine snapshots of the public rankings pages are the only
# point-in-time reconstruction available. Feasibility established
# 2026-09-05: ~1,000 in-season daily snapshots across 12 page variants,
# 2016-2025; QB coverage ~75 season-weeks, RB ~100, WR ~79, TE ~77.
#
# THREE PAGE ERAS, all parseable:
#   - ~2016-2017: server-rendered <table id="data"> -- rank td, a
#     player-label td (<a>/nfl/players/..>name</a> + <small.grey>team +
#     fp-id-NNN), and on flex pages a positional-rank td ("RB1").
#   - ~2017-2019: server-rendered <table id="rank-data"> whose rows carry
#     an <input class="wsis"> with data-name/team/position/id attributes.
#   - ~2020-2025: client app with the full payload embedded as
#     `var ecrData = {...};` JSON (players, rank_ecr, rank_min/max, week).
# The page's own week label (ecrData$week, else the <title>) is
# authoritative -- capture date alone misassigns Monday snapshots.
#
# OUTPUTS (committed, small):
#   data/ecr_history/manifest.csv           one row per snapshot attempted
#   data/ecr_history/ecr_hist_<season>_w<ww>.csv   chosen best source per
#     position-week, schema compatible with data/ecr/ecr_*.csv drops plus
#     provenance columns (source_page, wayback_ts, before_kick, ...)
# Raw HTML is cached OUTSIDE the repo (~400MB): ~/.cache/ecr_wayback/
# (override with env ECR_WAYBACK_CACHE). Safe to delete after a run.
#
# Usage: Rscript R/oneoff/ecr_wayback_harvest.R [--test]
#   --test: fetch/parse only a small stratified sample, write to
#           data/ecr_history/test/ for inspection.
# Polite to archive.org: 1 request / ~4s, exponential backoff on 429/5xx.
# Resumable: cached HTML is never refetched.

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(cli)
})

source("R/10d_name_helpers.R")

TEST_MODE <- "--test" %in% commandArgs(trailingOnly = TRUE)

OUT_DIR <- if (TEST_MODE) "data/ecr_history/test" else "data/ecr_history"
CDX_DIR <- file.path("data/ecr_history", "cdx")
CACHE_DIR <- Sys.getenv("ECR_WAYBACK_CACHE",
                        file.path(path.expand("~"), ".cache", "ecr_wayback"))
walk(c(OUT_DIR, CDX_DIR, CACHE_DIR), dir.create,
     recursive = TRUE, showWarnings = FALSE)

PAGES <- c("ppr-rb", "ppr-wr", "ppr-te", "qb", "rb", "wr", "te",
           "flex", "ppr-flex",
           "half-point-ppr-rb", "half-point-ppr-wr", "half-point-ppr-te")

# Per-position source preference: exact scoring match for the live feed
# (PPR for RB/WR/TE per 10d0, STD for QB) > half PPR > standard > flex.
PAGE_PREF <- tribble(
  ~position, ~page, ~pref,
  "RB", "ppr-rb", 1L, "RB", "half-point-ppr-rb", 2L, "RB", "rb", 3L,
  "RB", "ppr-flex", 4L, "RB", "flex", 5L,
  "WR", "ppr-wr", 1L, "WR", "half-point-ppr-wr", 2L, "WR", "wr", 3L,
  "WR", "ppr-flex", 4L, "WR", "flex", 5L,
  "TE", "ppr-te", 1L, "TE", "half-point-ppr-te", 2L, "TE", "te", 3L,
  "TE", "ppr-flex", 4L, "TE", "flex", 5L,
  "QB", "qb", 1L
)

cli_h1("ECR Wayback harvest{if (TEST_MODE) ' -- TEST MODE' else ''}")

# ---------------------------------------------------------------- CDX --
# Daily-collapsed snapshot inventory per page, cached to CDX_DIR.
fetch_cdx <- function(page) {
  f <- file.path(CDX_DIR, paste0(page, ".txt"))
  if (file.exists(f) && file.size(f) > 100) return(invisible(f))
  url <- paste0(
    "http://web.archive.org/cdx/search/cdx?url=fantasypros.com/nfl/rankings/",
    page, ".php&from=2016&to=2026&filter=statuscode:200",
    "&collapse=timestamp:8&fl=timestamp,original,length")
  for (try in 1:5) {
    system2("curl", c("-s", "--max-time", "120", shQuote(url), "-o", shQuote(f)))
    ok <- file.exists(f) &&
      any(grepl("^[0-9]{14} ", readLines(f, n = 5, warn = FALSE)))
    if (ok) break
    Sys.sleep(20 * try)
  }
  if (!ok) cli_abort("CDX inventory failed for {page}")
  Sys.sleep(5)
  invisible(f)
}
walk(PAGES, fetch_cdx)

read_cdx <- function(page) {
  read_delim(file.path(CDX_DIR, paste0(page, ".txt")), delim = " ",
             col_names = c("wayback_ts", "original", "bytes"),
             col_types = "ccc") |>
    filter(str_detect(wayback_ts, "^\\d{14}$")) |>
    mutate(page = page)
}

tasks <- map(PAGES, read_cdx) |>
  list_rbind() |>
  mutate(
    date  = as.Date(substr(wayback_ts, 1, 8), format = "%Y%m%d"),
    month = as.integer(substr(wayback_ts, 5, 6)),
    day   = as.integer(substr(wayback_ts, 7, 8))
  ) |>
  # In-season only: Sep-Dec plus the first days of Jan (weeks 17/18).
  filter(month %in% 9:12 | (month == 1 & day <= 8)) |>
  arrange(page, wayback_ts)

if (TEST_MODE) {
  set.seed(42)
  tasks <- tasks |>
    mutate(year = substr(wayback_ts, 1, 4)) |>
    group_by(page, era = year <= "2019") |>
    slice_sample(n = 1) |>
    ungroup() |>
    select(-year, -era)
}

cli_alert_info("{nrow(tasks)} in-season snapshots to process")

# -------------------------------------------------------------- fetch --
# id_ suffix returns the original page body without Wayback chrome.
fetch_snapshot <- function(page, wayback_ts, original) {
  f <- file.path(CACHE_DIR, paste0(page, "_", wayback_ts, ".html"))
  if (file.exists(f) && file.size(f) > 5000) return(f)
  url <- paste0("https://web.archive.org/web/", wayback_ts, "id_/", original)
  for (try in 1:4) {
    system2("curl", c("-sL", "--compressed", "--max-time", "90",
                      shQuote(url), "-o", shQuote(f)))
    ok <- file.exists(f) && file.size(f) > 5000
    if (ok) {
      # Wayback's own outage/exclusion pages are small but can exceed the
      # size floor; they never contain a rankings payload.
      head_txt <- readChar(f, min(file.size(f), 4000L))
      if (grepl("Internet Archive", head_txt, fixed = TRUE) &&
          !grepl("fantasypros", head_txt, ignore.case = TRUE)) ok <- FALSE
    }
    if (ok) { Sys.sleep(4); return(f) }
    if (file.exists(f)) file.remove(f)
    Sys.sleep(15 * try)
  }
  NA_character_
}

# -------------------------------------------------------------- parse --
title_week <- function(html) {
  m <- str_match(html, "<title>[^<]*?Week (\\d+)[^<]*</title>")[, 2]
  as.integer(m)
}

parse_json_era <- function(html) {
  m <- str_match(html, regex("var ecrData = (\\{.*?\\});", dotall = TRUE))[, 2]
  if (is.na(m)) return(NULL)
  d <- tryCatch(fromJSON(m), error = function(e) NULL)
  if (is.null(d) || is.null(d$players) || !is.data.frame(d$players) ||
      nrow(d$players) == 0) return(NULL)
  p <- d$players
  rank_col <- intersect(c("rank_ecr", "ecr", "rank", "rank_ave"), names(p))[1]
  if (is.na(rank_col)) return(NULL)
  wk <- suppressWarnings(as.integer(d$week %||% title_week(html)))
  tibble(
    player_name = p$player_name,
    position    = p$player_position_id %||% NA_character_,
    ecr_rank    = as.integer(p[[rank_col]]),
    ecr_best    = if ("rank_min" %in% names(p)) as.integer(p$rank_min) else NA_integer_,
    ecr_worst   = if ("rank_max" %in% names(p)) as.integer(p$rank_max) else NA_integer_,
    team        = if ("player_team_id" %in% names(p)) p$player_team_id else NA_character_,
    fp_player_id = if ("player_id" %in% names(p)) as.character(p$player_id) else NA_character_,
    week_label  = wk,
    scoring     = as.character(d$scoring %||% NA_character_),
    era         = "json"
  )
}

parse_table_era <- function(html) {
  tbl <- str_match(html,
                   regex("<table[^>]*id=\"rank-data\".*?</table>", dotall = TRUE))[, 1]
  if (is.na(tbl)) return(NULL)
  rows <- str_extract_all(tbl, regex("<tr[^>]*>.*?</tr>", dotall = TRUE))[[1]]
  rows <- rows[str_detect(rows, "class=\"wsis\"")]
  if (length(rows) == 0) return(NULL)
  attr_of <- function(row, a) str_match(row, paste0(a, "=\"([^\"]*)\""))[, 2]
  tibble(
    ecr_rank    = suppressWarnings(as.integer(
      str_match(rows, "<td[^>]*>(\\d+)</td>")[, 2])),
    player_name = attr_of(rows, "data-name"),
    team        = attr_of(rows, "data-team"),
    position    = attr_of(rows, "data-position"),
    fp_player_id = attr_of(rows, "data-id"),
    ecr_best    = NA_integer_,
    ecr_worst   = NA_integer_,
    week_label  = title_week(html),
    scoring     = NA_character_,
    era         = "table"
  ) |>
    filter(!is.na(ecr_rank), !is.na(player_name))
}

# Oldest era (~2016-2017): <table id="data">. Single-position pages have
# no position column, so position is filled from the page name by the
# caller; flex pages carry it as a "RB1"-style positional-rank cell.
parse_table_v1 <- function(html) {
  tbl <- str_match(html,
                   regex("<table[^>]*id=\"data\".*?</table>", dotall = TRUE))[, 1]
  if (is.na(tbl)) return(NULL)
  rows <- str_extract_all(tbl, regex("<tr[^>]*>.*?</tr>", dotall = TRUE))[[1]]
  rows <- rows[str_detect(rows, "player-label")]
  if (length(rows) == 0) return(NULL)
  tibble(
    ecr_rank    = suppressWarnings(as.integer(
      str_match(rows, "<td[^>]*>(\\d+)</td>")[, 2])),
    player_name = str_match(rows,
      "<a href=\"/nfl/players/[^\"]*\">([^<]+)</a>")[, 2],
    team        = str_match(rows, "<small class=\"grey\">([^<]*)</small>")[, 2],
    position    = str_match(rows, "<td>(QB|RB|WR|TE)\\d*</td>")[, 2],
    fp_player_id = str_match(rows, "fp-id-(\\d+)")[, 2],
    ecr_best    = NA_integer_,
    ecr_worst   = NA_integer_,
    week_label  = title_week(html),
    scoring     = NA_character_,
    era         = "table_v1"
  ) |>
    filter(!is.na(ecr_rank), !is.na(player_name))
}

# Single-position pages in the v1 era carry no per-row position marker.
PAGE_POSITION <- c(
  "qb" = "QB", "rb" = "RB", "wr" = "WR", "te" = "TE",
  "ppr-rb" = "RB", "ppr-wr" = "WR", "ppr-te" = "TE",
  "half-point-ppr-rb" = "RB", "half-point-ppr-wr" = "WR",
  "half-point-ppr-te" = "TE"
)

parse_snapshot <- function(f, page) {
  html <- tryCatch(readChar(f, file.size(f)), error = function(e) NULL)
  if (is.null(html)) return(NULL)
  out <- parse_json_era(html) %||% parse_table_era(html) %||%
    parse_table_v1(html)
  if (!is.null(out) && all(is.na(out$position)) && page %in% names(PAGE_POSITION))
    out$position <- PAGE_POSITION[[page]]
  out
}

# --------------------------------------------------------------- loop --
manifest <- vector("list", nrow(tasks))
parsed   <- vector("list", nrow(tasks))

for (i in seq_len(nrow(tasks))) {
  t <- tasks[i, ]
  f <- fetch_snapshot(t$page, t$wayback_ts, t$original)
  if (is.na(f)) {
    manifest[[i]] <- tibble(page = t$page, wayback_ts = t$wayback_ts,
                            status = "fetch_fail", week_label = NA_integer_,
                            n_players = 0L)
    next
  }
  rows <- parse_snapshot(f, t$page)
  bad_label <- is.null(rows) || all(is.na(rows$week_label)) ||
    !isTRUE(rows$week_label[1] >= 1 && rows$week_label[1] <= 18)
  if (bad_label) {
    manifest[[i]] <- tibble(page = t$page, wayback_ts = t$wayback_ts,
                            status = if (is.null(rows)) "parse_fail" else "not_weekly",
                            week_label = if (is.null(rows)) NA_integer_ else rows$week_label[1],
                            n_players = 0L)
    next
  }
  rows <- rows |>
    filter(position %in% c("RB", "WR", "TE", "QB")) |>
    mutate(page = t$page, wayback_ts = t$wayback_ts)
  manifest[[i]] <- tibble(page = t$page, wayback_ts = t$wayback_ts,
                          status = "ok", week_label = rows$week_label[1],
                          n_players = nrow(rows))
  parsed[[i]] <- rows
  if (i %% 25 == 0) cli_alert_info("{i}/{nrow(tasks)} snapshots processed")
}

manifest <- list_rbind(manifest)
parsed   <- list_rbind(compact(parsed))

cli_alert_success(
  "Processed {nrow(tasks)}: {sum(manifest$status == 'ok')} ok, {sum(manifest$status == 'parse_fail')} parse_fail, {sum(manifest$status == 'not_weekly')} not_weekly, {sum(manifest$status == 'fetch_fail')} fetch_fail")

if (nrow(parsed) == 0) {
  write_csv(manifest, file.path(OUT_DIR, "manifest.csv"))
  cli_abort("Nothing parsed -- manifest written, stopping.")
}

# ---------------------------------------------------------- selection --
# Season from capture year (January snapshots belong to the prior season);
# a snapshot is trustworthy for week W if captured before W's first
# kickoff -- rankings can shift intraweek, so later-but-still-pre-kick
# captures are preferred, and post-kick captures are kept but flagged.
sched <- nflreadr::load_schedules(2016:2025) |>
  filter(game_type == "REG") |>
  mutate(
    kick = as.POSIXct(paste(gameday, coalesce(gametime, "13:00")),
                      format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  ) |>
  group_by(season, week) |>
  summarise(first_kick = min(kick), .groups = "drop")

parsed <- parsed |>
  mutate(
    capture_utc = as.POSIXct(wayback_ts, format = "%Y%m%d%H%M%S", tz = "UTC"),
    cap_year    = as.integer(substr(wayback_ts, 1, 4)),
    cap_month   = as.integer(substr(wayback_ts, 5, 6)),
    season      = if_else(cap_month == 1, cap_year - 1L, cap_year),
    week        = week_label
  ) |>
  inner_join(sched, by = c("season", "week")) |>
  mutate(before_kick = capture_utc < first_kick) |>
  inner_join(PAGE_PREF, by = c("position", "page"))

chosen <- parsed |>
  group_by(season, week, position) |>
  arrange(desc(before_kick), pref, desc(wayback_ts), .by_group = TRUE) |>
  filter(page == first(page), wayback_ts == first(wayback_ts)) |>
  ungroup() |>
  # Position rank within the chosen source (flex pages mix positions).
  group_by(season, week, position) |>
  arrange(ecr_rank, .by_group = TRUE) |>
  mutate(pos_rank = row_number()) |>
  ungroup() |>
  mutate(player_name_norm = normalize_player_name(player_name)) |>
  select(season, week, player_name, player_name_norm, position, ecr_rank,
         pos_rank, ecr_best, ecr_worst, team, fp_player_id, scoring, era,
         source_page = page, wayback_ts, before_kick)

write_csv(manifest, file.path(OUT_DIR, "manifest.csv"))

chosen |>
  group_by(season, week) |>
  group_walk(function(g, k) {
    write_csv(bind_cols(k, g),
              file.path(OUT_DIR,
                        sprintf("ecr_hist_%d_w%02d.csv", k$season, k$week)))
  })

cli_h1("Coverage harvested (position-weeks by season)")
chosen |>
  distinct(season, week, position, before_kick) |>
  count(season, position, before_kick) |>
  pivot_wider(names_from = position, values_from = n, values_fill = 0L) |>
  arrange(season, desc(before_kick)) |>
  print(n = 40)

cli_alert_success(
  "{n_distinct(chosen[c('season','week')])} season-weeks written to {OUT_DIR}/")
