# R/10h_news_capture.R
# Live player-news blurb CAPTURE (D-roadmap: textual signal, forward
# archive). Fetches the FantasyPros player-news pages on a launchd
# schedule and archives raw gzipped HTML with OUR capture timestamp --
# no reconstruction question, ever. Capture-first philosophy: parsing
# happens offline in R/10h2_news_parse.R and can be re-run forever, so
# a parser bug can never lose data.
#
# STORAGE IS DELIBERATELY OUTSIDE THE REPO (~/boxscore-news by
# default): blurb text is FantasyPros'/reporters' copyright -- raw
# HTML and parsed text are NEVER committed and NEVER published. Only
# derived structured flags (role_change_up etc., built later in the
# override layer) enter the repo. Private research archive only,
# consistent with the FantasyPros terms accepted 2026-07-18
# (non-commercial, attribution on derived analysis).
#
# Ops contract (spec approved by Steve 2026-09-05):
#   - pages 1-3 per capture (~22 blurbs/page covers busy news days)
#   - every 4h via launchd (scripts/com.boxscoreprophet.newscapture.plist)
#   - heartbeat line per run in <dir>/logs/capture.log
#   - 3 consecutive failed runs -> <dir>/ALERT_news_capture flag file
#     (surfaced in the Tuesday cadence check); cleared on next success
#   - always exits 0 (cron-safe)
#
# Usage: Rscript R/10h_news_capture.R
#   env NEWS_CAPTURE_DIR overrides the archive location.

suppressPackageStartupMessages({
  library(httr2)
})

DIR <- Sys.getenv("NEWS_CAPTURE_DIR",
                  file.path(path.expand("~"), "boxscore-news"))
for (d in file.path(DIR, c("raw", "logs", "state", "parsed")))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

LOG <- file.path(DIR, "logs", "capture.log")
FAILS <- file.path(DIR, "state", "fail_count")
ALERT <- file.path(DIR, "ALERT_news_capture")
N_PAGES <- 3L

ts_utc <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
log_line <- function(msg) {
  cat(sprintf("%s %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
              msg), file = LOG, append = TRUE)
}

fetch_page <- function(page) {
  url <- "https://www.fantasypros.com/nfl/player-news.php"
  req <- request(url) |>
    req_url_query(page = if (page > 1) page else NULL) |>
    req_user_agent("boxscore-prophet news capture (personal, non-commercial)") |>
    req_timeout(45)
  for (try in 1:3) {
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    if (!is.null(resp) && resp_status(resp) == 200) {
      html <- resp_body_string(resp)
      # validity: a real page is large and carries the blurb container
      if (nchar(html) > 20000 && grepl("player-news-header", html, fixed = TRUE))
        return(html)
    }
    Sys.sleep(10 * try)
  }
  NULL
}

saved <- 0L
for (p in seq_len(N_PAGES)) {
  html <- fetch_page(p)
  if (is.null(html)) next
  f <- gzfile(file.path(DIR, "raw",
                        sprintf("news_%s_p%d.html.gz", ts_utc, p)), "w")
  writeLines(html, f)
  close(f)
  saved <- saved + 1L
  Sys.sleep(3)
}

if (saved > 0) {
  log_line(sprintf("OK pages=%d/%d", saved, N_PAGES))
  writeLines("0", FAILS)
  if (file.exists(ALERT)) file.remove(ALERT)
} else {
  n_fail <- 1L
  if (file.exists(FAILS))
    n_fail <- suppressWarnings(as.integer(readLines(FAILS, n = 1)[1])) + 1L
  if (is.na(n_fail)) n_fail <- 1L
  writeLines(as.character(n_fail), FAILS)
  log_line(sprintf("FAIL consecutive=%d", n_fail))
  if (n_fail >= 3) {
    writeLines(sprintf("news capture has failed %d consecutive runs (last: %s UTC)",
                       n_fail, ts_utc), ALERT)
    log_line("ALERT written")
  }
}

quit(save = "no", status = 0)
