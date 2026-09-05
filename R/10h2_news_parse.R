# R/10h2_news_parse.R
# Offline parser for the news-capture archive (R/10h_news_capture.R).
# Idempotent and re-runnable: reads raw/*.html.gz not yet listed in
# state/parsed_files.txt, extracts blurbs, dedupes on the FantasyPros
# news id, and appends to parsed/blurbs.jsonl. A parser improvement
# can always be replayed against the full raw archive (delete the
# state files and re-run).
#
# Output record (one JSON object per line, PRIVATE -- never committed):
#   news_id, slug, headline, published_utc, players (fp slugs),
#   body, impact, source, first_seen_capture (UTC ts of the capture
#   file that first contained it)
#
# Usage: Rscript R/10h2_news_parse.R
#   env NEWS_CAPTURE_DIR overrides the archive location.

suppressPackageStartupMessages({
  library(stringr)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || all(is.na(a))) b else a

DIR <- Sys.getenv("NEWS_CAPTURE_DIR",
                  file.path(path.expand("~"), "boxscore-news"))
RAW <- file.path(DIR, "raw")
IDX <- file.path(DIR, "state", "parsed_files.txt")
SEEN <- file.path(DIR, "state", "seen_news_ids.txt")
OUT <- file.path(DIR, "parsed", "blurbs.jsonl")

done <- if (file.exists(IDX)) readLines(IDX) else character(0)
seen <- if (file.exists(SEEN)) readLines(SEEN) else character(0)
files <- setdiff(list.files(RAW, "^news_.*\\.html\\.gz$"), done)

if (length(files) == 0) {
  cat("10h2: nothing new to parse\n")
  quit(save = "no", status = 0)
}

strip_tags <- function(x) {
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&#39;|&apos;", "'", x)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  str_squish(x)
}

# "Sat, Sep 5th 12:26am EDT" + capture year -> UTC. Year is inferred
# from the capture timestamp; a December blurb read in January belongs
# to the prior year.
parse_published <- function(s, capture_ts) {
  m <- str_match(s, "(\\w{3}), (\\w{3}) (\\d+)\\w{2} (\\d+):(\\d+)(am|pm) E[DS]T")
  if (is.na(m[1])) return(NA_character_)
  cap_year <- as.integer(substr(capture_ts, 1, 4))
  cap_month <- as.integer(substr(capture_ts, 5, 6))
  mon <- match(m[3], month.abb)
  yr <- if (mon == 12 && cap_month == 1) cap_year - 1L else cap_year
  hh <- as.integer(m[5]) %% 12L + if (m[7] == "pm") 12L else 0L
  et <- as.POSIXct(sprintf("%d-%02d-%02d %02d:%02d", yr, mon,
                           as.integer(m[4]), hh, as.integer(m[6])),
                   format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  format(et, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Item container: <div class="player-news-item"> holds (in order) the
# player thumbnail (own player link + "RB - DEN" caption), the header
# div (news link + headline + timestamp + author), the body <p> with
# (Source: ...), the Fantasy Impact <p>, and the Category footer.
parse_file <- function(fname) {
  con <- gzfile(file.path(RAW, fname), "r")
  html <- paste(readLines(con, warn = FALSE), collapse = "\n")
  close(con)
  capture_ts <- str_match(fname, "news_(\\d{14})_")[, 2]
  items <- str_split(html, fixed("<div class=\"player-news-item\">"))[[1]][-1]
  recs <- lapply(items, function(b) {
    b <- substr(b, 1, 12000)
    id_m <- str_match(
      b, "/nfl/news/(\\d+)/([a-z0-9-]+)\\.php\"[^>]*>([^<]+)</a>")
    if (is.na(id_m[1])) return(NULL)
    pub_m <- str_match(b, "(\\w{3}, \\w{3} \\d+\\w{2} [\\d:]+[ap]m E[DS]T)")
    players <- unique(str_match_all(b, "/nfl/players/([a-z0-9-]+)\\.php")[[1]][, 2])
    pt_m <- str_match(b, ">\\s*([A-Z]{1,3}) - ([A-Z]{2,3})\\s*</p>")
    author <- str_match(b, "/news/correspondents/[^\"]+\"[^>]*>([^<]+)<")[, 2]
    body <- str_match(b, regex("</div></div>\\s*<p>(.*?)</p>",
                               dotall = TRUE))[, 2]
    src <- str_match(body %||% "",
                     "\\(Source:\\s*(?:<a [^>]*>)?([^<)]+)")[, 2]
    body <- strip_tags(str_replace(body %||% "", "\\(Source:.*$", ""))
    impact <- strip_tags(str_match(
      b, regex("<em>Fantasy Impact:</em></b>\\s*(.*?)</p>",
               dotall = TRUE))[, 2])
    category <- str_match(b, "Category: <a [^>]*>([^<]+)<")[, 2]
    list(
      news_id = id_m[2], slug = id_m[3],
      headline = str_squish(id_m[4]),
      published_utc = parse_published(pub_m[2], capture_ts),
      players = as.list(players),
      pos = pt_m[2], team = pt_m[3],
      author = author, category = category,
      body = body, impact = impact, source = str_squish(src),
      first_seen_capture = capture_ts
    )
  })
  Filter(Negate(is.null), recs)
}

new_ids <- character(0)
n_new <- 0L
out_con <- file(OUT, "a")
for (f in sort(files)) {
  for (r in parse_file(f)) {
    if (r$news_id %in% c(seen, new_ids)) next
    writeLines(toJSON(r, auto_unbox = TRUE, null = "null"), out_con)
    new_ids <- c(new_ids, r$news_id)
    n_new <- n_new + 1L
  }
}
close(out_con)

cat(c(seen, new_ids), file = SEEN, sep = "\n")
cat(c(done, sort(files)), file = IDX, sep = "\n")
cat(sprintf("10h2: %d files parsed, %d new blurbs (total ids: %d)\n",
            length(files), n_new, length(seen) + length(new_ids)))
