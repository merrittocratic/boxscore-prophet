# read_stathead.R
# Ingest Stathead CSV exports and normalize them for joining against nflverse
# (gsis_id-keyed) tables. Handles the two things that make raw Stathead exports
# annoying: (1) multi-page exports of one query land as separate files, and
# (2) Stathead embeds a repeated header/rank row and occasional summary rows that
# break a naive read. The join key problem is real too: Stathead identifies
# players by display name, nflverse by gsis_id, so we normalize names to a match
# key and join through nflreadr's player table rather than hoping names collide
# cleanly (they don't -- "D.J." vs "DJ", Jr/Sr suffixes, accents).

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(janitor)

# --- 1. read one or many Stathead CSV files into a single tidy frame ----------
# Pass a vector of paths (e.g. the pages of one career-mode Season Finder query).
# All files MUST be exports of the SAME query -- we bind them row-wise.
read_stathead <- function(paths) {
  stopifnot(length(paths) >= 1)

  raw <- paths |>
    map(\(p) {
      # Stathead CSVs sometimes carry a title line above the real header; detect
      # the header row by finding the first line that contains "Player" or "Rk".
      lines <- read_lines(p)
      header_idx <- which(str_detect(lines, "(^|,)(Rk|Player)(,|$)"))[1]
      if (is.na(header_idx)) header_idx <- 1L  # fall back: assume row 1 is header

      read_csv(
        I(paste(lines[header_idx:length(lines)], collapse = "\n")),
        show_col_types = FALSE,
        # everything in as character first -- Stathead mixes "" and "--" for NA,
        # and we coerce deliberately after cleaning rather than letting readr guess
        col_types = cols(.default = col_character())
      )
    }) |>
    list_rbind()

  raw |>
    clean_names() |>
    # drop Stathead's repeated mid-table header rows (every ~20 rows it reprints
    # the column names as a data row) and any rank/summary artifact rows
    filter(
      !is.na(player),
      player != "Player",
      !str_detect(player, regex("^(league average|total)", ignore_case = TRUE))
    ) |>
    # Stathead uses "" and "--" and sometimes "?" for missing -- unify to NA
    mutate(across(everything(), \(x) na_if(na_if(na_if(str_trim(x), ""), "--"), "?"))) |>
    distinct()
}

# --- 2. build a name match key ------------------------------------------------
# Lowercase, strip accents, drop punctuation and Jr/Sr/II/III suffixes, collapse
# whitespace. This is the same normalization applied to both sides before joining
# so the key is symmetric. NOTE: name keys are inherently lossy (two "Mike
# Williams" exist) -- we disambiguate with position + a rough era where possible.
make_name_key <- function(name) {
  name |>
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>     # José -> jose
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b\\.?", "") |>
    str_replace_all("[^a-z ]", "") |>                  # drop ., ', - etc
    str_squish()
}

# --- 3. attach gsis_id via nflreadr players table -----------------------------
# nflreadr's load_players() is the nflverse "single source of truth" cross-walk;
# it already maps gsis_id <-> pfr_id and carries display_name, position, college.
# We join Stathead -> players on (name_key, position) which resolves the vast
# majority. Collisions and misses are RETURNED, not silently dropped, so you
# decide -- a silent name-join is exactly how the draft model got Jr/Sr bugs.
attach_gsis_id <- function(stathead_df,
                           player_col = "player",
                           pos_col = NULL) {
  players <- nflreadr::load_players() |>
    transmute(
      gsis_id,
      pfr_id,
      nfl_name = display_name,
      position,
      name_key = make_name_key(display_name)
    )

  sh <- stathead_df |>
    mutate(name_key = make_name_key(.data[[player_col]]))

  join_cols <- "name_key"
  if (!is.null(pos_col) && pos_col %in% names(sh)) {
    sh <- sh |> mutate(position = .data[[pos_col]])
    join_cols <- c("name_key", "position")
  }

  joined <- sh |> left_join(players, by = join_cols, relationship = "many-to-many")

  # surface the three outcomes instead of returning a quietly-wrong frame
  matched   <- joined |> filter(!is.na(gsis_id)) |> distinct()
  unmatched <- joined |> filter(is.na(gsis_id)) |>
    distinct(.data[[player_col]], name_key)
  # name_keys that hit >1 gsis_id == ambiguous, need manual disambiguation
  ambiguous <- matched |>
    add_count(name_key, name = "n_ids") |>
    filter(n_ids > 1) |>
    arrange(name_key)

  cli::cli_alert_info("Stathead rows in: {nrow(stathead_df)}")
  cli::cli_alert_success("Matched to gsis_id: {nrow(matched)}")
  if (nrow(unmatched) > 0)
    cli::cli_alert_warning("Unmatched names: {nrow(unmatched)} (see $unmatched)")
  if (nrow(ambiguous) > 0)
    cli::cli_alert_warning("Ambiguous name keys: {n_distinct(ambiguous$name_key)} (see $ambiguous) -- disambiguate by position/era before trusting these")

  list(matched = matched, unmatched = unmatched, ambiguous = ambiguous)
}

# --- usage --------------------------------------------------------------------
# One query exported across 3 pages:
#   df  <- read_stathead(c("exports/rb_comps_p1.csv",
#                          "exports/rb_comps_p2.csv",
#                          "exports/rb_comps_p3.csv"))
#   res <- attach_gsis_id(df, player_col = "player", pos_col = "pos")
#   res$matched      # ready to join against nflverse on gsis_id
#   res$unmatched    # eyeball these -- usually accents/suffixes the key missed
#   res$ambiguous    # the Mike Williamses -- resolve before using
#
# Then join straight into your model frames or comp tables on gsis_id.
