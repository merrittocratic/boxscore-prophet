# R/10d_name_helpers.R
# Shared player-name normalization for joining external feeds (FantasyPros
# ECR) to nflverse player names. Sourced by 10d0_ecr_fetch.R and
# 10d_content_tables.R -- both sides of a name join must normalize
# identically, so this lives in one file.
#
# Known cross-source differences handled:
#   - suffixes: "Jr.", "Sr.", "II"-"V" present/absent by source
#   - punctuation: "D.J. Moore" vs "DJ Moore", "Ja'Marr" apostrophes
#   - case and stray whitespace
# NOT handled (join will miss; 10d reports unmatched counts so these
# surface): true alias changes (e.g. Hollywood Brown vs Marquise Brown).
# Persistent misses get a manual alias row here.

NAME_ALIASES <- c(
  # "external form" = "nflverse form" -- extend as the join report surfaces
  "hollywood brown" = "marquise brown",
  "gabe davis"      = "gabriel davis"
)

normalize_player_name <- function(x) {
  out <- tolower(trimws(x))
  out <- gsub("\\.", "", out)                 # DJ, AJ, TJ
  out <- gsub("'", "", out)                   # JaMarr, DeVon
  out <- gsub("\\s+(jr|sr|ii|iii|iv|v)$", "", out)
  out <- gsub("\\s+", " ", out)
  hit <- out %in% names(NAME_ALIASES)
  out[hit] <- NAME_ALIASES[out[hit]]
  out
}
