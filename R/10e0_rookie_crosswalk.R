# R/10e0_rookie_crosswalk.R
# One-time crosswalk: ~/nfl-draft-model 2026 prospect predictions -> nflverse
# gsis_id, so the (planned) 10e rookie tracker can join weekly actuals from
# 10c to the draft model's pre-draft verdicts (.pred / p_boom / p_bust).
#
# Join chain (established 2026-07-30):
#   scored prospects (pfr_id, sparse)  --pfr_id-->  load_draft_picks(2026)
#     (pfr_player_id 257/257, actual round/pick/team)
#   draft_picks$gsis_id is ESB-FORMAT for 2026 (e.g. MEN516487), NOT true
#     gsis -- it joins load_players()$esb_id, which carries the real gsis_id.
#   Prospects missing pfr_id fall back to normalized name + position vs
#     draft_picks; undrafted prospects get a name+position match into
#     load_players(rookie_season == 2026) to catch signed UDFAs.
#
# Inputs:
#   $DRAFT_MODEL_DIR/data/05_scored_2026.rds  (default ~/nfl-draft-model)
#   data/rookie_crosswalk_overrides.csv        (optional; see header below)
# Outputs:
#   data/10e_rookie_crosswalk.csv   -- one row per matched prospect
#   output/10e_crosswalk_report.csv -- every fantasy-pos prospect + drafted
#                                      skill players the model did not score,
#                                      with match_method / miss reason
#
# This is a preseason setup step, not part of the weekly cadence. Re-run
# safe: outputs are fully rewritten each run (no append).

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(cli)
})

source("R/10d_name_helpers.R")

DRAFT_MODEL_DIR <- Sys.getenv("DRAFT_MODEL_DIR", unset = "~/nfl-draft-model")
SCORED_PATH     <- file.path(path.expand(DRAFT_MODEL_DIR), "data", "05_scored_2026.rds")
OVERRIDES_PATH  <- "data/rookie_crosswalk_overrides.csv"
OUT_CROSSWALK   <- "data/10e_rookie_crosswalk.csv"
OUT_REPORT      <- "output/10e_crosswalk_report.csv"

FANTASY_POS <- c("QB", "RB", "WR", "TE")
DRAFT_SEASON <- 2026L

cli_h1("Step 10e0: rookie crosswalk -- draft model -> gsis_id ({DRAFT_SEASON})")

if (!file.exists(SCORED_PATH)) {
  cli_alert_danger("Scored draft file not found: {SCORED_PATH}")
  cli_alert_info("Set DRAFT_MODEL_DIR if the repo lives elsewhere on this machine.")
  quit(status = 1)
}

# ---- 1. Draft model side -----------------------------------------------
# NOTE: the scored file has TWO position columns. `pos` is the raw scraped
# position and is NA for 427/717 rows (name-matching residue from the model
# build); `position` is the model's own grouping (fully populated, and what
# the predictions were conditioned on -- e.g. FB -> RB). Filter on `position`.
scored <- readRDS(SCORED_PATH) |>
  filter(position %in% FANTASY_POS) |>
  transmute(
    dm_name  = player_name,
    dm_norm  = normalize_player_name(player_name),
    dm_pos_raw = pos,
    pos = position,
    school,
    dm_pfr_id = pfr_id,
    pred      = .pred,
    pred_uncertainty,
    p_boom, p_bust, p_expected,
    model_verdict
  )

# The scored file carries duplicate prospect rows from the model build's
# name-matching issue (suffix variants like "Kevin Coleman" / "Kevin Coleman
# Jr." entered as separate players, scored with different feature coverage).
# Keep the better-linked twin: pfr_id present, else raw pos present. The
# dropped twins land in the report -- the real fix belongs upstream in
# ~/nfl-draft-model.
scored_ranked <- scored |>
  group_by(dm_norm, pos) |>
  arrange(is.na(dm_pfr_id), is.na(dm_pos_raw), .by_group = TRUE) |>
  mutate(dup_rank = row_number()) |>
  ungroup()
dropped_dupes <- scored_ranked |> filter(dup_rank > 1)
scored <- scored_ranked |> filter(dup_rank == 1) |> select(-dup_rank)
if (nrow(dropped_dupes) > 0) {
  kept_names <- scored |>
    semi_join(dropped_dupes, by = c("dm_norm", "pos")) |>
    select(dm_norm, pos, kept_name = dm_name, kept_pred = pred)
  dropped_dupes <- dropped_dupes |> left_join(kept_names, by = c("dm_norm", "pos"))
  cli_alert_warning("{nrow(dropped_dupes)} duplicate prospect rows dropped (name-variant twins; see report). Fix upstream in the draft model.")
}

stopifnot(!any(duplicated(scored[c("dm_norm", "pos")])))
cli_alert_info("Draft model fantasy-pos prospects: {nrow(scored)} ({sum(!is.na(scored$dm_pfr_id))} with pfr_id)")

# ---- 2. nflverse side ---------------------------------------------------
dp <- load_draft_picks(seasons = DRAFT_SEASON) |>
  filter(position %in% FANTASY_POS) |>
  transmute(
    draft_round = round, draft_pick = pick, draft_team = team,
    # 2026 quirk: this column carries ESB-format ids, not true gsis
    esb_id  = gsis_id,
    pfr_player_id, pfr_player_name,
    nfl_norm = normalize_player_name(pfr_player_name),
    position, college
  )
cli_alert_info("Drafted {paste(FANTASY_POS, collapse='/')} players: {nrow(dp)}")

players <- load_players() |>
  filter(!is.na(rookie_season), rookie_season == DRAFT_SEASON) |>
  transmute(
    gsis_id, esb_id,
    pl_pfr_id = pfr_id,
    nfl_display_name = display_name,
    pl_norm = normalize_player_name(display_name),
    position, latest_team
  )

# ---- 3. Match prospects -> draft picks ---------------------------------
# Stage 1: pfr_id (exact). Stage 2: normalized name + position.
m1 <- scored |>
  filter(!is.na(dm_pfr_id)) |>
  inner_join(dp, by = c("dm_pfr_id" = "pfr_player_id")) |>
  mutate(pfr_player_id = dm_pfr_id, match_method = "pfr_id")

m2 <- scored |>
  anti_join(m1, by = c("dm_norm", "pos")) |>
  inner_join(dp, by = c("dm_norm" = "nfl_norm", "pos" = "position")) |>
  mutate(match_method = "name_pos")

# Stage 2b: name only, tolerating position-label drift between the model
# grouping and nflverse (e.g. Bredeson: model RB, nflverse TE). Draft-class
# names are near-unique; the duplicate-gsis gate below catches collisions.
m2b <- scored |>
  anti_join(m1, by = c("dm_norm", "pos")) |>
  anti_join(m2, by = c("dm_norm", "pos")) |>
  inner_join(dp |> anti_join(bind_rows(m1 |> select(pfr_player_id),
                                       m2 |> select(pfr_player_id)),
                             by = "pfr_player_id"),
             by = c("dm_norm" = "nfl_norm")) |>
  mutate(match_method = "name_only")

matched_dp <- bind_rows(
  m1 |> select(-position, -nfl_norm),
  m2 |> rename(nfl_norm = dm_norm) |> mutate(dm_norm = nfl_norm) |> select(-nfl_norm),
  m2b |> select(-position)
)

# Stage 3: manual overrides for stragglers.
# data/rookie_crosswalk_overrides.csv columns:
#   dm_name, action ("map" | "drop"), pfr_player_id, note
# "map"  -> force-link the prospect to that draft-pick row
# "drop" -> exclude the prospect from the crosswalk (note says why)
if (file.exists(OVERRIDES_PATH)) {
  ov <- read_csv(OVERRIDES_PATH, show_col_types = FALSE)
  ov_map <- ov |> filter(action == "map")
  if (nrow(ov_map) > 0) {
    m3 <- scored |>
      semi_join(ov_map, by = "dm_name") |>
      anti_join(matched_dp, by = c("dm_norm", "pos")) |>
      inner_join(ov_map |> select(dm_name, pfr_player_id), by = "dm_name") |>
      inner_join(dp |> select(-nfl_norm, -position), by = "pfr_player_id") |>
      mutate(match_method = "override")
    matched_dp <- bind_rows(matched_dp, m3)
  }
  scored <- scored |>
    anti_join(ov |> filter(action == "drop"), by = "dm_name")
} else {
  ov <- tibble(dm_name = character())
}

# ---- 4. Undrafted prospects: name match into rookie players (UDFA) -----
udfa <- scored |>
  anti_join(matched_dp, by = c("dm_norm", "pos")) |>
  inner_join(players, by = c("dm_norm" = "pl_norm", "pos" = "position")) |>
  mutate(match_method = "udfa_name",
         draft_round = NA_integer_, draft_pick = NA_integer_,
         draft_team = latest_team, pfr_player_id = pl_pfr_id,
         pfr_player_name = nfl_display_name, college = NA_character_)

# ---- 5. Attach true gsis_id to drafted matches -------------------------
crosswalk <- matched_dp |>
  left_join(players |> select(gsis_id, esb_id, pl_pfr_id, nfl_display_name, latest_team),
            by = "esb_id") |>
  # ESB missing/unmatched -> second chance via pfr_id into players
  left_join(players |> select(gsis_id2 = gsis_id, pl_pfr_id2 = pl_pfr_id,
                              nfl_display_name2 = nfl_display_name,
                              latest_team2 = latest_team),
            by = c("pfr_player_id" = "pl_pfr_id2")) |>
  mutate(
    gsis_id          = coalesce(gsis_id, gsis_id2),
    nfl_display_name = coalesce(nfl_display_name, nfl_display_name2),
    latest_team      = coalesce(latest_team, latest_team2)
  ) |>
  select(-gsis_id2, -nfl_display_name2, -latest_team2, -pl_pfr_id) |>
  bind_rows(udfa |> select(-pl_pfr_id)) |>
  transmute(
    gsis_id, esb_id,
    pfr_id = pfr_player_id,
    nfl_name = coalesce(nfl_display_name, pfr_player_name),
    dm_name, pos, school,
    draft_round, draft_pick, draft_team,
    pred, pred_uncertainty, p_boom, p_bust, p_expected, model_verdict,
    match_method,
    # nflverse hands some rookies an ESB-format placeholder in gsis_id until
    # the real 00-0 id is assigned (22 of 671 as of 2026-07-30). Placeholders
    # will never join 10c actuals -- re-run this script in early September
    # and confirm this flag count drops to ~0.
    gsis_provisional = !is.na(gsis_id) & !grepl("^00-", gsis_id)
  ) |>
  arrange(pos, draft_pick)

# ---- 6. Gates -----------------------------------------------------------
dup_gsis <- crosswalk |> filter(!is.na(gsis_id)) |> count(gsis_id) |> filter(n > 1)
if (nrow(dup_gsis) > 0) {
  print(crosswalk |> semi_join(dup_gsis, by = "gsis_id"))
  cli_abort("Duplicate gsis_id in crosswalk -- fix via overrides before shipping.")
}
# Same class of dupe, but catchable even while gsis_id is still NA/placeholder:
# two model rows claiming one draft slot means an uncollapsed name variant.
dup_pick <- crosswalk |> filter(!is.na(draft_pick)) |> count(draft_pick) |> filter(n > 1)
if (nrow(dup_pick) > 0) {
  print(crosswalk |> semi_join(dup_pick, by = "draft_pick"))
  cli_abort("One draft pick matched by multiple prospect rows -- add a NAME_ALIASES entry (10d_name_helpers.R) so the dedupe collapses the variants.")
}

drafted_n   <- sum(!is.na(crosswalk$draft_pick))
drafted_gsis <- sum(!is.na(crosswalk$draft_pick) & !is.na(crosswalk$gsis_id))
n_prov <- sum(crosswalk$gsis_provisional)
cli_alert_info("Drafted prospects matched: {drafted_n} of {nrow(dp)} skill picks; {drafted_gsis} carry a gsis_id ({n_prov} still ESB placeholders)")
cli_alert_info("Signed UDFA matches: {sum(crosswalk$match_method == 'udfa_name')}")

anchors <- crosswalk |>
  filter(dm_name %in% c("Fernando Mendoza", "Ty Simpson")) |>
  select(dm_name, draft_team, draft_pick, gsis_id, model_verdict)
print(anchors)
stopifnot(nrow(anchors) == 2, !any(is.na(anchors$gsis_id)))

# ---- 7. Report: misses on both sides -----------------------------------
unmatched_prospects <- scored |>
  anti_join(crosswalk, by = c("dm_name", "pos")) |>
  transmute(name = dm_name, pos, school,
            side = "model_prospect_unmatched",
            note = "not drafted and no rookie-roster name match (likely unsigned)")

unscored_picks <- dp |>
  anti_join(crosswalk, by = c("pfr_player_id" = "pfr_id")) |>
  transmute(name = pfr_player_name, pos = position, school = college,
            side = "drafted_not_in_model",
            note = sprintf("pick %d (%s) -- draft model has no row", draft_pick, draft_team))

dupe_rows <- if (nrow(dropped_dupes) > 0) {
  dropped_dupes |>
    transmute(name = dm_name, pos, school,
              side = "duplicate_dropped",
              note = sprintf("name-variant twin of '%s' (pred %.3f vs kept %.3f) -- fix in draft model",
                             kept_name, pred, kept_pred))
} else tibble()

report <- bind_rows(
  crosswalk |> transmute(name = dm_name, pos, school,
                         side = "matched",
                         note = sprintf("%s -> %s", match_method,
                                        coalesce(gsis_id, "NO GSIS YET"))),
  unmatched_prospects,
  unscored_picks,
  dupe_rows
)

write_csv(crosswalk, OUT_CROSSWALK)
write_csv(report, OUT_REPORT)

cli_alert_success("Crosswalk: {nrow(crosswalk)} rows -> {OUT_CROSSWALK}")
cli_alert_success("Report: {nrow(report)} rows -> {OUT_REPORT}")
if (nrow(unmatched_prospects) > 0) {
  cli_alert_warning("{nrow(unmatched_prospects)} prospects unmatched (see report); add override rows if any are actually signed.")
}
if (nrow(unscored_picks) > 0) {
  cli_alert_warning("{nrow(unscored_picks)} drafted skill players not in the draft model (coverage note for content, not an error).")
}
