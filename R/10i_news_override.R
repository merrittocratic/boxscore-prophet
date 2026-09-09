# R/10i_news_override.R
# Stage 3 of the news-capture -> override pipeline: R/10h captures,
# R/10h2 parses to structured blurbs, this classifies and produces the
# per-week override candidate file. Live override layer per CLAUDE.md
# ("Beat-reporter/text signal is banned from training... lives in a live
# override layer instead, graded in-season, not trained on") -- this
# script NEVER writes into a trained feature table. Its output is a
# separate sidecar (data/news_overrides_<season>_w<week>.csv) that
# R/10c reads and applies as a BOUNDED nudge on top of the recalibrated
# probability, never baked into the model.
#
# DESIGN (from reading all 95 RB/WR/TE/QB blurbs in the first real pull
# of Earnest's archive, 2026-09-09): ~55-60% is practice-participation
# tracking (redundant with trained R/11b injury features), ~20-25% is
# depth-player transaction noise, ~10-15% is genuine role/opportunity
# signal -- which splits into (a) official depth-chart postings (cheap
# keyword rule catches these, no LLM needed) and (b) IMPLIED role change
# via a different player's move (e.g. "Willis waived... signals the
# return of Kittle") -- this is the case an LLM actually earns its keep
# on. Same player often accumulates many blurbs across a week (7 for one
# player in the sample) -- dedup to latest before classifying anything.
#
# PIPELINE:
#   1. Position filter: RB/WR/TE/QB only (~half the archive, free).
#   2. Unnest players (a blurb can name several) + crosswalk EACH row:
#      FantasyPros slug -> name -> gsis_id + TEAM, via the SAME
#      normalize_player_name() + unique-within-season roster join R/18a
#      and R/21a already use for ECR's own crosswalk (R/10d_name_
#      helpers.R). Team has to be known before timing validity (next
#      step) can be per-player rather than week-wide.
#   3. Timing validity: blurb published before THAT PLAYER'S OWN team's
#      kickoff (fallback: week's first kickoff if team unknown) -- same
#      pattern R/21a_discrimination_fns.R::ecr_join() and R/11b's
#      build_lock_table() already use. FIXED 2026-09-09 (Steve's own
#      sanity check): the original version filtered on the week's FIRST
#      kickoff for every player, which for any week with an early
#      Thursday game cut off ALL Friday/Saturday news about Sunday-game
#      players -- stricter than even the official Friday injury-report
#      lock (R/11b), let alone useful.
#   4. Dedup: latest VALID blurb per gsis_id (not per slug -- multiple
#      slug spellings can crosswalk to the same real player; dedup
#      happens after validity filtering so an invalid later blurb can
#      never displace a valid earlier one for the same player).
#   5. Depth-chart rule (cheap, no LLM, no API dependency): regex on
#      the headline for "listed as ... starter" etc. Flags directly,
#      source="rule". Handles the whole depth-chart sub-case for free.
#   6. LLM classification for everything else -- one Haiku call per
#      remaining blurb, see "STEP 6 CREDENTIAL" below.
#   7. Output: data/news_overrides_<season>_w<week>.csv.
#
# STEP 6 CREDENTIAL: Earnest already has an Anthropic API key in the
# Mac mini's Keychain, item name confirmed 2026-09-09 (Steve, via
# Earnest): "ANTHROPIC_API_KEY". get_anthropic_key() below follows the
# SAME 3-tier lookup every other API-gated script in this repo uses
# (env var -> macOS Keychain -> 1Password, see R/10d0_ecr_fetch.R::
# get_key()), never hardcoded, never echoed. Verified end-to-end
# 2026-09-09 via the 1Password tier on the laptop (real classification
# calls against real blurbs, including a caught-and-fixed retargeting
# bug -- see the beneficiary re-crosswalk note below) -- not yet
# verified against the actual Keychain tier on the mini itself.
#
# Usage: Rscript R/10i_news_override.R <season> <week>
#   Env NEWS_CAPTURE_DIR overrides the archive location (default
#     ~/boxscore-news, matching R/10h/R/10h2).

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(httr2)
  library(cli)
})

source("R/10d_name_helpers.R")

args   <- commandArgs(trailingOnly = TRUE)
SEASON <- if (length(args) >= 1) as.integer(args[1]) else cli_abort("Usage: Rscript R/10i_news_override.R <season> <week>")
WEEK   <- if (length(args) >= 2) as.integer(args[2]) else cli_abort("Usage: Rscript R/10i_news_override.R <season> <week>")

DIR <- Sys.getenv("NEWS_CAPTURE_DIR", file.path(path.expand("~"), "boxscore-news"))
BLURBS_PATH <- file.path(DIR, "parsed", "blurbs.jsonl")

FANTASY_POSITIONS <- c("RB", "WR", "TE", "QB")

cli_h1("10i: news override candidates -- {SEASON} week {WEEK}")

if (!file.exists(BLURBS_PATH)) cli_abort("No parsed archive at {BLURBS_PATH} -- run R/10h2_news_parse.R first")

# ===========================================================================
# 1. LOAD + POSITION FILTER
# ===========================================================================

blurbs_raw <- stream_in(file(BLURBS_PATH), verbose = FALSE) |> as_tibble()
blurbs <- blurbs_raw |>
  filter(pos %in% FANTASY_POSITIONS) |>
  mutate(published_utc = as.POSIXct(published_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

cli_alert_info("{nrow(blurbs_raw)} total blurbs -> {nrow(blurbs)} RB/WR/TE/QB after position filter")

# ===========================================================================
# 2. UNNEST players (list column of FP slugs, one blurb can name several)
# + CROSSWALK each row to gsis_id + TEAM -- slug -> name -> gsis_id, reusing
# normalize_player_name() + the unique-within-season roster join pattern
# from R/18a/R/21a. Team is needed for per-player timing validity (step 3)
# -- has to happen before that, not after.
# ===========================================================================

slug_to_name <- function(slug) {
  slug |>
    str_replace_all("-", " ") |>
    str_to_title()
}

rosters <- nflreadr::load_rosters(SEASON) |>
  filter(position %in% FANTASY_POSITIONS, !is.na(gsis_id)) |>
  mutate(nm = normalize_player_name(full_name)) |>
  distinct(position, nm, gsis_id, team) |>
  add_count(nm) |>
  filter(n == 1) |>
  select(nm, gsis_id, team)

by_player <- blurbs |>
  select(news_id, headline, body, impact, published_utc, pos, players) |>
  unnest_longer(players, values_to = "slug") |>
  filter(!is.na(slug), nzchar(slug)) |>
  mutate(name_guess = slug_to_name(slug),
         nm = normalize_player_name(name_guess)) |>
  left_join(rosters, by = "nm")

n_matched <- sum(!is.na(by_player$gsis_id))
cli_alert_info("Crosswalk: {n_matched}/{nrow(by_player)} matched to a gsis_id ({round(100*n_matched/nrow(by_player),1)}%)")
if (n_matched / nrow(by_player) < 0.85) {
  cli_alert_warning("Match rate below 85% -- inspect by_player$nm[is.na(by_player$gsis_id)] before trusting output")
}

matched <- by_player |> filter(!is.na(gsis_id))

# ===========================================================================
# 3. TIMING VALIDITY -- PER-PLAYER kickoff (fixed 2026-09-09, Steve's own
# sanity check): valid if published before THAT PLAYER'S OWN team's
# kickoff, falling back to the week's first kickoff if team is unknown --
# same pattern R/21a_discrimination_fns.R::ecr_join() and R/11b's
# build_lock_table() already use. The original version filtered on the
# week's FIRST kickoff for every player regardless of team, which for any
# week with an early Thursday game cut off ALL Friday/Saturday news about
# Sunday-game players -- stricter than even the official Friday
# injury-report lock (R/11b), let alone useful.
# ===========================================================================

sched <- nflreadr::load_schedules(SEASON) |>
  filter(game_type == "REG", week == WEEK) |>
  mutate(kick = as.POSIXct(paste(gameday, coalesce(gametime, "13:00")),
                           format = "%Y-%m-%d %H:%M", tz = "America/New_York"))
kicks <- bind_rows(
  sched |> select(team = home_team, kick),
  sched |> select(team = away_team, kick)
)
first_kick <- min(sched$kick, na.rm = TRUE)

matched <- matched |>
  left_join(kicks, by = "team") |>
  mutate(kick_eff = coalesce(kick, first_kick)) |>
  filter(!is.na(published_utc), published_utc < kick_eff)

cli_alert_info("{nrow(matched)} player-blurb rows valid (published before that player's own kickoff; fallback week's first kickoff {format(first_kick, tz='America/New_York')})")

# ===========================================================================
# 4. DEDUP -- latest VALID blurb per gsis_id (not per slug -- multiple
# slug spellings can crosswalk to the same real player; happens after
# validity filtering so an invalid later blurb can never displace a valid
# earlier one for the same player).
# ===========================================================================

matched <- matched |>
  arrange(gsis_id, desc(published_utc)) |>
  distinct(gsis_id, .keep_all = TRUE)

cli_alert_info("{nrow(matched)} distinct players after dedup")

# ===========================================================================
# 6. DEPTH-CHART RULE (cheap, no LLM) -- catches the clean sub-case for
# free: "listed as RB2 on ... depth chart", "named ... starter", etc.
# ===========================================================================

# Directional, not a single undifferentiated "depth_chart" flag (fixed
# 2026-09-09 -- caught before wiring to 10c: the original version treated
# "Jeremiyah Love listed as RB2" and "Tyler Allgeier listed as starter"
# identically, which would have pushed an "up" nudge onto a CONFIRMED
# BACKUP. Starter/QB1/RB1/WR1/TE1 postings are the beneficiary's own
# subject (no retargeting needed, unlike the LLM path); backup/QB2/RB2/
# etc. postings are a mild down signal for that same player.
STARTER_RX <- regex("named .*(starter|starting)|listed as .*(starter|QB1|RB1|WR1|TE1)\\b", ignore_case = TRUE)
BACKUP_RX  <- regex("listed as .*(QB2|RB2|WR2|WR3|TE2|backup)\\b", ignore_case = TRUE)

# Headline ONLY, not body/impact -- caught 2026-09-09: "Michael Penix Jr.
# to be inactive in Week 1" matched STARTER_RX because his OWN blurb's
# impact text mentions "Tua Tagovailoa was named the starter" (a related
# but DIFFERENT player). The headline for this style of blurb is always
# self-contained and correctly attributed to its own subject; body/impact
# often discusses other players. A cheap rule that can't verify WHOSE
# name a match belongs to should only match where attribution is certain.
rule_flagged <- bind_rows(
  matched |> filter(str_detect(headline, STARTER_RX)) |>
    mutate(flag_source = "rule", flag_type = "role_change_up", confidence = 1.0, reason = headline),
  matched |> filter(str_detect(headline, BACKUP_RX), !str_detect(headline, STARTER_RX)) |>
    mutate(flag_source = "rule", flag_type = "role_change_down", confidence = 0.6, reason = headline)
)

cli_alert_success("{nrow(rule_flagged)} flagged by the depth-chart rule (no LLM call)")

# ===========================================================================
# 7. LLM CLASSIFICATION -- one call per remaining blurb, Haiku. Falls
# back to no-flag (never errors the whole run) if the key is missing or
# a call fails -- an override candidate silently not firing is a much
# safer failure mode than a broken run or a fabricated flag.
# ===========================================================================

CLASSIFY_PROMPT <- "
You are screening NFL player-news blurbs for a fantasy football model's
live override layer. Most blurbs are routine practice-status tracking or
irrelevant roster churn -- do NOT flag those, the model already sees
official injury/practice data separately.

Flag ONLY if this blurb implies a real change in a fantasy-relevant
player's OPPORTUNITY (snaps, targets, carries, role) -- including a
DIFFERENT player than the one named in the headline (e.g. a backup being
waived can imply a starter's return; a signing can imply a committee
change). Do not flag pure injury-status updates ('limited practice',
'off injury report') even for a relevant player -- that's redundant with
trained data.

Blurb:
Headline: {headline}
Body: {body}
Impact note: {impact}

Respond with EXACTLY this JSON shape, nothing else:
{{\"flag\": true|false, \"affected_player\": \"<name or null>\",
 \"direction\": \"up\"|\"down\"|null, \"confidence\": 0.0-1.0,
 \"reason\": \"<one sentence>\"}}
"

get_anthropic_key <- function() {
  k <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  if (nzchar(k)) return(list(key = k, src = "env"))
  k <- tryCatch(
    system2("security", c("find-generic-password", "-s", "ANTHROPIC_API_KEY", "-w"),
            stdout = TRUE, stderr = FALSE),
    warning = function(w) character(0), error = function(e) character(0)
  )
  if (length(k) > 0 && nzchar(k[1])) return(list(key = k[1], src = "keychain"))
  k <- tryCatch(
    system2("op", c("item", "get", shQuote("Anthropic API Key"),
                    "--fields", "credential", "--reveal"),
            stdout = TRUE, stderr = FALSE),
    warning = function(w) character(0), error = function(e) character(0)
  )
  if (length(k) > 0 && nzchar(k[1])) return(list(key = k[1], src = "1password"))
  NULL
}

ANTHROPIC_MODEL <- "claude-haiku-4-5-20251001"  # cheap/fast -- this is simple screening, not deep reasoning

classify_blurb <- function(headline, body, impact, key_hit) {
  no_flag <- list(flag = FALSE, affected_player = NA_character_, direction = NA_character_,
                  confidence = NA_real_, reason = NA_character_)
  if (is.null(key_hit)) return(no_flag)

  prompt <- str_glue(CLASSIFY_PROMPT, headline = headline,
                     body = coalesce(body, ""), impact = coalesce(impact, ""))

  resp <- tryCatch({
    req <- request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        "x-api-key" = key_hit$key,
        "anthropic-version" = "2023-06-01",
        "content-type" = "application/json"
      ) |>
      req_body_json(list(
        model = ANTHROPIC_MODEL,
        max_tokens = 200L,
        messages = list(list(role = "user", content = prompt))
      )) |>
      req_retry(max_tries = 3, backoff = ~ 2^.x) |>
      req_timeout(30)
    resp_body_json(req_perform(req))
  }, error = function(e) {
    cli_alert_warning("classify_blurb: API call failed ({conditionMessage(e)}) -- treating as no-flag")
    NULL
  })
  if (is.null(resp)) return(no_flag)

  txt <- tryCatch(resp$content[[1]]$text, error = function(e) NA_character_)
  # Haiku wraps JSON in a ```json ... ``` fence despite being told not to
  # (verified 2026-09-09) -- extract the {...} blob rather than assume
  # the response is bare JSON.
  json_txt <- str_extract(txt %||% "", regex("\\{.*\\}", dotall = TRUE))
  parsed <- tryCatch(jsonlite::fromJSON(json_txt), error = function(e) NULL)
  if (is.null(parsed) || !isTRUE(parsed$flag %in% c(TRUE, FALSE))) {
    if (!is.null(parsed) && is.null(parsed$flag)) cli_alert_warning("classify_blurb: unexpected response shape, treating as no-flag: {txt}")
    return(no_flag)
  }
  list(flag = isTRUE(parsed$flag),
       affected_player = parsed$affected_player %||% NA_character_,
       direction = parsed$direction %||% NA_character_,
       confidence = as.numeric(parsed$confidence %||% NA_real_),
       reason = parsed$reason %||% NA_character_)
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

to_classify <- matched |> anti_join(rule_flagged, by = "news_id")
key_hit <- get_anthropic_key()
if (is.null(key_hit)) {
  cli_alert_warning("No Anthropic API key found (checked env, Keychain 'ANTHROPIC_API_KEY', 1Password 'Anthropic API Key') -- {nrow(to_classify)} blurbs will all resolve to no-flag.")
} else {
  cli_alert_info("Anthropic key found via {key_hit$src} -- classifying {nrow(to_classify)} remaining blurbs with {ANTHROPIC_MODEL}")
}

llm_results <- to_classify |>
  rowwise() |>
  mutate(cls = list(classify_blurb(headline, body, impact, key_hit))) |>
  ungroup() |>
  unnest_wider(cls) |>
  filter(flag) |>
  mutate(flag_source = "llm", flag_type = paste0("role_change_", direction))

# The override target is often NOT the blurb's own subject -- e.g. "Willis
# waived... signals Kittle's return" should apply to Kittle, not Willis
# (who's off the team). Verified as a real bug 2026-09-09 (Willis/
# Oladokun/Henderson all got flagged on THEMSELVES despite being waived/
# inactive, with the actual beneficiary named in `reason` but never
# crosswalked). Re-crosswalk `affected_player` through the SAME roster
# lookup and prefer it whenever it resolves; fall back to the blurb's own
# subject only when affected_player is null/unresolved (the common case
# where a player's own move affects their own opportunity, e.g. Jacobs'
# court date, Boutte's role clarity).
if (nrow(llm_results) > 0) {
  llm_results <- llm_results |>
    mutate(affected_nm = normalize_player_name(coalesce(affected_player, ""))) |>
    left_join(rosters |> rename(affected_gsis_id = gsis_id), by = c("affected_nm" = "nm")) |>
    mutate(
      final_gsis_id = coalesce(affected_gsis_id, gsis_id),
      final_name    = if_else(!is.na(affected_gsis_id), affected_player, name_guess)
    )
  n_retargeted <- sum(!is.na(llm_results$affected_gsis_id) & llm_results$affected_gsis_id != llm_results$gsis_id)
  n_unresolved <- sum(!is.na(llm_results$affected_player) & is.na(llm_results$affected_gsis_id))
  cli_alert_info("LLM overrides: {n_retargeted} re-targeted to a named beneficiary, {n_unresolved} named a beneficiary that didn't crosswalk (kept on the blurb's own subject, flagged for review)")
  llm_results <- llm_results |>
    mutate(gsis_id = final_gsis_id, name_guess = final_name)
}

# ===========================================================================
# 8. OUTPUT
# ===========================================================================

# source_player: the blurb's own subject (audit trail) -- for rule-based
# flags this is always the same as the target; for LLM flags it's often
# NOT (see the re-targeting note above), so keeping both makes it
# possible to see at a glance whether an override moved to a different
# player than the headline named.
overrides <- bind_rows(
  rule_flagged |> mutate(source_player = name_guess, unresolved_beneficiary = NA_character_) |>
    select(gsis_id, name_guess, source_player, unresolved_beneficiary,
           flag_source, flag_type, confidence, reason, news_id, published_utc),
  llm_results  |> mutate(source_player = paste0(headline),
                         unresolved_beneficiary = if_else(!is.na(affected_player) & is.na(affected_gsis_id), affected_player, NA_character_)) |>
    select(gsis_id, name_guess, source_player, unresolved_beneficiary,
           flag_source, flag_type, confidence, reason, news_id, published_utc)
) |>
  mutate(season = SEASON, week = WEEK, .before = 1)

dir.create("data", showWarnings = FALSE)
out_path <- sprintf("data/news_overrides_%d_w%02d.csv", SEASON, WEEK)
readr::write_csv(overrides, out_path)

cli_alert_success("{out_path} ({nrow(overrides)} override candidates: {sum(overrides$flag_source=='rule')} rule, {sum(overrides$flag_source=='llm')} llm)")
cli_h1("10i complete")
