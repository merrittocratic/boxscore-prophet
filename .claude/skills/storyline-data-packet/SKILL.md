---
name: storyline-data-packet
description: Given a list of NFL storylines (team/player narratives, e.g. "are the Raiders for real?"), build a fact-based data packet that looks BACK at every game so far this season, then AHEAD to the upcoming opponent, using nflverse, the model's own weekly outputs, and (once built) the internal efficiency-proxy models, formatted as a Cousin Claude handoff. Use when Steve gives storylines for the Thursday preview article (or any write-up) and wants supporting stats pulled together.
---

# Storyline Data Packet

Turn a short list of narrative claims into a sourced data packet Steve
(or Cousin Claude) can write from. This is research, not drafting --
never write the actual column here; that is `/movers-column` or
`/on-the-record`'s job, or Cousin Claude's, depending on which piece
this feeds.

Every packet has two halves, in this order: **Retrospective** (what the
season so far actually shows) then **Look-ahead** (how that profile
matches up with the upcoming opponent). The primary consumer is the
Thursday next-week preview article. Example: "Are the Raiders for
real?" -> pull every Raiders game this season (who they played, how
they won/lost, whether the underlying efficiency backs the record),
then this week's opponent and whether the Raiders' strengths line up
against that opponent's weaknesses or run straight into its strengths.

## Inputs

Steve gives a list of storylines, usually 3-5 -- e.g. "are the Raiders
for real?", "Cincinnati's defense can carry Burrow," "the Chargers look
lost." Default window: retrospective = every completed game this season
through the most recently completed week; look-ahead = the next
scheduled game. Confirm if Steve names a different window, and note a
bye (look ahead to the game after it and say so).

## Process, per storyline

### Part 1 -- Retrospective (season to date)

1. **Identify the game(s) and players the storyline actually turns on.**
   A team-level claim ("Chiefs look scary") still needs a specific
   player or unit to ground it (QB box score, a featured back, a
   defensive unit) -- vague team-level color isn't a data point.
2. **Pull every game so far, not just last week, from nflverse**
   (`nflreadr::load_schedules`, `load_player_stats`, `load_pbp`,
   `load_nextgen_stats`). Build a short game log (opponent, result,
   score, off/def EPA per play, success rate) plus season-to-date
   totals and league rank. Favor EPA/success-rate/explosive-play framing
   over raw counting stats -- it travels better into "why," not just
   "what." Two things make a "for real?" claim concrete instead of
   vibes:
   - **Trend:** is the good (or bad) stuff steady across games, or one
     outlier game carrying the season line? Say which.
   - **Who they did it against:** the quality of opponents faced so far
     (those opponents' own season EPA/play ranks), and a prior-season
     baseline (e.g. last year's team EPA/play) for "return to form" or
     "step back" claims. A 3-0 record against three bottom-10 offenses
     is a different story than 2-1 against contenders.
   Also flag record-vs-underlying mismatches: close-game luck (one-score
   wins), turnover margin, non-offensive TDs -- but only when this
   team's data actually shows them, never as a stock explanation (see
   step 5).
3. **Check the model's own outputs for the completed weeks** --
   `output/10c_scored_slate_<season>_w<week>.csv` (pred_tot, thresholds,
   probabilities), `output/10d_ecr_gap_<season>_w<week>.csv`
   (model_rank vs ecr_rank -- the "model called this before the market
   did" or "the model was skeptical and got it right" nugget), and
   `output/10d_receipts_<season>_w<week>.csv` for graded weeks
   (fp_actual, hit_start/hit_boom). Across multiple weeks, a
   season-long pattern (the model has been higher than ECR on this
   team's WR1 every week and been right) beats a one-week nugget. This is the house's own
   differentiated angle -- always check it, even if the storyline reads
   as purely a box-score story at first.
4. **If a directional efficiency-proxy nugget applies** (once
   [[project_pff_run_grade_crosswalk]]'s cross-position build lands --
   check that memory for what's live), score the relevant player and
   report it as a PERCENTILE within the historical model sample, never
   as a specific grade number, and never naming PFF. See the hard rules
   below -- this is not optional framing, it is a ToS-driven constraint.
5. **If the data contradicts the proposed storyline, say so plainly**
   and lead with what the data actually shows instead of quietly
   dropping the point or forcing the original framing. A corrected
   nugget is usually a BETTER nugget. Two different shapes this takes --
   don't reach for just one of them out of habit (see the 2026-09-18
   Bills/Lions packet, where the model defaulted to inventing a
   turnover/defense correction nobody had proposed, because that was
   the shape of the only example on file at the time):
   - *Model-vs-market correction* (2026-09-16 W1 packet): "Burrow
     exceeded projections" wasn't true, but "the model was skeptical of
     Burrow relative to ECR's #1 ranking, and that skepticism paid off
     while the defense forced 4 turnovers" is a stronger, truer story.
   - *Score-implied illusion correction, no model/ECR angle at all*
     (2026-09-18 Bills 41, Lions 31): the scoreline invites "Allen
     out-threw Goff since Buffalo won." False -- Goff's passing line
     (327 yds, 4 TD, +18.58 EPA, +6.0 CPOE) was a 96th-percentile
     single-game passing performance league-wide; Allen's (91st
     percentile) was excellent but clearly second-best on the field.
     Allen won on rushing TDs, third-down offense, and run efficiency
     -- not by out-throwing the losing QB. The correction here has
     nothing to do with turnovers, defense, or the model/ECR gap; it's
     a plain stat comparison the final score obscures.
   The generalizable move is: find what the *final score itself*
   implies that the underlying box score/EPA/model data does not
   support -- not a stock explanation (turnovers, defense) applied out
   of habit regardless of whether this game's data backs it.
6. **Across the packet as a whole, lean toward model hits but don't
   scrub out a miss.** The model-rank-gap nugget (step 3) is most
   valuable when it shows the model calling something right ahead of
   ECR/consensus -- that's the differentiation story and should be the
   majority of what gets surfaced. But if one of the storylines' games
   also has a clean model miss sitting right there (see the 2026-09-16
   W1 packet's Herbert call: model_rank 1, busted), include it rather
   than dropping it for a cleaner sweep. An all-hits packet reads as
   cherry-picked; one honest miss alongside several hits reads as
   credible. Don't go hunting for a miss to include if the week's data
   doesn't hand you one, and don't manufacture "balance" by padding a
   real hit with false hedging -- the ratio should reflect what
   actually happened, just don't suppress a miss that's already there.

### Part 2 -- Look-ahead (the upcoming opponent)

7. **Identify the next opponent and game context** from
   `load_schedules` (opponent, home/away, rest days, `spread_line` /
   `total_line` -> implied team points). The market line is the only
   game-level expectation in this pipeline -- the model is player-level
   fantasy, not a game-outcome model. Never fabricate a win probability
   or projected score; report the market's number as the market's.
8. **Profile the opponent season to date**, same method as step 2:
   off/def EPA per play, success rate, pass-defense vs run-defense
   splits, explosive plays allowed, quality of their own schedule.
9. **Match strengths against weaknesses.** Line up what the
   retrospective said this team does well/poorly against what the
   opponent allows/takes away (e.g. "Raiders' offense lives on
   explosive passes; this opponent allows the 3rd-fewest explosive
   passes"). One or two sharp matchup points beat a full side-by-side
   table. If the matchup is a genuine test of the storyline ("first
   top-10 defense they've seen"), say that explicitly -- it's the hook
   for the preview.
10. **Model outputs for the upcoming week**, from the freshest
    `output/10c_scored_slate_<season>_w<nextweek>.csv` (Tuesday full run,
    or a later rescore if one has landed -- say which) and
    `output/10g_movers_<season>_w<nextweek>.csv`: the storyline team's
    key players' start/boom chances vs their own baseline, the
    `opp_def_adj_*` matchup read, and the ECR gap if it's available for
    that week. These are forward-looking chances, not results -- label
    them as such.

## Hard rules

- **PFF data/grades/stats never appear in the packet, directly or by
  name**, per [[project_paid_data_options]] -- verified 2026-09-16 that
  this covers raw charted stats too, not just composite grades. Any
  efficiency-proxy nugget is described only as Boxscore Prophet's own
  public-data model; PFF is never mentioned as its source or inspiration
  in anything that could get published.
- Nothing from this skill is a repo content artifact -- per
  `CLAUDE.md`, a fact sheet like this defaults to CHAT OUTPUT ONLY. Do
  not write it to `~/content/draft/` or anywhere in this repo's
  `content/` unless Steve explicitly asks for a persisted file.
- Format for [[reference_cousin_claude]]: self-contained numbers, no
  file:line citations (Cousin Claude has no repo access), organized by
  storyline with each fact tagged by source category (nflverse / model
  / proxy) so Steve can see at a glance what's public-record vs.
  house-differentiated.
- **First use of any advanced/jargon stat in the packet gets a
  plain-language gloss inline** -- one short clause, not a paragraph --
  e.g. "passing EPA (points of value added per throw, vs. what an
  average play in that spot would be expected to do)." Every later
  mention of that same stat in the same packet can go bare. See the
  glossary below for the terms that come up most; write a same-style
  gloss on the fly for anything not listed there.

### Plain-language glossary (reuse this phrasing)

- **EPA (Expected Points Added):** how many points a play added or cost
  the offense, compared to what an average play would be expected to do
  in that same down/distance/field-position situation.
- **CPOE (Completion % Over Expected):** how much better or worse a QB
  completed passes than expected, given how difficult each individual
  throw was (depth, coverage, etc.) -- a QB accuracy stat that adjusts
  for degree of difficulty.
- **Success rate:** the share of plays that gained enough yards to keep
  the offense "on schedule" (roughly 40% of yards-to-go on 1st down,
  60% on 2nd, 100% on 3rd/4th) -- a hit-rate stat, not a big-play stat.
- **Rush yards over expected (NGS):** how many rushing yards a back
  gained beyond what's expected given the blockers and defenders in the
  box on that play, from player-tracking data.
- **DVOA:** a Football Outsiders efficiency metric adjusted for
  opponent, down, distance, and situation. NOT currently pulled anywhere
  in this pipeline -- if a storyline seems to call for it, say so and
  use EPA/success rate instead rather than fabricating a DVOA-like
  number.

- Look-ahead facts use the same no-gambling-language rule as the
  columns: the spread/total is "the market expects," not odds, a price,
  or a pick.

## Output

Chat output, organized by storyline. Each storyline gets two labeled
blocks -- **Looking back** then **Looking ahead: <opponent>** -- each a
short list of sourced bullets, not prose; Steve or Cousin Claude does
the writing. End each storyline with a one-line verdict on the
retrospective question (e.g. "for real: efficiency backs the record" /
"record is ahead of the underlying numbers") and the single matchup
point most worth watching. Close the packet by naming anything that
came back weaker or contrary to the proposed storylines.
