---
name: storyline-data-packet
description: Given a list of weekly NFL storylines (team/player narratives, e.g. "the Giants look strong"), pull nflverse box scores, the model's own weekly outputs, and (once built) the internal efficiency-proxy models to build a fact-based data packet for a write-up, formatted as a Cousin Claude handoff. Use when Steve gives a list of storylines for a wrap-up column and wants supporting stats pulled together.
---

# Storyline Data Packet

Turn a short list of narrative claims into a sourced data packet Steve
(or Cousin Claude) can write from. This is research, not drafting --
never write the actual column here; that is `/movers-column` or
`/on-the-record`'s job, or Cousin Claude's, depending on which piece
this feeds.

## Inputs

Steve gives a list of storylines, usually 3-5, tied to a specific week
-- e.g. "the Giants looked strong," "Cincinnati's defense can carry
Burrow," "the Chargers looked lost at home." Confirm season/week if not
stated (default to the most recently completed week).

## Process, per storyline

1. **Identify the game(s) and players the storyline actually turns on.**
   A team-level claim ("Chiefs look scary") still needs a specific
   player or unit to ground it (QB box score, a featured back, a
   defensive unit) -- vague team-level color isn't a data point.
2. **Pull the box score and game context from nflverse**
   (`nflreadr::load_schedules`, `load_player_stats`, `load_pbp`,
   `load_nextgen_stats`). Favor EPA/success-rate/explosive-play framing
   over raw counting stats where it's available -- it travels better
   into "why," not just "what." A same-season or prior-season baseline
   (e.g. team offensive EPA/play across all of last year) is what makes
   "return to form" or "step back" claims concrete instead of vibes.
3. **Check the model's own outputs for that week** --
   `output/10c_scored_slate_<season>_w<week>.csv` (pred_tot, thresholds,
   probabilities), `output/10d_ecr_gap_<season>_w<week>.csv`
   (model_rank vs ecr_rank -- the "model called this before the market
   did" or "the model was skeptical and got it right" nugget), and
   `output/10d_receipts_<season>_w<week>.csv` if that week has been
   graded (fp_actual, hit_start/hit_boom). This is the house's own
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

## Output

Chat output, organized by storyline. Each storyline gets a short list
of sourced bullets, not prose -- Steve or Cousin Claude does the
writing. Close by naming anything that came back weaker or contrary to
the proposed storyline, not just the supporting facts.
