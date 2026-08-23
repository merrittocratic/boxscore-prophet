---
name: on-the-record
description: Draft the weekly "On the Record" grading column for Substack from last week's 10d receipts and ECR gap. Use when Steve asks for the Tuesday recap, the grading column, or "On the Record."
---

# On the Record (Tuesday Grading Column)

Draft the weekly public grading column: what the model said last week,
stated as a probability before kickoff, graded against what actually
happened. This is the accountability piece -- see `CONTENT_GUIDE.md` for
the full voice contract. The output is a DRAFT for Steve to edit; never
commit or push.

## Inputs (in priority order)

1. `output/10d_receipts_<season>_w<prevweek>.md` -- REQUIRED. Calibration
   by stated band (stated probability vs. actual hit rate), worst misses
   (highest stated odds that did not hit), longshots that hit (lowest
   stated odds that cleared the bar), and "still on the board" for any
   games not yet final. If this file does not exist, run
   `Rscript R/10d_content_tables.R <season> <prevweek>` first (requires
   the Tuesday full run for `<prevweek>` to have completed -- do not
   run against a week that hasn't fully played out).
2. `output/10d_ecr_gap_<season>_w<prevweek>.csv` -- OPTIONAL. Model rank
   vs. FantasyPros ECR rank (`rank_gap`, positive = model ranked the
   player higher than consensus). Use for ONE specific "here's how that
   compared to what everyone else said" callout per column, restated in
   prose. Do not reproduce the table -- boards/rankings stay scarce, same
   rule as the movers column. Skip this beat entirely if the file is
   missing rather than inventing a comparison.
3. Voice reference: read 2-3 recent pieces in `~/content/published/`
   (and any prior `on_the_record` drafts in `~/content/draft/`) before
   writing. Absorb tone, do not imitate structure verbatim.

## Week 1 exception

There is no prior week to grade in Week 1 -- do not attempt to draft this
column for Week 1. Flag this to Steve and stop rather than fabricating a
column from partial or preseason data.

## Column contract

- 400-700 words, ASCII only (no unicode dashes/quotes/arrows).
- Structure: one-sentence headline grade for the week -> the calibration
  read in plain English (are stated bands running hot or cold, stated
  simply, e.g. "when we said 'coin flip,' it hit close to half the
  time") -> 2-3 named misses stated plainly (player, what we said, what
  happened) -> 1-2 named longshot hits -> optional one-line ECR
  comparison if the gap file supports it -> one-line teaser to Saturday's
  movers column.
- A bad week (more misses than hits, or a calibration band running well
  off) is NOT something to soften or bury. State it as plainly as a good
  week. This column's entire value proposition is that it doesn't flinch.
- Every number stated must trace to a row in the receipts file. Never
  round a miss into looking closer than it was.
- Percentages: whole numbers, no decimals. Translate at least the
  headline stat into plain English ("hit a little better than one in
  three," not just "36%").

## Voice guardrails

Full list in `CONTENT_GUIDE.md`. The ones that bite hardest here:

- No gambling language ("odds," "fade," "price," "sharp," "lock").
  Probabilities are "chances."
- The model is "the model," third person -- not "our model."
- Never claim more calibration skill than the receipts file actually
  shows. If a band is off, say it's off; do not spin a miss into a
  "the process was still right" line unless the receipts data actually
  supports that read (e.g., a longshot hit inside its stated band is a
  fine thing to note as the system working as designed -- a miss inside
  a well-calibrated band is not the same thing and should not be
  described the same way).

## Output

Write to `~/content/draft/w<NN>_on_the_record.md` (zero-padded week, no
season prefix, e.g. `~/content/draft/w02_on_the_record.md`). Steve's
content folder, OUTSIDE this repo -- do not write into `content/` here.
Report word count. Surface proposed revisions in chat for approval before
editing the file on any subsequent pass. Never commit or push; Steve
handles git.
