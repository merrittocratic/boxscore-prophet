---
name: on-the-record
description: Draft the weekly "On the Record" grading Substack Note (150-200 words) from last week's 10d receipts and ECR gap. Use when Steve asks for the Wednesday recap, the grading note, or "On the Record."
---

# On the Record (Wednesday Note)

Draft the weekly public grading piece as a short Substack NOTE, not a
full article: what the model said last week, stated as a probability
before kickoff, graded against what actually happened. This is the
accountability piece -- see `CONTENT_GUIDE.md` for the full voice
contract. The output is a DRAFT for Steve to edit; never commit or push.

## Why Wednesday

The Tuesday 23:30 full run is what builds the prior week's receipts, and
by then Monday Night Football is final. A Wednesday note always grades a
fully played-out week off a fresh receipts file.

## Inputs (in priority order)

1. `output/10d_receipts_<season>_w<prevweek>.md` -- REQUIRED. Calibration
   by stated band (stated probability vs. actual hit rate), worst misses
   (highest stated chances that did not hit), longshots that hit (lowest
   stated chances that cleared the bar), and "still on the board" for any
   games not yet final. If this file does not exist, run
   `Rscript R/10d_content_tables.R <season> <prevweek>` first (requires
   the Tuesday full run for `<prevweek>` to have completed -- do not
   run against a week that hasn't fully played out).
2. `output/10d_ecr_gap_<season>_w<prevweek>.csv` -- OPTIONAL. Model rank
   vs. FantasyPros ECR rank (`rank_gap`, positive = model ranked the
   player higher than consensus). Use for at most ONE "here's how that
   compared to what everyone else said" line, in prose. Never reproduce
   the table. Skip this beat entirely if the file is missing rather than
   inventing a comparison.
3. Voice reference: read 2-3 recent pieces in `~/content/published/`
   (and any prior `on_the_record` drafts in `~/content/draft/`) before
   writing. Absorb tone, do not imitate structure verbatim.

## Week 1 exception

There is no prior week to grade in Week 1 -- do not attempt to draft this
note for Week 1. Flag this to Steve and stop rather than fabricating a
note from partial or preseason data.

## Note contract

- 150-200 words, hard range. ASCII only. No em-dashes in any form (not
  the unicode character, not "--" as a stand-in); use a period, comma,
  colon, or parentheses instead.
- Substack Notes render as plain short-form text: no section headers,
  no tables, no bullet-heavy layout.
- Structure: one-sentence headline grade for the week -> the calibration
  read in one plain-English sentence ("when the model said coin flip, it
  hit close to half the time") -> the single most notable miss (player,
  what the model said, what happened) -> the single most notable
  longshot hit -> optional one-line ECR comparison -> optional one-line
  teaser to Thursday's preview. With this little room, one miss and one
  hit is the default; a second miss only if the week was genuinely bad
  and one name would understate it.
- A bad week (more misses than hits, or a calibration band running well
  off) is NOT something to soften or bury. The short format is not an
  excuse to drop the miss -- the miss is mandatory, the ECR line and
  teaser are what get cut for space.
- Every number stated must trace to a row in the receipts file. Never
  round a miss into looking closer than it was.
- Percentages: whole numbers. Translate the headline stat into plain
  English ("hit a little better than one in three," not just "36%").

## Voice guardrails

Full list in `CONTENT_GUIDE.md`. The ones that bite hardest here:

- No gambling language ("odds," "fade," "price," "sharp," "lock").
  Probabilities are "chances."
- The model is "the model," third person -- not "our model."
- Never claim more calibration skill than the receipts file actually
  shows. A longshot hit inside its stated band can be noted as the
  system working as designed; a miss inside a well-calibrated band is
  not the same thing and should not be spun that way.

## Output

Print the note in chat for copy/paste AND write it to
`~/content/draft/w<NN>_on_the_record.md` (zero-padded week, e.g.
`~/content/draft/w03_on_the_record.md`). Steve's content folder,
OUTSIDE this repo -- never write into `content/` here. Report word
count. Surface proposed revisions in chat for approval before editing
the file on any subsequent pass. Never commit or push; Steve handles git.
