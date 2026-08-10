# Boxscore Prophet -- Orientation Brief

Written 2026-08-04 as a handoff/orientation document. Read this
first, then `building_in_public_log.md` for the story, then the
README decision log (D1-D23) for full technical rationale.

## What this is

A fantasy football (season-long PPR) probability engine plus a
content operation under the Merrittocracy brand. It does NOT publish
point projections. For every RB, WR, QB, and TE each week it
publishes two probabilities:

- P(startable week): P(FP >= 15) for RB/WR, >= 20 for QB, >= 12 for TE
- P(boom week): P(FP >= 20) for RB/WR, >= 25 for QB, >= 17 for TE

Distribution: Tuesday boards + deep dives on Substack, boards and
alerts on X, re-scored during the week as injury news lands. 2026 is
the first live season (launch = Week 1, September).

## How the model works (one paragraph per layer)

1. **Feature tables** (per position, 2014-2025, nflverse data):
   rolling volume/efficiency features, defense adjustments, injury
   state machine (Friday-lock practice reports), Vegas opening lines
   (spread + implied total). Hard rule: nothing enters the trained
   feature set unless it is point-in-time reconstructable as of
   Friday lock.
2. **Two models per position, never one**: a volume model and a
   per-touch efficiency model (D1). QB is two-component pass/rush
   because a single total underprices scramblers by ~2.7 FP/game.
   TE is a WR-spine derivative with a targets-per-snap role feature
   (a TE on the field may be blocking; a WR is running a route).
3. **Engine**: nested-CV-tuned LightGBM ("3A-v2") won a five-way
   bake-off (D6). Intervals come from per-fold power-law conformal
   construction scaled by predicted volume (D5; deployment seam
   fixed in 06b0 after a train/serve skew was caught).
4. **FP translation**: regression bridge to fantasy points plus
   empirical simulation for the tails -- no parametric distribution
   (D11). Simulation order is part of the published-number contract
   (adding TE once shifted QB numbers via the shared RNG stream).
5. **Recalibration maps** (the "last inch", D13): per
   position-threshold Platt/isotonic variants, several Vegas-aware
   and volume-conditional. Deployed methods are in
   `data/*fp_recal_maps.rds`. Maps are FROZEN for 2026 (D23: weekly
   refitting won 7 of 8 pools in a 2025 pretend-deploy but missed
   the pre-committed 0.5pp bar; re-run before 2027).
6. **Weekly runner (10-series)**: builds slates, scores them,
   renders content boards (start/boom/streamer), rookie tracker,
   and a weekly self-evaluation scorecard (10f) with a frozen
   watch-cell registry and two pre-committed drift alarms. Exact-
   match gates reproduce backtest logic at |diff| = 0 before
   anything ships.

## The house discipline (why the numbers are trustable)

- Every experiment pre-registered: expectation, decision rule, and
  pass/fail bars stated before code runs. Bars never move after data
  arrives; overrides are signed, not laundered.
- Walk-forward everywhere; the model only ever sees Friday-knowable
  information. Beat-reporter/text signal is banned from training
  (not reconstructable) -- it lives in a live override layer whose
  value gets graded in-season.
- Nulls are published with receipts. The pre-registered ablation
  ladder closed 2026-08-01: five rungs, two shipped (injury states;
  Vegas, three layers deep), three published nulls (weather, OL /
  opponent front, rookie priors -- all already priced by the market
  or handled by existing layers).
- Known limitations are documented, not hidden: star shrinkage
  (player effects deferred), two residual game-script cells (QB in
  projected-close games, TE as big underdogs), QB-context watch
  cells (D21/D22) parked below their bars in the 10f registry.

## Who does what

- **Steve** (owner): all decisions on bars, ships, and spends.
  Nothing publishes without his explicit yes.
- **Manfred** (Claude Code on the laptop = the primary developer):
  feature work, experiments, content drafts. Laptop is test/stage.
- **Earnest** (OpenClaw agent on the Mac Mini = production): runs
  the Tuesday cadence, re-scores as injury reports land, posts
  drafts to Steve via Telegram for approval. Also a public-facing
  character in the content.
- Single-writer rule: `data/deployment_params.rds` and
  `data/deploy_models/` are committed only by Earnest in-season.
- Git: no auto-commit/auto-push, ever. Pull before starting work.

## Content workflow (movers column)

The primary weekly Substack vehicle is the start/sit MOVERS column:
players whose P(start) moved most vs their own trailing published
baseline. Split of responsibilities:

- Data is a repo artifact. `R/10g_movers_table.R` (wired into
  `weekly_run.sh`, both modes) writes `output/10g_movers_<wtag>.csv`;
  Earnest surfaces the top movers to Telegram via the digest
  (`refresh_latest.sh` manifest + `earnest_notify.sh`).
- The written column is NOT a repo artifact. The `/movers-column`
  skill drafts it to `~/content/draft/w<NN>_movers_column.md`
  (zero-padded week, no season prefix) -- Steve's content folder,
  outside this repo. A `.gitignore` rule blocks stray
  `content/*_movers_column.md` from being committed here; the one
  exception is `content/2025_w15_movers_column.md`, kept tracked as
  the historical demo.

## FOR EARNEST: team-code fix landed 2026-08-09 (read before arming)

Upstream regression, caught on a 2026 W1 slate build. The 2026
nflverse WEEKLY ROSTER release codes Arizona `AZ`; schedules code it
`ARI`. 2024 and 2025 rosters both used `ARI`, so this is new drift.

Impact if unpatched: `build_exante_roster` sets `posteam` from the
roster, then `inner_join`s to schedule-derived `games_long` -- so
every Cardinal was SILENTLY dropped from every slate. 28 skill
players, including Trey McBride, Marvin Harrison Jr., and James
Conner. No warning, no row-count anomaly. It would also persist once
games are played, because `posteam` is
`coalesce(posteam_now, posteam_hist)` and the roster's `AZ` keeps
winning over PBP's `ARI`.

Note `nflreadr::clean_team_abbrs()` does NOT fix this -- it returns
`AZ` unchanged.

Fix in `R/10b_roster_helpers.R`:
- `TEAM_CODE_ALIASES` + `normalize_team_codes()`, applied to `posteam`
  before the schedule join. An alias only fires when its TARGET is a
  valid schedule code and the original is not, so it can never rewrite
  a code the schedule already uses.
- A new warning fires when any roster row carries a team code absent
  from the SEASON's schedule vocabulary (season, not week -- bye-week
  teams must not trip it). The silence was the real bug; the alias map
  is just today's instance.

Verified: all four hindcast gates still pass at max |diff| = 0e+00
(2025 W15, RB/WR/TE/QB). 2026 W1 slate rows 881 -> 908, and all four
positions now carry 32 distinct `defteam` values instead of 31.

ACTION WHEN ARMING: after `git pull`, run a 2026 W1 slate build and
confirm 32 distinct `defteam` per position and no team-code warning.
If a DIFFERENT team goes missing later in the season, the warning now
names it -- add it to `TEAM_CODE_ALIASES` rather than working around
it downstream.

## Current state (2026-08-06)

- 2026 rollover committed; 2025 opener backfill done (Vegas join 96%).
- All four positions content-ready; boards validated on 2025
  hindcast weeks (W13-15).
- 10f weekly eval + watch registry live in the Tuesday cadence.
- Movers pipeline (10g) built, wired into the runner, and pushed;
  smoke-tested on 2025 W13-15 (deltas center ~0, median |move| 3pp).
  NOT yet exercised on a live multi-week ledger.
- Earnest's cron is BUILT but NOT ARMED. September pass before
  Week 1: re-run rookie crosswalk (~12 pending GSIS IDs), confirm
  ECR aliases, remove any legacy direct `weekly_run.sh` cron lines,
  run `earnest_setup.sh --arm`, babysit the first Tuesday (confirm
  the movers digest renders on a real manifest).
- Season teaser committed at `content/2026_season_teaser.md` with
  board chart (`content/teaser_charts.R`); X thread drafted at
  `content/2026_season_teaser_x_thread.md`. Both awaiting Steve's
  final review + the live Substack URL.
- Paid data: ECR subscription live (renews 2027-07-18); odds data
  deliberately free (opening lines); no injury feeds, ever.

## September production arming checklist

Run these steps ON THE MAC MINI itself, or from an SSH shell into the
Mac Mini. Do not run them on Manfred/laptop; they validate and modify
production-local cron, keychain, repo state, and OpenClaw delivery.

Recommended sequence:

1. SSH to the Mac Mini, then `cd ~/.openclaw/workspace/boxscore-prophet`
2. `git status` should be clean; `git pull --ff-only` before touching cron
3. Remove any old cron entries that call `scripts/weekly_run.sh`
   directly, they bypass the production wrapper
4. Run `bash scripts/earnest_setup.sh` and confirm preflight passes
5. Re-check rookie crosswalk / pending GSIS IDs and ECR alias sanity
5b. Team-code check (see the Earnest section at the top of this file):
   build a 2026 W1 slate and confirm 32 distinct `defteam` per
   position and no team-code warning in the log
6. When ready to arm for the live season, run
   `bash scripts/earnest_setup.sh --arm`
7. Babysit the first Tuesday full run: confirm `earnest_cron.sh`
   commits/pushes outputs, refreshes `output/latest/`, and sends the
   Telegram summary/media cleanly

Managed cron block that `earnest_setup.sh --arm` installs:

```cron
# BEGIN boxscore-prophet cadence (managed by earnest_setup.sh)
30 23 * * 2  bash /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/scripts/earnest_cron.sh full    >> /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/logs/cron.log 2>&1
0  15 * * 4  bash /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/scripts/earnest_cron.sh rescore >> /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/logs/cron.log 2>&1
0  15 * * 6  bash /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/scripts/earnest_cron.sh rescore >> /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/logs/cron.log 2>&1
0  8  * * 0  bash /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/scripts/earnest_cron.sh rescore >> /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/logs/cron.log 2>&1
0  15 * * 1  bash /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/scripts/earnest_cron.sh rescore >> /Users/merrittocracyclaw/.openclaw/workspace/boxscore-prophet/logs/cron.log 2>&1
# END boxscore-prophet cadence
```

10g movers automation status:

- `weekly_run.sh` runs `R/10g_movers_table.R` on both `full` and
  `rescore`
- `refresh_latest.sh` promotes the latest movers CSV to
  `output/latest/movers.csv`
- `earnest_notify.sh` includes top movers up/down in the Telegram
  summary
- The written MOVERS column draft is still a separate content step via
  the `/movers-column` skill to
  `~/content/draft/w<NN>_movers_column.md`; cron does not auto-draft
  the Substack post

## Where to read deeper

- `README.md` -- architecture, repository map, decision log D1-D23
  (the full technical record; D18/D19 = Vegas, D23 = frozen maps).
- `building_in_public_log.md` -- the narrative version, one entry
  per roadmap bend, written for eventual publication.
- `R/` -- numbered pipeline stages (03x bake-off, 04x WR, 06x FP
  translation, 08x-09x QB, 10x runner, 11x injury, 12x TE, 13x
  Vegas, 14x-16x published nulls, 17a refit experiment).
- `output/` -- every experiment's receipts as CSVs; `data/` --
  frozen tables, deployed models and maps.
