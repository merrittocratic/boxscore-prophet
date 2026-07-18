# Building in Public Log

Narrative-grade decision log: the moments worth writing about, with the
receipts pinned. Technical detail lives in README decision entries; this
file records why the roadmap bent where it did.

## 2026-07-18: The decomposition picks the roadmap (ablation ladder pre-registration)

The pipeline reached content-ready on all three positions with every
architecture decision graded by a pre-committed judge. The next question
-- "what data do we add?" -- gets the same discipline. The error
decomposition, not the data-source menu, picks the order.

### What the decomposition says

- Volume is the solved component (models calibrated, conditional recal
  honest). Features that mostly sharpen volume (Vegas lines, rest/travel)
  are cheap controls, expected flat.
- Per-touch efficiency context is the open frontier (opponent quality
  beyond current defense adjustments, environment).
- The genuinely weak population is USAGE SHOCKS: a depth-chart change
  (injury, benching) that the rolling features can only see one week
  late. The 2024 McCaffrey -> Jordan Mason share shock is the archetype.
  Note precisely: post-recalibration the low-usage strata are honest
  (within ~1pp) ON AVERAGE -- the failure mode is the transition week,
  not the steady state.
- Star shrinkage is a known limitation with a known fix (player effects)
  deliberately deferred.

### Pre-registered ablation ladder (LOCKED before any family is coded)

One family at a time, each scored on the FROZEN rubric: 80% coverage
primary, sharpness tiebreak, low-usage veto intact, walk-forward folds
unchanged. Every feature must be reconstructable as of Friday lock --
if it cannot be point-in-time reconstructed, it does not enter the
trained feature set, period.

1. Injury practice-report state machine (nflreadr load_injuries):
   DNP/Limited/Full progression features for the player AND the
   depth-chart players above him. Targets the usage-shock population
   directly; historically archived, so walk-forward clean. First on the
   ladder because it is the only family aimed at the weak population.
2. Vegas lines (spread, total, implied team total from load_schedules):
   the cheap control. Expect flat RMSE on the solved component; run it
   to have the receipt.
3. Weather (Open-Meteo historical FORECAST archive, 2021+): thin
   post-prediction adjustment layer, NOT feature-table surgery (frozen
   tables stay frozen; the layer maps (prediction, forecast wind, temp,
   dome) -> adjustment, walk-forward on 2021-2025). Mechanism: wind ->
   volume mix; extreme cold+wind -> per-touch efficiency drag. Modest
   coefficient expected; December content payoff.
4. Opponent front / OL context (beyond current def_*_epa_adj).
5. Rookie prior enrichment.
6. Rest / travel / short week: real, tiny, cheap -- batched with 2.

NGS tracking features: parked pending a sourcing check (point-in-time
reconstructability unclear).

External benchmark (aspirational): props-market closing lines as the
toughest baseline. OPEN SOURCING QUESTION -- historical player-prop
closing lines are paywalled/patchy; benchmark only becomes a claim if a
clean archive is found.

### The text-data boundary (decided)

Beat-reporter signal (committee chatter, snap-count plans) genuinely
moves usage but CANNOT be backtested point-in-time without heroic
reconstruction. Decision: it lives in the ROUTER as a live override
layer (Earnest's X monitoring feeds depth-chart shocks to 10b), never
in the trained feature set. The walk-forward stays clean; the override
layer's value gets measured in-season by logging every override and
grading the counterfactual.

### The narrative-check series (content, not modeling)

Contract-year, revenge-game, primetime effects: run each through the
fold harness, publish the results INCLUDING the nulls. "We tested
revenge games so you don't have to."

## 2026-07-18: The gate that caught a ghost roster (and the A/B that sized it)

The slate builder's exact-match validation gate -- built to prove the
deployment feature carry-forward reproduces the frozen backtest logic --
caught something else entirely: the WR feature table contained 3,734
NA-player pseudo-rows (~17%), one per team-game, from unattributed
targets slipping through an NA in the roster ID list. Those ghost rows
had been part of WR model training and interval calibration all along.

Instead of assuming the effect was small OR panic-rebuilding, we ran the
house play: a pre-committed A/B (filtered refit vs shipped, identical
hyperparameters, identical evaluation rows, decision rule stated before
the run). Verdict: immaterial -- 80% coverage moved +0.53pp pooled,
+0.15pp on the low-usage veto stratum, width under +2%, and the ghost
rows turned out to be mildly STABILIZING calibration mass. The shipped
chain stands; the filter gets fixed in the next feature-table version.

Two lessons worth publishing: validation gates find bugs they were not
built for, and "measure it before you rebuild it" turned a scary-looking
17% contamination number into a half-point of nothing.

## 2026-07-17: Two rosters survive, two engines do not

The two-product architecture resolved: the upside engine (hierarchical
Bayes) lost its deployment case at both positions -- WR on structural
grounds (D14), RB by pre-committed veto (D16: more honest on streamers,
but decisively less sharp, and discrimination is what a streamer board
is for). The two-product concept survives as a roster split over single
engines. The veto's real payload was finding the shipped chain's
streamer stratum was mis-cut (8 -> 10); fixing the cut closed a +3.25pp
bias to -0.3pp. Full detail: README D13-D16.
