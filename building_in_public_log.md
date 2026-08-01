# Building in Public Log

Narrative-grade decision log: the moments worth writing about, with the
receipts pinned. Technical detail lives in README decision entries; this
file records why the roadmap bent where it did.

## 2026-07-26: The null we PAID to publish -- weather is already in the price

Rung 3 (weather) closed the same day rung 2 shipped, and the two
verdicts are a matched pair: rung 2 found the market knows things our
models did not; rung 3 found that once you let the market in, it has
already told you about the weather.

We fetched five seasons of kickoff-hour forecasts -- forecasts as of
lock, never observed weather, the same discipline the golf stack
taught us -- and measured whether wind, cold, or rain move anything
our Vegas-aware chain gets wrong. Pre-stated expectation: mostly
absorbed, because a 15-mph-wind game opens with a lower total and our
chain now reads totals. Pre-committed bars, wider than rung 2's out of
respect for a five-season window. Result: nothing fires. The null is
published, no weather feature enters the models, and the slate's
weather stays what it was -- context for readers.

One cell gets a bookmark rather than a patch: WR starts in 15+ mph
wind read 6.6 points cold, on a sample (52 windy games) below the
floor we committed to before looking. The bar does not move after the
data arrives. The archive grows ~10 windy games a year; the question
becomes answerable honestly around 2027. That is the difference
between a finding and a temptation.

## 2026-07-26: Shipping the market -- three layers deep, with a gate that kept saying no

The Vegas rung shipped today, and the story of the ship pass is the
story of a closure gate refusing to be satisfied -- twice -- and being
right both times.

We pre-committed the finish line before integrating anything: the
published probabilities had to be conditionally honest by Vegas bucket
(no cell off by 3+ points). Attempt one: the efficiency features from
the ablation. The gate said no -- and the investigation found that
touchdowns-per-EPA is itself environment-dependent: hold a player's
EPA and touches fixed, and he scores ~0.3 more fantasy points in a
projected shootout than the translation curve expects. More red-zone
trips per unit of EPA. No efficiency feature can ever see that, so the
implied total went into the translation regression. Attempt two: the
gate said no again -- game-script cells remained, built from
compounded sub-threshold biases (big-underdog RBs over-projected by
0.8 carries, favored QBs by a dropback) that no single layer owned.
The recalibration layer -- the designated principled finish -- gained
Vegas-aware maps under an extended judge, with a twist we want on the
record: the second candidate round was declared TERMINAL before it
ran. Adaptive iteration on a judge is a slope; you stay honest by
announcing where you stop.

Final score: 32 dishonest cells down to 2, both under 4 points, both
published as known limitations (QB starts in projected-close games,
TE starts on big underdogs -- game-script shape, a future ladder
family). Six of eight deployed recalibration maps now read the market.
The remaining closer-line residue is the measured price of using free
opening lines: we know exactly what late-week information costs us,
and it is not worth $100 a month.

## 2026-07-19: The cheap control that wasn't -- Vegas knows something our models don't

Rung 2 of the ablation ladder was pre-registered as the boring one:
"Vegas lines: the cheap control. Expect flat RMSE on the solved
component; run it to have the receipt." We ran it to have the receipt.
The receipt says we were wrong, and the way we were wrong is precise.

The volume models ARE flat against Vegas -- the decomposition's claim
that volume is solved survived contact at all four positions. But the
per-touch EFFICIENCY layer carries a clean monotone bias in implied team
total: the models underpredict scoring environment they cannot see. A QB
in a 27-point-implied game outperforms his prediction by +2.4 EPA on
average; in an 18-point-implied game he underperforms by -2.7. That
gradient flows through the simulation into the published tails, where
high-total games understate start/boom odds by 4-12pp and low-total
games overstate them. Vegas prices the joint environment -- own-offense
quality, matchup, pace -- and our defense adjustments alone do not.

The A/B (spread + implied total added to the efficiency features only,
everything else frozen and verified to reproduce the shipped models at
machine precision): all four positions pass the pre-committed bar, with
residual gradients shrinking 61-80% while intervals get slightly
NARROWER. Honest and sharper at once -- the feature is informative, not
a width tax.

One discipline note: the ablation above used closing lines, which cannot
be reconstructed at Friday lock, and the pre-registered rule is
absolute: not point-in-time reconstructable, not in the trained feature
set. So before spending a dollar on timestamped odds archives, we
tested the free variant: OPENING lines, posted Sunday night -- stale by
up to a week, but unambiguously known at our lock.

The openers kept the signal. WR passed at 85.6% gradient shrink
(better than its closing-line arm), QB at 69.1% (gave back less than a
point), RB at 64%. TE missed the pre-committed 50% bar at 49.3% -- and
we are publishing that as the FAIL it is, with the owner's decision
layered explicitly on top: TE ships anyway, because the alternative was
paying ~$100 to move an estimated sub-point of shrink, which is buying
a pass mark rather than a better product. The bar stays where it was
pre-registered; the decision to override it is signed and dated rather
than laundered through a rounded number.

Total spent on the data that fixes a 12-point calibration hole: $0.

## 2026-07-19: The fourth position -- why TEs are not small WRs

TE was the neglected position, and the temptation was to fold it into the
WR model with a position flag. The feasibility diagnostic (house rule:
data before model work) said no, for a structural reason: 26.5% of TE
weeks with 50%+ offensive snaps produce fewer than 3 targets, versus
10.7% for WRs. A WR on the field is running a route; a TE on the field
might be a sixth lineman. Snap share -- a load-bearing WR feature --
does not imply target volume for TEs. The fix is a role feature (rolling
targets-per-snap) built on the snaps table so the zero-target blocking
weeks the outcome table never sees still enter the window.

Two more things the data insisted on: TE thresholds are 12/17, not 15/20
(the WR cuts hit at barely half the reference rates), and the "TE booms
are TD-gated" intuition is FALSE once thresholds are rate-matched --
boom-without-a-TD rates are identical to WR (3.0% vs 2.8%). We published
the failed expectation with the receipt, per house rule.

The build itself was the frozen WR chain end to end: rubric passed at
-0.4pp, simulation beat the normal approximation by more than half, the
conditional recalibration bake-off picked volume-aware maps under the
unchanged judge, and the slate gate reproduced the frozen table at
|diff| = 0 for three straight weeks.

One lesson for the contract with readers: inserting the TE simulation
into the scoring script BEFORE QB shifted the shared random-number
stream and moved an already-published QB probability 2.6pp -- enough to
trip the pre-committed 10pp reconciliation flag. Published numbers must
be invariant to adding a position, so TE simulates last, and simulation
order is now an explicit part of the published-number contract. The
reconciliation gate caught it, again, on a failure mode nobody designed
it for.

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

## 2026-07-18: Ladder rung 1 -- the injury state machine ships for RB (and the WR null)

The first pre-registered feature family went through the frozen harness
end to end in one day. Order of operations, receipts at every step:

1. DIAGNOSE FIRST (11a): before writing a single feature, we measured
   the shipped volume model's residuals inside injury states built from
   OBSERVED absences. RB confirmed the pre-registered thesis: the week a
   higher-usage back goes down, his backup is underprojected by +2.55
   touches, with clean dose-response in the vacated share. Returning
   backs are overprojected by -1.24 (stale rolling shares meet the snap
   ramp). WR came back flat everywhere (vacated targets scatter across
   the route tree), and the QB check found ~1 dropback of signal against
   a 9-dropback noise floor. The family was always an RB volume story.

2. EX-ANTE LAYER (11b): weekly report sequences -- not within-week
   practice logs, which the archive does not carry -- with a hard
   Friday-lock rule: report rows modified after lock (7.1% of 2014-2024,
   mostly Saturday downgrades) are masked. The Friday-knowable version
   of the fresh-shock state shows +3.83 touches of bias -- HIGHER than
   the observed-state number, because a shock official by Friday is the
   certain kind. What is not knowable Friday belongs to the live
   override layer, never the trained features.

3. A/B UNDER THE FROZEN RUBRIC (11c/11d): same folds, same tuning
   procedure, injury features into the volume model only. RB verdict:
   fresh-shock bias +3.83 -> +0.96, volume RMSE on those weeks -14%,
   return-week bias halved, steady state untouched, pooled coverage and
   the low-usage veto unchanged, widths a hair sharper. Ships. WR
   verdict: nothing moved -- the null, published with its receipt.

4. INTEGRATION with the same paranoia as the original runner: one shared
   implementation computes the states for training and for the slate,
   and the slate builder's exact-match gate now includes the seven
   injury columns -- passing at |diff| = 0 on four hindcast weeks.

The cadence note that matters for readers: injury designations do not
exist on Tuesday. The Tuesday board scores every back as if healthy;
the injury signal enters at the Thursday/Saturday/Sunday re-scores as
reports land -- which is also exactly how the training data saw the
world at Friday lock. The Sunday-morning board is the one that knows
who is out.

## 2026-07-18: The reconciliation gate that found a train/serve skew

10c (score a slate end-to-end, reconcile against the backtest chain)
shipped with pre-committed bounds: r >= 0.95 per position-threshold,
mean signed diff within +-2pp, no unexplained row over 10pp. QB passed
outright. RB and WR breached -- r down to 0.87, nineteen rows past
10pp -- and the diagnosis was better than a pass would have been.

Two causes, both structural, neither a bug. First, the backtest scaled
RB/WR interval widths by OBSERVED test-week volume; deployment scores
before kickoff and can only use predicted volume. Raw probability
diffs correlated -0.6 to -0.8 with the observed-minus-predicted volume
gap: every flagged RB had a gap of six-plus opportunities (predicted 9,
got 18). The backtest had quietly been conditioning on game script.
Second, the recalibration maps -- isotonic step functions with 20-30pp
cliffs -- amplified those few-pp raw diffs into 25pp final diffs.

The deeper find: the deployed maps were FIT on probabilities from
obs-vol intervals but APPLIED to probabilities from pred-vol intervals.
A genuine train/serve skew, hiding inside a documented seam.

The fix was not to widen the bounds. We rebuilt the translation chain
on deployment-consistent inputs (06b0 rescales every backtest fold
interval to pred-vol scaling; 06b/06c rerun downstream), and let the
frozen 6c bake-off re-judge. The step maps LOST to smooth maps on the
honest inputs -- their wins had been partly fitting the scaling
mismatch. Rerun reconciliation: 18 of 18 bound cells pass across three
hindcast weeks (r 0.97-0.99, means under 2pp), one explainable rookie
row at 10.05pp out of ~930 comparisons.

Lesson worth publishing: a reconciliation gate is not a formality --
ours caught the model being graded on information it will never have
on a Sunday morning, and the pre-committed judge, rerun on honest
inputs, reversed its own earlier pick.

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

## 2026-08-01: Rung 4 comes back null, and the trenches tell one story

The OL/opponent-front rung published its null under the same locked
rule as weather: nothing at the pre-committed bars. The interesting
part is the shape of the nothing. Every axis that measured trench
LEVELS -- how good the line is, how fierce the front is -- came back
flat, which is the market and the defensive adjustments doing their
job. The only lean in the table is a family of QB cells that are
really one cell wearing four hats: quarterbacks behind compromised
lines (a broken starting five, a high sack-rate-allowed line, a
sack-generating opponent) run about -0.8 to -1.4 EPA of overprediction,
all below the 2.0 QB bar, all drawing from the same player-weeks. The
pre-registered favorite (heavy boxes suffocating RBs beyond what the
adjustments know) is dead -- flat to positive. Watch item recorded;
any future QB-specific look must rebuild continuity ex-ante from
Friday-lock injury designations first, because this diagnostic sized
the observed state, same as the injury rung did before its build.
Full detail: README D21.
