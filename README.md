# boxscore-prophet

Calibrated weekly threshold probabilities for fantasy football, built on EPA
interval models. The published numbers are **P(FP >= 15)** (the start/sit
decision) and **P(FP >= 20)** (the boom indicator), PPR scoring, season-long
audience first.

The EPA models are infrastructure. The probabilities are the product.

## Architecture at a glance

```
  play-by-play (nflreadr)
        |
        v
  [1] Feature layer (frozen)          rb_feature_table v2.0 / wr_feature_table v1.0
        |                             efficiency and volume kept separate
        v
  [2] Rubric (frozen)                 204 walk-forward folds, pre-committed
        |                             decision rule, low-usage veto
        v
  [3] Bake-off  ---------------->     WINNER: 3A-v2 (tuned LightGBM +
        |                             power-law conformal intervals)
        v
  [4] WR clone of 3A-v2               same construction, WR features
        |
        v                             RB + WR = the projection spine
  [6] EPA -> PPR translation          per-position OLS + empirical simulation
        |                             (06 normal approx superseded by 06b)
        v
  P(FP >= 15), P(FP >= 20)            per player per week, calibration-checked
```

Parked branches: 3C hierarchical Bayes as an RB upside model (see two-product
architecture below); stadium/weather features (deferred after content pivot).

## The modeling problem

A player's weekly fantasy outcome decomposes into **volume** (opportunities:
carries + targets) times **efficiency** (EPA per opportunity). These are
modeled separately and never collapsed, because they have different dynamics:
volume is role-driven and relatively stable; per-touch efficiency is noisy
and its variance depends on usage tier. The models predict *intervals*, not
points, because the product is a probability -- and an honest probability
requires an honest distribution, not just an accurate center.

## Decision log

Every consequential decision, with the reason it was made. Ordered roughly by
pipeline stage.

### D1. Efficiency and volume are separate models; grouping keys never collapsed

`player_id / defteam / season / week` are preserved intact everywhere, and
`epa_per_opp_obs` is never pre-multiplied into totals in the feature layer.

**Why:** Collapsing the nesting would foreclose hierarchical models (random
effects need the keys), and pre-combining eff x vol would hide which component
a calibration failure comes from. Every diagnosis in this project (committee-
back undercoverage, WR sigma problems) depended on being able to look at the
components separately.

### D2. Freeze discipline: feature table and rubric locked before any contender runs

`output/rb_feature_table_v2.0.csv`, `data/fold_map.rds`, and `R/metrics.R`
were frozen before step 3. All bake-off contenders ran against identical
inputs, splits, and metrics.

**Why:** Model comparisons are meaningless if the playing field moves between
contenders. Any post-hoc change to data or metric would confound "better
model" with "different conditions."

### D3. The rubric: coverage first, sharpness as tiebreak, low-usage veto

Pre-committed before results existed:
- PRIMARY: pooled combined 80% coverage closest to nominal
- TIEBREAK: narrowest mean 80% width among models within +-2pp of nominal
- VETO: low-usage-bucket combined 80% delta beyond +-10pp disqualifies
  regardless of pooled performance

**Why coverage first:** a sharp interval that misses is worse than a wide one
that covers -- the product is honest probabilities. **Why the veto:** pooled
metrics are dominated by high-volume players; a model can look calibrated
overall while quietly abandoning committee backs. The veto forces honesty for
the 5-8 touch tier. This proved decisive twice: it caught 3C (v1) and
disqualified 3E, whose winning pooled sharpness was bought with -9pp
undercoverage on workhorses.

### D4. Tuning objective is RMSE on inner holdout -- never coverage or sharpness

**Why:** Tuning to the decision criterion is gameable. A model tuned to look
calibrated is not the same as a model that is calibrated. RMSE is a neutral
objective; calibration is measured afterward, out-of-sample, by the frozen
rubric.

### D5. Interval construction: per-fold power-law conformal (Mechanism A)

Combined intervals use conformal quantiles on residuals normalized by
`opportunities^alpha`, with alpha fitted per fold by OLS on
log(|resid|) ~ log(opp), clamped to [0.20, 0.90]. Median fitted alpha ~ 0.51.

**Why:** total-EPA variance scales sub-linearly with opportunities --
workhorses are more consistent per touch than committee backs. Naive pooled
conformal (alpha=0) overcovered low-usage players by +12pp; linear
normalization (alpha=1) undercovered them by -11pp. The fitted power law
(~sqrt(opp), consistent with theory) resolved both. This construction is
frozen and shared by every contender so that learner quality is the only
thing being compared.

### D6. Bake-off verdict: 3A-v2 (nested-CV-tuned LightGBM) is the projection engine

Five paradigms competed on 11,918 RB player-games (2014-2025, 204 folds):
default LightGBM (3A), tuned LightGBM (3A-v2), RF + conformal (3B),
hierarchical Bayes Student-t (3C), TabPFN (3D), direct quantile LightGBM (3E).

Final v2 table (pooled combined 80% delta / width / low-usage veto):

| Model | Pooled | Width | Low-usage | Verdict |
|---|---|---|---|---|
| 3A default | -0.3pp | 11.17 | -15.1pp | vetoed |
| **3A-v2 tuned** | **-0.3pp** | **9.24** | **-0.8pp** | **winner** |
| 3B RF | -0.1pp | 9.63 | +0.8pp | clean, wider |
| 3C HierBayes | +0.6pp | 10.32 | +13.4pp | vetoed |
| 3D TabPFN | crashed (reticulate segfault) | | | not blocking |
| 3E quantile | -1.8pp | 8.77 | +8.6pp | disqualified (stratified) |

**Why 3A-v2:** within 1pp of nominal across every touch stratum AND the
sharpest surviving model. **Why 3E lost despite winning pooled sharpness:**
its width was bought with -9.0pp undercoverage on 14+ touch players -- the
primary audience of a projection product. Stratified honesty outranks pooled
sharpness. **Why the 3C veto was not "fixed":** re-running 3C with a
different combined construction to pass would have confounded the paradigm
comparison. Its veto stands; its insight is used elsewhere (D7).

### D7. Two-product architecture: two rosters, not two numbers per player

The bake-off's deepest finding: every point-estimator + conformal approach
missed the committee-back efficiency tail by 9-12pp regardless of learner;
only 3C's Student-t with volume-conditional sigma calibrated it. No single
model was honest for both workhorses and dart throws.

Resolution -- a product decision, not a modeling patch:
- **Projection model (3A-v2):** high-touch roster. "What should I expect
  from the players I'm starting?"
- **Upside model (3C):** low-touch roster. "Which fringe guy has a tail
  worth streaming?" (RB only for now -- see D9.)

Players are routed to exactly one product by predicted role/volume.

**Why two rosters instead of running both models on everyone:** picking which
number to show per player recreates the switching-leaderboard seam the
architecture exists to avoid. A player moving between products when his role
changes reads as news, not as a modeling artifact.

### D8. WR clone (04a/04b): same construction, wider intervals accepted

The WR projection model is a direct clone of 3A-v2 -- identical tuning grid,
split protocol, and interval construction, WR-specific features. Passed the
rubric: pooled -0.5pp, all strata within +-3pp, width 10.10 (vs RB 9.24).

**Why the wider width was accepted:** weekly target volume is genuinely more
volatile than carry volume. Forcing RB-level sharpness would cost coverage.

### D9. WR 3C parked; no prior widening

The WR upside model failed twice (sigma ~ log_opp, then + air yards): WR
per-target EPA is broadly more dispersed than the Student-t allows at the
fitted sigma level. The failure is the sigma *level*, not the sigma *driver*.

**Why parked rather than patched:** widening the sigma prior until the veto
passes is tuning toward the decision criterion (violates D4's spirit). WR
per-target EPA has a different distributional character (deep-shot right
tail) than RB per-carry EPA. The two-product architecture may be RB-only;
that is an acceptable outcome. Revisit only with a fundamentally different
distributional spec.

### D10. The product is P(threshold), not a point projection

Published numbers: P(FP >= 15) for start/sit, P(FP >= 20) for booms. PPR.
Season-long players first, DFS later if warranted.

**Why:** a calibrated probability is one number per player per week that
moves as news breaks and resolves cleanly on Monday -- the same mechanics
that make win probability work as golf content. Every platform publishes
point projections; almost nobody publishes calibrated threshold
probabilities. The EPA interval machinery exists precisely to make these
probabilities honest. Corollary: all modeling decisions are judged by whether
they improve P(15+)/P(20+) calibration, not EPA metrics for their own sake.

### D11. Translation layer: regression bridge, empirical simulation, no parametric tails

`06_fp_translation.R` fits per-position OLS `fp_ppr ~ total_epa +
opportunities` (b_epa ~ 1.0 for both positions: one point of EPA is one PPR
point; the opportunities term carries the reception bonus). Its v1 normal
approximation failed its own calibration check exactly where a normal should
fail on right-skewed scoring: overconfident about must-starts at 15+,
understated booms at 20+ (residual skewness RB 0.90, WR 1.19).

`06b_fp_simulation.R` (v2) replaces the normal with simulation:
- EPA and volume drawn from each player-week's own conformal quantile points
  (piecewise-linear inverse CDF -- no distributional assumption)
- eff/vol error dependence via Gaussian copula, rho estimated from fold
  errors (RB 0.03, WR 0.12)
- translation noise resampled from actual OLS residuals, binned by volume
  tier (low-tier skew 1.4-1.8 vs ~0.7 for workhorses -- dart throws are
  boom-or-bust in translation too)
- P(20+) <= P(15+) coherent by construction (same draws)

**Why simulation over skew-t or a direct classifier:** it adds zero new
parametric assumptions -- every distortion in real scoring (TD lumpiness,
the floor near zero) is replayed from data. The EPA spine stays
scoring-format-agnostic: half-PPR or DFS scoring is a new cheap regression,
not a new model. A direct P(threshold) classifier was considered and
deliberately deferred: 3E already demonstrated where direct target-learning
cuts corners, and a classifier forfeits the interval infrastructure and
multi-threshold coherence. It remains the fallback experiment if the
translation route stalls (bake it off on Brier + stratified deciles).

**Data note:** the v1 run surfaced an NA-player_id cartesian join (dplyr
matches NA to NA) that inflated WR output 6x and polluted the v1 WR
regression fit. Fixed by filtering NA ids before all joins.

### D12. Current calibration status and the open WR gap

n-weighted mean |calibration delta| after v2 (2026-07-05):

| Position | 15+ | 20+ | Status |
|---|---|---|---|
| RB | 1.9pp | 1.3pp | publishable |
| WR | 2.3pp | 2.2pp | boom bins still understate +6-7pp |

The remaining WR miss is structural: conformal intervals are symmetric by
construction (pred +/- half-width), so the simulation cannot recover skew in
the EPA errors themselves -- and WR per-target EPA is the skewed quantity
(same character as the D9 finding, one layer downstream). The next lever is
**asymmetric conformal** in the WR interval construction: separate upper and
lower quantiles from signed normalized residuals instead of one quantile on
absolute residuals. Acceptance bar: WR 20+ content bins inside +-3pp without
breaking the rubric (pooled +-2pp, veto) or the decision zone.

## Repository map

```
R/
  build_rb_feature_layer.R      [1] RB feature table (frozen v2.0)
  build_bakeoff_rubric.R        [2] fold map + decision rule (frozen)
  metrics.R                     [2] shared metrics (frozen)
  03a_lgbm_control.R            [3] contender: LightGBM defaults
  03a_interval_construction.R   [3] Mechanism A power-law conformal (shared)
  03a_v2_lgbm_tuned.R           [3] WINNER: nested-CV-tuned LightGBM
  03b_rf_conformal.R            [3] contender: random forest
  03c_hierarchical_bayes.R      [3] contender: Student-t HierBayes (upside engine)
  03d_tabpfn.R                  [3] contender: TabPFN (crashed in v2)
  03e_quantile_lgbm.R           [3] contender: direct quantile (disqualified)
  04_bakeoff_results.R          [3] rubric adjudication
  04a_wr_feature_layer.R        [4] WR feature table (frozen v1.0)
  04b_wr_lgbm_tuned.R           [4] WR projection model (3A-v2 clone)
  05_wr_3c_hierarchical_bayes.R [5] WR upside attempt (parked, do not deploy)
  06_fp_translation.R           [6] EPA -> PPR regression + v1 normal approx
  06b_fp_simulation.R           [6] v2 simulation probabilities (current)
  read_stathead.R               data utility

data/    RDS intermediates (feature tables, outcomes, fold map)
output/  frozen CSV artifacts: feature tables, fold predictions, coverage
         tables, calibration tables, deployment params (06b_sim_params,
         06b_resid_pools)
logs/    run logs for reproducibility
```

## Reproducibility

- Data: nflreadr play-by-play and player stats, seasons 2014-2025, REG only
- Evaluation: 204 walk-forward folds (train on past, test on next week);
  fold map frozen in `data/fold_map.rds`
- Seeds: 42 everywhere randomness exists (simulation, tuning)
- Every model stage writes its coverage/calibration tables to `output/` and
  its console log to `logs/`

## Status (2026-07-05)

- RB projection + translation: **calibrated, content-ready**
- WR projection: calibrated at the EPA level; boom translation understates
  (open item -- asymmetric conformal experiment next)
- RB upside product (3C): scoped, not yet built out
- Content launch order: RB first is viable; WR boom numbers are
  directionally conservative (real rates run hotter than stated)
