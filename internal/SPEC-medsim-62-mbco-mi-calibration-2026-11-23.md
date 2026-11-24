# medsim #62 MBCO-MI calibration harness

This specification records the approved 0.5-percentage-point relative margin,
6.5% absolute size ceiling, and skewed-error linear model for the minor-release
gate in task 1b of [medsim #62](https://github.com/Data-Wise/medsim/issues/62).
The default change already merged in [PR #63](https://github.com/Data-Wise/medsim/pull/63).
The harness is implemented under `inst/mbco-calibration/`. Pilot, production
run, and release require separate authorization. Task 2 delegation and task 3
cross-package architecture are outside this specification.

## Design and fixed budget

Use the existing medsim linear mediation generator and change the error shape
for the non-Gaussian condition. Reuse the Missing Effect design choices of
normally distributed exposure, direct path 0.14, unit residual variances,
Fleishman skewness 1.5 and kurtosis 4 for the skewed condition, and 20 requested
`mice` normal-model imputations. Use mediator-only MAR driven by standardized
observed exposure and outcome. Cross target MAR rates of 10%, 30%, and 50% with
sample sizes 100, 200, and 500 and with both error shapes. Record realized
missingness. The path values below carry over the archive's interior nulls and
select its symmetric moderate-effect alternative.

| Type | Paths `(a, b)` | Cells | Attempts per cell | Total datasets |
|---|---|---:|---:|---:|
| Null | `(0, 0)`, `(.39, 0)`, `(0, .39)` | 54 | 100,000 | 5,400,000 |
| Power | `(.39, .39)` | 18 | 10,000 | 180,000 |
| Total | | 72 | | 5,580,000 |

Each `(cell, replicate)` generates one dataset. When it has missing values,
create one completed imputation list and compute the per-imputation and stacked
likelihood triples once. From those shared fits, emit own/Li, own/Chan–Meng,
fixed/Li, and fixed/Chan–Meng.
Preserve the two union-null branch fits. For each ARIV rule, the two df choices
share D4 and the corresponding `r4`. All four share the point estimate, both
ARIV diagnostics, and branch information. Keep the medsim six-field method
contract intact and add harness diagnostics in the run outputs.

Use six combine groups, one for each sample size and attempt budget. Each
sample size has a null group with 18 cells and a power group with six cells.
Give each group a separate chunk directory and combine with its own expected
chunk count and `nsim` (100,000 or 10,000). Preserve the stock combine integrity
checks. For the four arm p-values and their two result aliases, count distinct
values at 12 significant digits. The collapse threshold and decimal-place
rule for other columns stay the same. Use the saved values for all rejection
decisions. Reconcile the six group receipts against one frozen 72-cell
manifest that lists each cell, its group, and its exact replicate ID range. Before
submission, check the actual medsim seed mapping for collisions over every
planned `(scenario name, global replicate ID)`, including across groups.
Resolve any collision before freezing the confirmatory manifest.

## Release decision and paired Monte Carlo uncertainty

Use `p < .05` as the rejection rule. Let `new` be fixed/Chan–Meng and `legacy`
be own/Li. The primary paired estimand in each null cell is
`Pr(new rejects, legacy accepts) - Pr(legacy rejects, new accepts)`. Report
both discordant counts, the mean of the paired rejection differences, and
the sample SD of those differences divided by `sqrt(R)` for `R` pairs.

Base the release decision on all attempted replicates. Treat an unusable new
result as a rejection and an unusable legacy result as a nonrejection in the
decision calculation. Use the following dispositions for each attempted draw.

| Data and method path | Gate treatment |
|---|---|
| Missing values and 20 usable imputations | Use each arm's observed p-value if finite and within `[0, 1]`. |
| No realized missing values | Record a complete-by-design draw with effective `K = 1`. Use the adapter's complete-data MBCO chi-square result in all four arms if valid. It contributes to absolute size and contributes zero to the paired difference. Record it separately from the MI branch-mix denominator. |
| Missing values and failed imputation, fallback, or effective `K` other than 20 | Record the reason and code all arms as unusable. Retain the draw in the attempt budget. |
| Valid data and imputation path but a missing or invalid arm p-value | Code that arm as unusable. Keep valid results from the other arms. |

Report the complete-pair estimate and MCSE separately. Record attempted,
usable, and unusable counts and reasons for each arm, effective imputation
counts, complete-by-design draws, fallback, `branch_mix` numerator and
denominator, and size or power with MCSE. Report branch mixing among usable MI
draws with an observed flag. Power is descriptive and has no minimum release
threshold.

For each of the 54 null cells, compute one-sided exact Clopper–Pearson bounds
from the attempted denominator. Use tail probability `0.05 / (3 * 54)` for
each of these three component bounds. Bonferroni provides simultaneous coverage
of at least 95% across the full null grid without assuming independence between
the component counts or across cells. The binomial bounds assume independent
replications within each cell.

1. Upper-bound the new-only rejection probability, `U_new_only`.
2. Lower-bound the legacy-only rejection probability, `L_legacy_only`.
3. Upper-bound the new arm's absolute rejection probability, `U_new`.

The numeric gate passes only if every null cell has
`U_new_only - L_legacy_only < 0.005` and `U_new < 0.065`. Use the exact endpoint
rules for zero and all-success counts. An estimate below a threshold with a
bound at or above it leaves the gate open.

### Budget basis and stopping rule

The Missing Effect archive contains 36 null cells at 25% MAR, with 2,000
pairs each across mediator-only and mediator-plus-outcome patterns. The largest
fixed/Chan–Meng rate was 5.7% and the largest new-only rate was 0.2%. In six
cells the new arm rejected more often than legacy. The largest increase was
5.10% versus 4.90%, with four new-only and zero legacy-only rejections. That
pairing gives an MCSE of about 0.10 percentage points. The archived input is
job `4331219` from medsim 0.5.1, with its raw CSV identified by SHA-256
`356f99b634359b5f9a61a5d7a6a19132ff9245703273609ad58ef1e999d2297d` in Missing
Effect's `docs/decisions/RERUN-MANIFEST-d4-df-2026-10.md`. Its four-arm summary
is `simulation_results/rereference-d4-df-cells.csv`. Historical effective
imputation counts and fallback status remain unverified.

The planning envelope allows absolute new rejection up to 6.0% and new-only
rejection up to 0.3% in every cell, including conservative coding of unusable
results. At 100,000 pairs, at most 6,234 new rejections and 424 new-only
rejections suffice for both bounds even with zero credit for legacy-only
rejections. Exact binomial tail probabilities and a union bound give a 94.87%
lower bound on all-cell assurance under the envelope. This assumption is
unverified at the requested MAR rates. The absolute ceiling drives the budget.
At 10,000 attempts, each individual arm's binomial power estimate has an MCSE
of at most 0.5 percentage points over all attempts, counting unusable results as
nonrejections for that power summary. The archived mediator-only runs averaged
0.31–0.35 worker-seconds per dataset by sample size, implying about 507 worker-hours
at historical throughput. Use the authorized pilot to estimate the current
worker time, peak memory, storage, and combine cost before production approval.

Freeze scenario IDs, replicate IDs, seeds, and code and dependency identities
before the confirmatory run. Stop each cell at its fixed attempt count and
evaluate rejection results once after reconciling all planned IDs. A scheduler
interruption, including preemption or a time limit, may be retried for the
same assigned IDs with the same seeds and pinned inputs. Keep each attempt's
receipt and output outside the canonical combine directory until reconciliation.
For overlapping IDs, require identical deterministic fields, including arm
outputs, diagnostics, and recorded statistical failure status. Exclude timing
and scheduler metadata from that comparison. Promote one complete, audited
chunk per planned chunk ID. An overlap disagreement or unresolved missing or
duplicate key holds the gate.

Recorded imputation and fit failures are statistical outcomes and stay in the
denominator. A scheduler retry cannot replace one with a differing result.
An unexpected DGP error is a combine integrity violation and holds the gate
for investigation. Leave the gate open for incomplete provenance, a benchmark
discrepancy, or any bound that fails to clear its threshold. Review the failed
cell and reopen the default decision if needed. Any follow-up requires a
separately approved design and run. There is no result-dependent top-up, seed
replacement, or early pass.

## missingmed benchmark crosswalk

| Benchmark | Comparison and role |
|---|---|
| Plain Gaussian, 25% MAR, `n = 200, 500`, `m = 20` | Closest historical comparison. Use mediator-only MAR and align the origin and both interior nulls by which path is zero. Compare fixed/Chan–Meng with fixed/Chan–Meng and own/Chan–Meng with own/Chan–Meng. For `n = 200, b = 0`, medsim reports 4.95% fixed and 5.60% own, versus 4.70% and 6.50% in missingmed. The nonzero paths, MAR predictors, and intercept calibration differ. Use these rates as context. |
| Plain Gaussian, 40% MAR | Stress benchmark for the same two Chan–Meng arms. At `n = 200`, fixed size was 3.5% and 4.1% in the interior nulls, versus 8.3% and 7.5% for own. The archived medsim rate is 25%, and this gate uses 10%, 30%, and 50%. |
| GLM families and Gaussian interaction | Chan–Meng results provide context for binary and count outcomes and unequal branch constraint counts. The Gaussian interaction benchmark excluded many own-arm p-values. The gate's skewed condition retains the linear model and equal branch constraint counts. |

Benchmark sources are the pinned missingmed [calibration spec](https://github.com/Data-Wise/missingmed/blob/4272b367b483018fccb1a0aadc7f618e94ee6901/docs/specs/SPEC-glm-mbco-calibration-2026-10-09.md)
and [plain-Gaussian results](https://github.com/Data-Wise/missingmed/blob/4272b367b483018fccb1a0aadc7f618e94ee6901/docs/specs/RESULTS-glm-own-gauss-2026-10-09.txt).

Shared-input parity clears the missingmed benchmark for task 1b. Use the
medsim release candidate revision pinned in the confirmatory manifest and
missingmed revision `4272b367b483018fccb1a0aadc7f618e94ee6901`. Use all three
plain-Gaussian cases from missingmed's
[`mbco-d4-medsim-0.5.1.rds`](https://github.com/Data-Wise/missingmed/blob/4272b367b483018fccb1a0aadc7f618e94ee6901/tests/testthat/fixtures/mbco-d4-medsim-0.5.1.rds)
fixture, Git blob `e9956e7ea9127f4b7138b3073c7880b88508e583`. Use its completed
imputations and formulas as inputs. Compute fresh Chan–Meng outputs in both
implementations on those inputs.

For each fixture and both ARIV settings, compare D4, `r4`, denominator df,
and p-values element by element. Finite values `x` and `y` agree when
`abs(x - y) <= 1e-10 * max(1, abs(x), abs(y))`. Infinite df must be positive
infinity on both sides. Require identical stacked branch, branch-mix flag,
constraint dimension, and `p < .05` decision. Report every discrepancy and
hold the gate for any mismatch. This is a deterministic implementation check
on shared inputs. Independently estimated historical sizes remain contextual.

## Implementation handoff and verification

Use the package's `inst/` harness conventions and the existing
`medsim_run_chunk()` execution and combine path. Pin the package revision,
dependency versions, RNG convention, commands, input and output hashes, and
chunk manifests in the run receipt. The [harness README](../inst/mbco-calibration/README.md)
specifies the local checks, run commands, and receipts. Exact replicate-key
coverage and successful reconciliation are prerequisites to interpreting the
gate.

- Run a two-replicate, one-cell smoke test for output shape and column names.
- Check a fixed-seed fixture for shared point estimate and both ARIV diagnostics
  across all four arms. Check that changing df preserves D4 within each ARIV
  rule. Use a fixture with branch mixing and verify that forcing ARIV to be
  ignored makes the assertion fail.
- On fixed input data, supply the same completed imputations to the harness and
  the public `medsim_method_mbco_mi()` adapter. Exercise the adapter with explicit
  ARIV and df settings for all four arms. Compare each harness p-value with the
  adapter's selected `indirect_p` and verify the fixed-arm diagnostic,
  point estimate, and branch diagnostics. The fixture must distinguish both
  switches. Exercise the unmodified adapter on complete data and verify all
  four harness arms use its complete-data p-value.
- Verify six stock combine groups, global seed uniqueness, and cross-group key
  reconciliation. Check retry agreement and failure dispositions for duplicate
  or missing IDs, imputation fallback, invalid arm output, and DGP failure.
  Cover zero discordances and bounds exactly at either threshold.
- After authorization, use a bounded Hopper pilot to verify the SLURM path,
  runtime and memory, preserved pairing, and receipt completeness. Pilot results
  are outside the confirmatory budget. Size the production chunks from the
  pilot and obtain separate approval before scaling.
