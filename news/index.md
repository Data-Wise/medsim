# Changelog

## medsim 0.6.0

### New features

- **[`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)
  gains `n` and `n_truth`.** `n` sets the sample size of each simulated
  data set
  ([`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  passes it to every `data_generator(n = )`). `n_truth` sets the size of
  the one large draw used to compute ground truth. Both default to
  `NULL`, which keeps the runner’s defaults (200 and 10000) and leaves
  existing configs unchanged. They follow `...`, so they match only by
  full name; `medsim_config(n = 100)` used to fail because `n` partially
  matched `n_replications`, `n_cores` and `n_chunks`. The sample size is
  now shown by [`print()`](https://rdrr.io/r/base/print.html).

- **[`medsim_analyze_performance()`](https://data-wise.github.io/medsim/reference/medsim_analyze_performance.md)
  accepts a `medsim_results` object** and merges its `$truth` table
  itself, so `medsim_analyze_performance(results)` works like the other
  `medsim_analyze_*()` functions. A data.frame that already has
  `<param>_truth` columns still works as before. A missing truth now
  gives an error that names the parameter and points to `compute_truth`.
  When only some scenarios lack truth, their bias and RMSE are `NA` and
  a warning names those scenarios.

- **[`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
  warns once, and only when it matters.** An SE, interval or p-value
  column of a scored parameter (`<p>_se`, `<p>_ci_lower`,
  `<p>_ci_upper`, `<p>_p`) is skipped silently. Any other output column
  without a truth is listed in a single warning, instead of one warning
  per column.

- **[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  gains `df = c("li", "chan_meng")`.** It sets the denominator degrees
  of freedom of the D4 `F` reference for both `indirect_p` and
  `indirect_p_fixed`. `"li"` (the default) keeps the Li et al. (1991) /
  Meng-Rubin approximation, so existing results are unchanged.
  `"chan_meng"` uses Chan and Meng (2022, eq. 2.15),
  `nu = k(K-1)(1 + 1/r4)^2`, the reference of
  `mitml::testModels(method = "D4")`; with it, the p-value matches
  `mitml` (`ariv = "positive"`) to 1e-6, where the legacy df matched
  only to about 10%. The option re-refers the p-values only: the
  statistic, `r4`, `r4_fixed` and the random-number stream are
  identical, so a run can switch df without changing its replicates.
  Both choices give a chi-square reference when `r4 = 0`. Eq. 2.15 is
  derived for the own-branch `r4` at a regular null; for
  `ariv = "fixed"`, and at the nonregular null `a = b = 0`, either df is
  a working reference whose calibration rests on simulation.

- **[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  gains `compute_truth`**
  ([\#41](https://github.com/Data-Wise/medsim/issues/41)). Chunks
  previously carried no `$truth`, so a combined cluster run supported
  power analysis but not coverage or bias. Only chunk 1 computes truth,
  so it is computed once rather than once per chunk.
  `config$compute_truth_every_chunk <- TRUE` opts every chunk in. Chunk
  provenance records `truth_requested`.
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  now unions truth across all chunks: it used to discard every later
  chunk’s truth whenever chunk 1 had none.

- **Ground-truth integrity
  ([\#41](https://github.com/Data-Wise/medsim/issues/41)).** A failed
  `compute_truth()` used to be written to the truth cache, and every
  later run with the same fingerprint loaded the failure. A failure is
  now never cached, and failure entries that older versions cached are
  treated as misses and recomputed. A truth function that succeeds but
  returns an unusable shape (a bare named vector, or a list element of
  length \> 1) is reported, where before it contributed nothing
  silently. **Breaking:** `medsim_run(truth_on_failure = )` and
  `medsim_run_chunk(truth_on_failure = )` default to `"stop"`, which
  raises a `medsim_truth_failure_error` before any replication runs;
  this used to warn and continue with an empty truth. Pass
  `truth_on_failure = "warn"` to continue without that scenario’s truth.
  Scenarios whose truth functions return different parameters no longer
  crash the truth table.

- **`truth_coverage` violation
  ([\#41](https://github.com/Data-Wise/medsim/issues/41)).**
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  and
  [`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md)
  raise it when truth rows carry no parameters, when a scenario has
  results but no truth, or when chunks were asked for truth and none
  carries it. It goes through `on_violation`. Runs that never requested
  truth are unaffected.

- **[`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
  and
  [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
  stop on a truth table with no parameter columns**
  ([\#41](https://github.com/Data-Wise/medsim/issues/41)), where they
  used to return an empty analysis as if it had succeeded.

- **[`medsim_cache_save()`](https://data-wise.github.io/medsim/reference/medsim_cache_save.md)
  writes atomically**
  ([\#41](https://github.com/Data-Wise/medsim/issues/41)): it writes a
  temporary file unique to the writer, next to the target, then renames
  it into place. A failed rename stops with an error.

### Documentation

- **Three new articles.** “Tutorial: Your First Coverage Study” builds a
  complete study step by step, from the data-generating model to
  coverage and type I error with Monte Carlo SEs. “Analyzing and
  Reporting Results” maps each question to its analysis function, and
  explains method comparison under common random numbers, tables,
  figures and
  [`medsim_workflow()`](https://data-wise.github.io/medsim/reference/medsim_workflow.md).
  “Cookbook: Recipes for Common Tasks” has short recipes: scenario
  grids, sample size, nonnormal and missing data, failing methods,
  caching and clusters.
- **The sample size is documented:**
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  calls each data generator with the configuration’s `n` (200 unless set
  with `medsim_config(n = )`), so a generator’s own default `n` is used
  only when you call it by hand.
- **The missing-data vignette runs again.** Five of its six runnable
  chunks failed against the current API
  (`medsim_amputate(vars =, rate =)`, a
  [`medsim_scenario_missing()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing.md)
  call without `true_params`, a grid with `rates`/`skew`, and
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  given a list of method factories). They now use `target`/`prop`,
  `true_params`, `props`/`nonnormal_list`, and one method per
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  call. The vignette also gains a section on `ariv` and `df`, and no
  longer describes `MC-CI` as a delta-method interval or `branch_switch`
  as a switching rate.
- **`branch_switch` is documented as what it is:** the majority
  union-null branch (1 = `a = 0`), so
  [`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/reference/medsim_summarize_branch_switch.md)
  reports the share of replicates whose majority branch is `a = 0`;
  imputation disagreement is `branch_mix`.
- **The README and
  [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  no longer claim an exact `mitml` match:** the D4 statistic matches,
  and the p-value matches only with `df = "chan_meng"`.
- **Runnable `@examples`** for
  [`medsim_rnonnormal()`](https://data-wise.github.io/medsim/reference/medsim_rnonnormal.md),
  [`medsim_amputate()`](https://data-wise.github.io/medsim/reference/medsim_amputate.md),
  [`medsim_scenario_missing()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing.md),
  [`medsim_scenario_missing_grid()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing_grid.md),
  [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md),
  [`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/reference/medsim_method_mc_ci.md),
  [`medsim_method_ipw()`](https://data-wise.github.io/medsim/reference/medsim_method_ipw.md)
  and
  [`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/reference/medsim_summarize_branch_switch.md)
  (the imputation-based ones in `\donttest{}`).

### Bug fixes

- **Overlapping attempts at one chunk no longer share a temporary
  file.**
  [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  wrote every attempt to `.chunk_NNNN.rds.tmp` before renaming it into
  place. A SLURM retry that overlapped a still-running attempt could
  therefore interleave writes, or rename the other attempt’s
  half-written file into place. Each attempt now writes its own
  temporary file (process ID plus a random suffix) in `output_dir` and
  removes it on exit. When attempts compete, a complete result always
  replaces the chunk file, and a partial result never replaces a
  complete one: it warns, keeps the complete file, and still exits with
  an error.

- **[`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md)
  now reports partial chunks.** It audited only the result rows, so a
  combined object recovered with `on_violation = "warn"` passed a later
  standalone audit even when its `chunk_provenance` flagged a chunk as
  partial: the surviving replication ids can still be contiguous. The
  audit now raises the same `partial_chunk` violation, with the same
  message, as
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md).
  Chunk provenance also records `n_rows`, so the audit can say whether a
  flagged chunk is empty or truncated. Chunks written before this
  version get a generic description. A bare data.frame carries no
  provenance and skips the check.

- **All-reps-failed chunks are no longer silently dropped
  ([\#50](https://github.com/Data-Wise/medsim/issues/50)).**
  [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  short-circuited before writing a file when no replication survived, so
  the chunk carried no `partial = TRUE` provenance marker – and a
  trailing all-failed chunk therefore combined with **zero** integrity
  violations. The reserved-`error` contract violation fails *every*
  replication of any chunk it touches, so `partial_chunk` could never
  fire for the very defect class it was built to catch. Such chunks now
  write a flagged, zero-row file (already a supported shape downstream)
  before re-[`stop()`](https://rdrr.io/r/base/stop.html)ing, so
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  reports `partial_chunk` as designed. No API change, but note the
  behavior change this intends: a run that previously combined silently
  past an all-failed chunk now stops under the default
  `on_violation = "stop"`.

- **An all-chunks-failed combine now reports the real cause
  ([\#50](https://github.com/Data-Wise/medsim/issues/50)).** With every
  array task failing,
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  reached the seed audit with zero rows and warned that the chunks
  *“lack the schema-v2 stamp (produced by an older medsim)”* – advice to
  chase a version mismatch that does not exist, on a run whose actual
  problem is that every replication failed. The audit now returns early
  on zero rows, `partial_chunk` says “they hold no results at all”
  rather than “only their completed reps”, and `config$n_replications`
  is `0` instead of `-Inf`.

- **The “real parallel” tests no longer run the serial path under
  `R CMD check --as-cran`
  ([\#50](https://github.com/Data-Wise/medsim/issues/50)).**
  [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)
  falls back to sequential execution when `_R_CHECK_LIMIT_CORES_` is
  set, and the three cases meant to exercise the parallel failure path
  never cleared it – so the release gate’s `--as-cran` run covered no
  parallel code in that file while every assertion still passed. The
  fallback is correctness-preserving, which is why nothing noticed.
  Those cases now clear the variable, keep the fixture clear of the
  four-task fallback floor, and a new positive-control test asserts
  replications actually execute on worker processes.

- **Numeric-kind contract collision fixed.** v0.5.0 declared `error`
  both as the mandatory result column of the `"numeric"` estimand kind
  ([`medsim_scenario_numeric()`](https://data-wise.github.io/medsim/reference/medsim_scenario_numeric.md),
  [`medsim_estimand()`](https://data-wise.github.io/medsim/reference/medsim_estimand.md)
  docs) and as a reserved failure-schema field that
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  rejects when a successful `method()` returns it – the documented
  numeric contract was unusable as written. The mandatory column is now
  `est_error` (breaking only for code written against the never-runnable
  v0.5.0 contract).

- **[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  schema-vote fixed to not be vetoed by an empty chunk.** A single
  legitimately empty chunk (e.g. `n_chunks > n_reps`) counted as “not
  schema-v2” in the whole-combine vote, stripping the schema-v2 stamp
  and silently skipping the replication-id contiguity and cross-scenario
  seed-collision audits for every chunk, not just the empty one. Empty
  chunks are now exempted from the vote.

- **New `partial_chunk` integrity violation.** A chunk written by a task
  that failed mid-run can still present a clean, contiguous id block
  (e.g. 1-15 of an intended 20) that the contiguity audit alone cannot
  distinguish from a genuinely complete chunk.
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)/[`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md)
  now flag any chunk whose provenance is marked `partial = TRUE`.

- **New `dgp_failure` Gate A violation.** A `data_generator()` failure
  row (introduced alongside this fix) still carries a valid, contiguous
  global replication id, so the contiguity audit alone can no longer
  detect it as missing data.
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)/[`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md)
  now report the count of `data_generator()`-failure rows directly.

- **`medsim_run(parallel = TRUE)` no longer silently drops replications
  on a task-level error.** An error escaping a replication task – a
  contract violation (the reserved-`error`-field stop) or an
  infrastructure error, as opposed to a `method()`/`data_generator()`
  failure (both of which already became failure rows) – was caught
  internally and then silently dropped by
  [`.medsim_rbind_reps()`](https://data-wise.github.io/medsim/reference/dot-medsim_rbind_reps.md)’s
  row filter: the run “succeeded” with fewer rows and no diagnostic.
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  now stops with a `medsim_run_failure` condition (subclass of
  `medsim_error`) carrying whatever completed before the failure –
  recover it with
  `tryCatch(medsim_run(...), medsim_run_failure = function(e) e$results)`.
  Serial (`parallel = FALSE`) execution gets the same per-task handling,
  so both modes now behave identically on a task-level error.

- **`data_generator()` failures are now failure rows, not silent aborts
  (or crashes) that stop the whole run.** A failing `data_generator()`
  produces a failure row (`error = "data_generator error: <msg>"`,
  distinct from a `method()` failure’s plain message) and the run
  continues; the estimator correctly never runs for that replication,
  and `converged` stays `NA` rather than being forced to `0` (which
  would have poisoned `converged_mean`).
  [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)’s
  output gains `n_dgp_failed`/`dgp_failure_rate` alongside the existing
  `n_failed`/`failure_rate` (now conditional on `data_generator()`
  having succeeded) – the two rates use different denominators and do
  not sum, by design.

- **[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  rescues partial results on a task-level failure.** A chunk interrupted
  by a `medsim_run_failure` now writes its completed replications
  atomically (with provenance `partial = TRUE`) before
  re-[`stop()`](https://rdrr.io/r/base/stop.html)ing, instead of losing
  the whole chunk. The array task still exits nonzero for `sacct`
  visibility – this is forensics, not a `--requeue` signal.

- **`medsim_check_results(list())` no longer errors** on the supported
  `n_replications == 0` case (empty task list).

- **[`medsim_summarize_results()`](https://data-wise.github.io/medsim/reference/medsim_summarize_results.md)
  reports `NA`, not `Inf`/`-Inf`, for an all-`NA` cell** (e.g. every
  replication in a scenario a `data_generator()` failure) –
  [`min()`](https://rdrr.io/r/base/Extremes.html)/[`max()`](https://rdrr.io/r/base/Extremes.html)
  no longer emit a “no non-missing arguments” warning for a cell with
  genuinely nothing to summarize.

## medsim 0.5.1

### New features

- [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  gains `ariv = c("own", "fixed")` and always emits additive branch
  diagnostics alongside the method contract: `indirect_p_fixed` (D4
  p-value with the ARIV `r4` recomputed on the branch the *stacked*
  constrained fit selected), `branch_mix` (imputations disagree on the
  constrained branch), `p_branch_a`, `stacked_branch`, `r4`, `r4_fixed`.
  `.medsim_d4_mbco()` gains `fixed_branch =` and returns
  `stacked_branch`. Motivation: the standard Chan–Meng `r4` averages
  per-imputation MBCO statistics each computed on its own winning
  branch, so branch disagreement pulls the average down and
  under-estimates the ARIV (Missing Effect comparator pilot,
  2026-08-16). Default `ariv = "own"` keeps legacy behavior;
  `indirect_p` is unchanged unless `ariv = "fixed"`. The stacked D4
  model is fit once per replication and shared by both ARIV computations
  ([\#44](https://github.com/Data-Wise/medsim/issues/44)).

### Bug fixes

- Gate A.2 (seed-collapse audit in
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  /
  [`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md))
  no longer flags the
  [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  branch diagnostics: `branch_mix`, `stacked_branch`, `p_branch_a`,
  `r4`, `r4_fixed` join `converged`/`branch_switch` in the default
  `collapse_exclude` (they are legitimately low-cardinality — 0/1 flags,
  an `m`-valued share, ARIVs clamped at 0). Without this, a chunked
  MBCO-MI run raised `[collapse]` violations and the default
  `on_violation = "stop"` refused to combine it (pre-release adversarial
  review, [\#44](https://github.com/Data-Wise/medsim/issues/44)
  follow-up).

## medsim 0.5.0

### New features

- **Pre-integration review hardening** (7 confirmed findings). The
  collapse audit excludes discrete contract fields BY NAME
  (`collapse_exclude`, default `converged`/`branch_switch`) instead of
  by observed distinctness – a TOTALLY collapsed estimate column (1-2
  distinct values) is no longer invisible to the very check built for
  it. An all-failed run now fires an `all_failed` violation instead of
  combining clean. Mixed legacy+v2 chunk directories are
  order-independent (attributes stripped + one warning; the audit never
  mis-stops on legacy ids). `error` is a documented RESERVED
  method-field name – a method returning it stops immediately instead of
  having `converged` silently forced to 0. Pilot checks: `n` compared
  numerically (200L vs 200 no longer false-positives) and NA-vs-value
  asymmetries against the pilot are mismatches. All-provenance-less
  chunk sets warn about the skipped SHA assertion. The combine `rbind`
  is schema-harmonized (ragged chunk columns cannot crash it), and the
  submit template creates `logs/` up front.

- **Gate C: chunk provenance + single-SHA assertion**
  ([\#34](https://github.com/Data-Wise/medsim/issues/34)). Every chunk
  file now carries a provenance header (R version, medsim + key
  dependency versions, hostname, code SHA, sec/rep, UTC timestamp). The
  SHA **auto-detects** via `git rev-parse HEAD` in the running script’s
  directory (explicit `code_sha =` overrides; `pkg:medsim-<version>` tag
  outside git).
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  asserts one SHA across all chunks – catching a mid-run code edit +
  partial resubmit that would silently mix two code states.
  Provenance-less legacy chunks warn, never stop.

- **Gate D: pilot-subset positive control**
  ([\#34](https://github.com/Data-Wise/medsim/issues/34)). Because seeds
  depend only on `(scenario, replication)`, a full run’s reps
  `1..B_pilot` are draw-identical to an archived pilot at the same `n`.
  `medsim_combine_chunks(pilot_reference =, pilot_tol = 1e-9)` asserts
  **identity first** (sample size + scenario fingerprints – a stale
  pilot fails as `pilot_config_differs`, never masquerading as a seeding
  regression), then compares estimate columns only (never `elapsed`)
  within tolerance – a free regression check that harness, environment,
  and seeding are unchanged since the pilot passed.

- **Gate B: hardened SLURM submit template**
  ([\#34](https://github.com/Data-Wise/medsim/issues/34), fixes
  [\#37](https://github.com/Data-Wise/medsim/issues/37)).
  [`medsim_write_submit_script()`](https://data-wise.github.io/medsim/reference/medsim_write_submit_script.md)
  now emits a fail-loud script: `#!/bin/bash -l` (on Hopper `module` is
  defined only in login shells – the old plain `#!/bin/bash` template
  failed there with “module: command not found”), `set -eo pipefail`, a
  hard-failing `module load` (never `|| true`), a `command -v Rscript`
  pre-check, `#SBATCH --requeue`, and `Rscript` as the final command so
  its exit code is the task’s exit code. Optional
  `config$array_throttle = K` emits `--array=1-N%K`. There is
  deliberately no shell-side output-file gate – completeness is audited
  at combine time by Gate A. The `inst/hopper-tests/submit_chunk.sh`
  exemplar is updated to the same pattern, and the `chunk_%04d.rds`
  naming convention is centralized in an internal helper shared by
  writer and combiner.

- **Gate A: combine-step seed-provenance audit**
  ([\#34](https://github.com/Data-Wise/medsim/issues/34),
  SPEC-medsim-chunked-run-gates).
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  now audits the combined grid and the new
  [`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md)
  runs the same audit standalone: per-scenario `replication`
  **contiguity** (gapless, duplicate-free `1..max`, equal across
  scenarios – self-validating, no external count needed; optional
  `nsim =` pin), the **collapse signature** on every continuous estimate
  column (`n_distinct > collapse_threshold * n_ok`; the 0.3.1 seed
  collapse produced ~17 distinct outcomes in 1000 while every chunk
  exited 0), all-failed cells reported as `cell_failed`, and
  **cross-scenario seed collisions** in
  [`.medsim_det_seed()`](https://data-wise.github.io/medsim/reference/dot-medsim_det_seed.md)’s
  hash space. Violations route through one
  `on_violation = c("stop", "warn", "ignore")` control; the default
  `"stop"` signals a `medsim_combine_violation` condition that **carries
  the combined results**
  (`tryCatch(..., medsim_combine_violation = function(e) e$results)`
  recovers an hours-long run’s good cells). **Breaking**: the old
  `expected_chunks` warn-and-combine default is now a violation under
  the same control – for a deliberate partial combine (interim look at a
  running array), pass `on_violation = "warn"`. Legacy (pre-schema-v2)
  frames skip the id-based audits with a warning, never an error.

### Bug fixes

- **Chunked runs: `replication` is now the GLOBAL rep id** (schema v2;
  [\#36](https://github.com/Data-Wise/medsim/issues/36),
  SPEC-medsim-chunked-run-gates P1). Previously each SLURM chunk emitted
  chunk-local ids (`1..chunk_size`), so a combined 4-chunk/nsim-20 run
  carried 5 distinct `replication` values each appearing 4 times, rows
  were not uniquely identifiable, and
  [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
  reported the chunk size as `n_replications`. Standalone (non-chunked)
  runs are unchanged. Result frames now carry `medsim_schema = 2L` and a
  `medsim_meta_cols` provenance attribute recording which columns the
  runner wrote (consumed by
  [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md);
  legacy frames fall back to the old name list).
- **[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  rebuilds `$summary` and `$config`** (P2). It previously returned chunk
  1’s slice statistics and chunk-sized `config$n_replications` as if
  they described the combined run.
- **One failed replication no longer crashes the run** (P3). Failure
  rows and success rows had different columns, so `rbind` errored
  (“names do not match previous names”) – in chunk mode converting one
  transient rep failure into a missing chunk file. All rows now share
  one schema: an `error` column on every row (`NA` on success), `NA`
  estimates and `converged = 0` on failure rows.
- **Logical method-contract fields are no longer dropped** (P3).
  `branch_switch`/`converged` returned as logicals (including logical
  `NA`) were silently excluded from `$results`, breaking
  [`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/reference/medsim_summarize_branch_switch.md).
- **Chunk mode no longer writes intermediate CSVs**
  ([\#38](https://github.com/Data-Wise/medsim/issues/38), P3).
  Concurrent array tasks sharing an `output_dir` overwrote each other’s
  fixed-name `results_scenario_*.csv`/`all_results.csv` (last writer
  wins), leaving partial data that looked complete. The chunk `.rds` is
  the artifact; standalone runs still write their CSVs.

### Quality fixes (2026-07 review round)

- [`medsim_scenario_pmed()`](https://data-wise.github.io/medsim/reference/medsim_scenario_pmed.md)
  now computes the ground-truth P_med with the exact closed form
  \`Phi((beta_ay + alpha_ax*beta_my) / sqrt(2*(beta_my^(2\*sigma_m)2
  - sigma_y^2)))`instead of an unseeded 4x50k Monte-Carlo potential-outcome draw at construction time. The truth is now deterministic, exact, and leaves the RNG state untouched; the`n_po`argument (and the`n_po`parameter of the internal`.medsim_pmed_truth()\`)
    has been removed.
- [`medsim_validate_scenario()`](https://data-wise.github.io/medsim/reference/medsim_validate_scenario.md)
  now validates generator output for *all* estimand kinds:
  interval/numeric (and other non-core) kinds previously got zero column
  validation; they now require a non-empty data.frame (at least 1 row
  and 1 column). X/M/Y columns are still not required for these kinds –
  their column contract remains method-defined.
- `.medsim_analyze_coverage_interval()` now emits a `coverage_mcse`
  column (`sqrt(p*(1-p)/n_valid)`, mirroring the point path) in both the
  overall and by-scenario coverage frames, so
  [`medsim_table_coverage()`](https://data-wise.github.io/medsim/reference/medsim_table_coverage.md)
  renders a real MCSE for interval-kind runs instead of an `NA`
  placeholder. The table builder is also hardened to render body rows
  even when optional columns (`coverage_mcse`, `n_valid`, `n_failed`)
  are absent.
- `medsim_config(seed_stream=)` is deprecated: the value was stored and
  documented but has never been consumed by any medsim function. Passing
  a non-`NULL` value now emits a deprecation warning pointing at
  `medsim_run_parallel(seed=)`; the value is still stored for
  back-compatibility.
- The
  [`medsim_cache_load()`](https://data-wise.github.io/medsim/reference/medsim_cache_load.md)
  documentation example now unwraps the fingerprinted ground-truth cache
  format `list(truth=, fingerprint=)` written by
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  instead of treating the wrapper as the truth value.

&nbsp;

- Internal refactor:
  [`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md)
  and
  [`medsim_scenario_gauge()`](https://data-wise.github.io/medsim/reference/medsim_scenario_gauge.md)
  now share one linear-Gaussian-with-interaction data generator
  (`.medsim_lingauss_dgp()`) and one corner-mean truth helper
  (`.medsim_corner_means()`) instead of duplicating them verbatim.
  Scenario draws and closed-form truths are bit-identical to before.
- Internal refactor: the delta-method product SE is now a single helper
  (`.medsim_se_prod()`) used by the MC-CI and IPW adapters, and
  [`medsim_method_bounds()`](https://data-wise.github.io/medsim/reference/medsim_method_bounds.md)
  /
  [`medsim_method_pmed_mbco()`](https://data-wise.github.io/medsim/reference/medsim_method_pmed_mbco.md)
  fit their mediation regressions through the generalized
  `.medsim_md_fit_ab()` (named-coefficient lookup, replacing fragile
  positional `coef()[k]` indexing; one edge-path change:
  degenerate/collinear fits now error loudly instead of a silent
  `se = 0.1` fallback). Estimates, SEs, CIs, and p-values are
  bit-identical to before.
- Performance:
  [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  computes the three MBCO log-likelihoods once per imputation
  (`.medsim_mbco_lls()`) and derives both the D4 LRT statistic and the
  union-null branch indicator from that triple – about 6 instead of 10
  [`lm()`](https://rdrr.io/r/stats/lm.html) fits per imputation, with
  bit-identical statistics, p-values, and `branch_switch` values.
- Internal rename: `.gen_complete_med()` is now
  `.medsim_gen_complete_med()`, matching the package’s `.medsim_*`
  internal naming convention.

## medsim 0.4.0

### Bug fixes

- **Critical**:
  [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  produced correlated, non-independent replications across SLURM array
  chunks. `chunk_config$rep_offset` was computed but never consumed, and
  every chunk called `set.seed(config$seed)` with the same scalar – so
  every chunk regenerated the identical short sequence of
  “replications”, collapsing e.g. a 1000-replication/60-chunk study to
  ~17 truly distinct outcomes. Fixed by seeding each replication
  deterministically from `(scenario_name, global_rep_id)` via the new
  internal
  [`.medsim_det_seed()`](https://data-wise.github.io/medsim/reference/dot-medsim_det_seed.md),
  independent of chunk count, worker count, cluster type, or execution
  order. `config$seed`/`seed_stream` no longer affect
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)/[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  replication draws (they remain meaningful only for a direct
  `medsim_run_parallel(seed = ...)` call) – see updated docs on
  [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)
  and
  [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md).

### Testing infrastructure

- Two-tier test model for the simulation/parallel code. **Tier A**
  (`tests/testthat/`, always run by `R CMD check`) adds single-core
  correctness guards — coverage-instrument discrimination, cross-chunk
  RNG independence, truth-cache invalidation across the combine seam,
  failure-rate / NA-CI accounting, and chunk boundary cases. **Tier B**
  (`inst/hopper-tests/`, run only on a SLURM cluster, never by
  `R CMD check`) covers the at-scale and many-core-FORK properties CRAN
  cannot exercise: production-grid
  [`.medsim_det_seed()`](https://data-wise.github.io/medsim/reference/dot-medsim_det_seed.md)
  collision checks, real FORK-cluster RNG independence/reproducibility,
  and a self-contained can-fail coverage dogfood. Documented in the new
  `cluster-testing` vignette (all chunks non-evaluated, so it adds
  negligible check time).
- Building the Tier-A suite surfaced and fixed several latent defects:
  an interval-branch `failure_rate`, a stale truth-cache when the DGM
  changed, an empty-chunk phantom-replication footgun (`1:0`),
  missing-chunk detection in
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  (new `expected_chunks` argument), and an all-NA analyze crash.

### New features

- Gauge-residual estimand
  ([\#24](https://github.com/Data-Wise/medsim/issues/24)):
  [`medsim_scenario_gauge()`](https://data-wise.github.io/medsim/reference/medsim_scenario_gauge.md) +
  [`medsim_method_gauge()`](https://data-wise.github.io/medsim/reference/medsim_method_gauge.md)
  route the P1 gauge `P_med`/`W` coverage grid through
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md),
  mirroring Sobol; analytic + bootstrap (percentile) CI arms.

- ADEMP reporting
  ([\#25](https://github.com/Data-Wise/medsim/issues/25)): per-cell
  coverage Monte Carlo SE (`coverage_mcse`),
  [`medsim_nsim_for_mcse()`](https://data-wise.github.io/medsim/reference/medsim_nsim_for_mcse.md)
  for sizing replications to a target coverage MCSE, failed-run logging
  (`n_failed`, `failure_rate`) in coverage output,
  [`medsim_plot_se_vs_estimate()`](https://data-wise.github.io/medsim/reference/medsim_plot_se_vs_estimate.md)
  for SE-vs-estimate diagnostic scatter plots, and
  [`medsim_analyze_performance()`](https://data-wise.github.io/medsim/reference/medsim_analyze_performance.md)
  /
  [`medsim_table_performance()`](https://data-wise.github.io/medsim/reference/medsim_table_performance.md)
  for full ADEMP performance summaries (bias, empirical SE, model SE,
  RMSE, and MCSEs).

## medsim 0.3.1 (2026-06-19)

### Test coverage

- Expanded test coverage from 93.2% to 95.1%, meeting the \>80% target.
- New tests cover: `pbapply` progress path in `medsim_run_sequential`
  (via `with_mocked_bindings`), format-helper branches in
  `.format_time_latex`, `.format_pvalue_latex`, `.format_speedup_latex`,
  error-boxplot single- and multi-method branches in
  `medsim_plot_error_boxplot` (including the `parameter = NULL`
  all-columns path and RColorBrewer palette), timing warning path in
  `medsim_plot_timing`, coverage-plot stop in `medsim_plot_coverage`,
  combined-panel paths in `medsim_plot_combined_panel`, and
  `medsim_tables_workflow` tryCatch success and error-handler paths.
- Fix: `seed=` argument now honored in sequential fallback paths (R CMD
  check environment and single-core/few-task cases).

## medsim 0.3.0 (2026-06-19)

### New features

#### Estimand-kind abstraction (spine)

- [`medsim_estimand()`](https://data-wise.github.io/medsim/reference/medsim_estimand.md)
  — first-class estimand descriptor that tags a scenario with a kind
  (`"point"`, `"interval"`, `"probabilistic"`, `"numeric"`). Enables
  kind-aware dispatch across the entire simulation pipeline.
- [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
  gains an `estimand=` argument (default `NULL` for full backward
  compatibility with all v0.2.x code).
- [`medsim_validate_scenario()`](https://data-wise.github.io/medsim/reference/medsim_validate_scenario.md)
  skips hardcoded X/M/Y column checks for non-mediation kinds (numeric,
  interval).
- [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
  dispatches on estimand kind: `interval` → partial-ID / Imbens-Manski
  coverage branch; `probabilistic` → MBCO CI branch.

#### Hopper / SLURM cluster harness

- [`medsim_write_submit_script()`](https://data-wise.github.io/medsim/reference/medsim_write_submit_script.md)
  — generate a SBATCH array script for UNM CARC Hopper.
- [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  — run one chunk of replications (auto-detects `SLURM_ARRAY_TASK_ID`);
  saves `chunk_<id>.rds` to `output_dir`.
- [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  — read all chunk RDS files and return a single `medsim_results` object
  with deduplicated truth rows.
- [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)
  gains `chunk_id`, `n_chunks`, `array_size`, `seed_stream`,
  `partition`, `walltime`, `mem_per_cpu`, and `r_module` parameters.
- [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)
  gains deterministic **L’Ecuyer-CMRG** per-worker seeding (`seed=`
  argument) so chunked array runs are bit-reproducible.

#### P_med probabilistic mediation (`probabilistic` kind)

- [`medsim_scenario_pmed()`](https://data-wise.github.io/medsim/reference/medsim_scenario_pmed.md)
  — linear SEM scenario with cross-world potential-outcome ground truth
  computed at construction time (independent-residuals assumption).
- [`medsim_method_pmed_mbco()`](https://data-wise.github.io/medsim/reference/medsim_method_pmed_mbco.md)
  — two-branch MBCO CI for P_med; returns `pmed`, `pmed_ci_lower`,
  `pmed_ci_upper`, `pmed_p`, `branch_switch`, `converged`.

#### Differential-misclassification bounds (`interval` kind)

- [`medsim_scenario_dm()`](https://data-wise.github.io/medsim/reference/medsim_scenario_dm.md)
  — partial-ID bounds scenario for me-mediator / me-exposure studies;
  requires **medrobust** (Suggests); synthetic fallback when absent.
- [`medsim_method_bounds()`](https://data-wise.github.io/medsim/reference/medsim_method_bounds.md)
  — estimator adapter returning `{p}_lower/_upper`,
  `{p}_im_lower/_im_upper`, `feasible`, `falsified`.

#### Numeric accuracy scenarios (`numeric` kind)

- [`medsim_scenario_numeric()`](https://data-wise.github.io/medsim/reference/medsim_scenario_numeric.md)
  — thin wrapper for accuracy/timing studies (product-of-three,
  approximation quality); `estimand$kind = "numeric"` disables
  coverage/power analysis; only `error`, `abs_error`, `elapsed_sec`
  result columns.

### Bug fixes

- D4 p-value acceptance test: loosened tolerance to 0.1 (relative) to
  accommodate F-distribution tail sensitivity when mice produces
  slightly different imputations across environments; the F-statistic
  check remains tight at 1e-3.

## medsim 0.2.1 (2026-06-11)

### Documentation

- Synced `CLAUDE.md` and `R-UNIVERSE-STANDARDS.md` to the v0.2.0 state:
  the missing-data DGM feature is documented as shipped, the validated
  D4-MBCO method is described, and dependencies are reconciled —
  `Remotes:` is now `Data-Wise/medfit` only (PR
  [\#18](https://github.com/Data-Wise/medsim/issues/18) dropped the
  unused `missingmed`/`rmediation`).

No code changes since 0.2.0 — documentation and metadata only.

## medsim 0.2.0 (2026-06-11)

### New features

#### Missing-data + nonnormality DGM generators

Reusable data-generating utilities + missing-data mediation estimator
adapters for the Missing Effect study (MBCO-MI vs Monte-Carlo CI under
missingness × nonnormality), reusable by `sensitivity` /
`measurement error`.

- [`medsim_rnonnormal()`](https://data-wise.github.io/medsim/reference/medsim_rnonnormal.md)
  — draw values with a target marginal skew/excess kurtosis (Fleishman
  power method; pure base R, feasibility-guarded).
- [`medsim_amputate()`](https://data-wise.github.io/medsim/reference/medsim_amputate.md)
  — insert `NA`s under MCAR / MAR / MNAR via a rate-calibrated logistic
  amputer; multi-column targets; `mice` optional.
- [`medsim_scenario_missing()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing.md)
  /
  [`medsim_scenario_missing_grid()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing_grid.md)
  — missing-data mediation scenarios (X→M→Y with optional nonnormal
  residuals → amputation) + factorial grid builder.
- [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  /
  [`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/reference/medsim_method_mc_ci.md)
  /
  [`medsim_method_ipw()`](https://data-wise.github.io/medsim/reference/medsim_method_ipw.md)
  — estimator adapters returning the 6-field `method()` contract.
  [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  implements the validated **D4-stacked MBCO** union-null test (`mice`
  multiple imputation → MBCO likelihood-ratio statistic →
  Reiter/Chan–Meng D4 pooling → F reference), reproducing
  `mitml::testModels(method = "D4")` exactly; it degrades to the
  complete-case MBCO chi-square test when imputation is unavailable.
  [`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/reference/medsim_method_mc_ci.md)
  uses `RMediation::medci()` when present, else a base-R
  product-of-normals interval.
- [`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/reference/medsim_summarize_branch_switch.md)
  — summarize the MBCO union-null branch-switch rate per scenario.

The estimator adapters use `mice` (multiple imputation) + `RMediation`
(Monte-Carlo CI), both in `Suggests`; `mitml` is suggested for the D4
validation test. No new hard dependencies. (The earlier
`missingmed`/`rmediation` Suggests/Remotes were dropped — the validated
D4-MBCO method uses neither.)

### Bug fixes

- Vignette `getting-started.qmd`: the analysis step errored (“no ground
  truth available”) because
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  was called without `compute_truth`. Added a `compute_truth` example so
  [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
  computes accuracy metrics.

## medsim 0.1.1 (2026-05-11)

Cleanup release. No user-facing API changes; pure documentation,
metadata, and dependency-declaration hygiene.

### Dependencies

- Removed `probmed` and `medrobust` from `Suggests:` and `Remotes:`. The
  package’s example code never referenced them and the cross-package
  integration is now documented as an ecosystem pattern rather than a
  declared dependency. Users who want to test probmed or medrobust
  methods via medsim can install them separately from GitHub.
- `medfit` and `RMediation` remain in `Suggests:` because they are
  exercised by method-testing examples (medfit) or available via CRAN
  (RMediation).

### Documentation

- [`medsim_cache_init()`](https://data-wise.github.io/medsim/reference/medsim_cache_init.md),
  [`medsim_cache_info()`](https://data-wise.github.io/medsim/reference/medsim_cache_info.md),
  and
  [`medsim_cache_list()`](https://data-wise.github.io/medsim/reference/medsim_cache_list.md)
  examples now use
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)/[`tempfile()`](https://rdrr.io/r/base/tempfile.html)
  instead of writing to the working directory — makes the examples
  CRAN-clean and shows users a more idiomatic pattern.
- Vignette `getting-started.qmd`: the install chunk
  (`pak::pak("Data-Wise/medsim")`) is explicitly marked `eval: false` so
  it doesn’t run during vignette builds or R CMD check. Also fixed the
  install command to use canonical case `Data-Wise`.
- `README.md`: fixed three broken URLs (Codecov moved to
  `app.codecov.io`, medrobust link → GitHub repo, Discussions link
  removed since the feature isn’t enabled).
- `DESCRIPTION`: canonicalized GitHub URLs to use `Data-Wise` org case.
- Added `cran-comments.md` as a future-reference template (gitignored
  from package builds).

## medsim 0.1.0 (2026-05-11)

**Initial release.** First tagged version of medsim, providing
standardized infrastructure for Monte Carlo simulation studies in
mediation analysis as part of the mediationverse ecosystem.

### Features

#### Core infrastructure

- [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)
  — environment-aware configuration (test/local/cluster modes)
- [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  — simulation execution with progress tracking
- [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)
  — parallel execution with PSOCK/FORK clusters
- [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
  — define custom simulation scenarios
- [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md)
  — standard mediation scenarios

#### Analysis

- [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
  — summarize simulation results
- [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
  — coverage rate computation
- [`medsim_analyze_power()`](https://data-wise.github.io/medsim/reference/medsim_analyze_power.md)
  — power analysis
- [`medsim_compare_methods()`](https://data-wise.github.io/medsim/reference/medsim_compare_methods.md)
  — multi-method comparison

#### Visualization

- [`medsim_plot_coverage()`](https://data-wise.github.io/medsim/reference/medsim_plot_coverage.md),
  [`medsim_plot_error_boxplot()`](https://data-wise.github.io/medsim/reference/medsim_plot_error_boxplot.md),
  [`medsim_plot_timing()`](https://data-wise.github.io/medsim/reference/medsim_plot_timing.md),
  [`medsim_plot_combined_panel()`](https://data-wise.github.io/medsim/reference/medsim_plot_combined_panel.md)
  — publication-ready plots
- [`medsim_figures()`](https://data-wise.github.io/medsim/reference/medsim_figures.md)
  — one-call wrapper that generates all standard figures

#### LaTeX tables

- [`medsim_table_accuracy()`](https://data-wise.github.io/medsim/reference/medsim_table_accuracy.md),
  [`medsim_table_coverage()`](https://data-wise.github.io/medsim/reference/medsim_table_coverage.md),
  [`medsim_table_power()`](https://data-wise.github.io/medsim/reference/medsim_table_power.md),
  [`medsim_table_timing()`](https://data-wise.github.io/medsim/reference/medsim_table_timing.md),
  [`medsim_table_comparison()`](https://data-wise.github.io/medsim/reference/medsim_table_comparison.md)
  — publication-ready table generators
- [`medsim_tables()`](https://data-wise.github.io/medsim/reference/medsim_tables.md)
  and
  [`medsim_tables_workflow()`](https://data-wise.github.io/medsim/reference/medsim_tables_workflow.md)
  — one-call wrappers

#### End-to-end workflow

- [`medsim_workflow()`](https://data-wise.github.io/medsim/reference/medsim_workflow.md)
  — single function from simulation to manuscript-ready output
  (analysis + figures + tables)

#### HPC support

- Automatic SLURM / PBS / LSF environment detection
- Parallel processing with automatic core detection
- Ground truth caching for expensive computations

#### Ecosystem integration

- Part of the mediationverse ecosystem
- Designed to test methods from medfit, probmed, RMediation, and
  medrobust
- GitHub-only siblings (medfit, medrobust, probmed) resolved via
  DESCRIPTION `Remotes:` field so `pak::pkg_install(".")` works

### Bug fixes

- `medsim_run_parallel(packages = ...)` on PSOCK clusters used to crash
  with `object 'packages' not found`. The `packages` argument was a free
  variable inside a `clusterEvalQ` call and never reached the workers.
  Switched to `clusterCall`, which serializes the argument to workers.
  Regression test added.

### Documentation

- Comprehensive pkgdown website at <https://data-wise.github.io/medsim/>
- Vignette: `getting-started.qmd` (custom-scenarios and HPC vignettes
  planned for a future release)
- All exported functions have roxygen2 documentation; high-level docs
  (README, NEWS, CLAUDE.md) audited and consistent with the actual API

### Testing

- Unit tests for all core modules including `R/runner.R`,
  `R/parallel.R`, and `R/visualize.R`. Test suite covers happy paths,
  edge cases, input validation, error handling, and HPC-environment
  detection.

### Infrastructure

- GitHub Actions CI/CD on macOS, Windows, Ubuntu release, and Ubuntu
  oldrel-1; PR-time R-CMD-check completes in ~4 minutes
- Weekly R-devel signal via cron (`R-CMD-check-devel.yaml`)
- R-hub workflow available via manual dispatch (with Quarto installed
  for vignette re-build)
- Codecov integration for coverage tracking
- Concurrency cancel-in-progress on R-CMD-check and test-coverage

### Compatibility

- Requires R \>= 4.1.0
- Suggests: medfit, probmed, RMediation, medrobust (for method testing)

------------------------------------------------------------------------

*medsim is in active development. Breaking changes between 0.x releases
remain possible until a 1.0.0 release.*
