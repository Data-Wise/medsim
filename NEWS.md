# medsim (development version)

## New features

* **Pre-integration review hardening** (7 confirmed findings). The collapse
  audit excludes discrete contract fields BY NAME (`collapse_exclude`,
  default `converged`/`branch_switch`) instead of by observed distinctness --
  a TOTALLY collapsed estimate column (1-2 distinct values) is no longer
  invisible to the very check built for it. An all-failed run now fires an
  `all_failed` violation instead of combining clean. Mixed legacy+v2 chunk
  directories are order-independent (attributes stripped + one warning; the
  audit never mis-stops on legacy ids). `error` is a documented RESERVED
  method-field name -- a method returning it stops immediately instead of
  having `converged` silently forced to 0. Pilot checks: `n` compared
  numerically (200L vs 200 no longer false-positives) and NA-vs-value
  asymmetries against the pilot are mismatches. All-provenance-less chunk
  sets warn about the skipped SHA assertion. The combine `rbind` is
  schema-harmonized (ragged chunk columns cannot crash it), and the submit
  template creates `logs/` up front.

## New features

* **Gate C: chunk provenance + single-SHA assertion** (#34). Every chunk file
  now carries a provenance header (R version, medsim + key dependency
  versions, hostname, code SHA, sec/rep, UTC timestamp). The SHA
  **auto-detects** via `git rev-parse HEAD` in the running script's directory
  (explicit `code_sha =` overrides; `pkg:medsim-<version>` tag outside git).
  `medsim_combine_chunks()` asserts one SHA across all chunks -- catching a
  mid-run code edit + partial resubmit that would silently mix two code
  states. Provenance-less legacy chunks warn, never stop.

* **Gate D: pilot-subset positive control** (#34). Because seeds depend only
  on `(scenario, replication)`, a full run's reps `1..B_pilot` are
  draw-identical to an archived pilot at the same `n`.
  `medsim_combine_chunks(pilot_reference =, pilot_tol = 1e-9)` asserts
  **identity first** (sample size + scenario fingerprints -- a stale pilot
  fails as `pilot_config_differs`, never masquerading as a seeding
  regression), then compares estimate columns only (never `elapsed`) within
  tolerance -- a free regression check that harness, environment, and seeding
  are unchanged since the pilot passed.

* **Gate B: hardened SLURM submit template** (#34, fixes #37).
  `medsim_write_submit_script()` now emits a fail-loud script: `#!/bin/bash -l`
  (on Hopper `module` is defined only in login shells -- the old plain
  `#!/bin/bash` template failed there with "module: command not found"),
  `set -eo pipefail`, a hard-failing `module load` (never `|| true`), a
  `command -v Rscript` pre-check, `#SBATCH --requeue`, and `Rscript` as the
  final command so its exit code is the task's exit code. Optional
  `config$array_throttle = K` emits `--array=1-N%K`. There is deliberately no
  shell-side output-file gate -- completeness is audited at combine time by
  Gate A. The `inst/hopper-tests/submit_chunk.sh` exemplar is updated to the
  same pattern, and the `chunk_%04d.rds` naming convention is centralized in
  an internal helper shared by writer and combiner.

* **Gate A: combine-step seed-provenance audit** (#34, SPEC-medsim-chunked-run-gates).
  `medsim_combine_chunks()` now audits the combined grid and the new
  `medsim_audit_results()` runs the same audit standalone: per-scenario
  `replication` **contiguity** (gapless, duplicate-free `1..max`, equal across
  scenarios -- self-validating, no external count needed; optional `nsim =`
  pin), the **collapse signature** on every continuous estimate column
  (`n_distinct > collapse_threshold * n_ok`; the 0.3.1 seed collapse produced
  ~17 distinct outcomes in 1000 while every chunk exited 0), all-failed cells
  reported as `cell_failed`, and **cross-scenario seed collisions** in
  `.medsim_det_seed()`'s hash space. Violations route through one
  `on_violation = c("stop", "warn", "ignore")` control; the default `"stop"`
  signals a `medsim_combine_violation` condition that **carries the combined
  results** (`tryCatch(..., medsim_combine_violation = function(e) e$results)`
  recovers an hours-long run's good cells). **Breaking**: the old
  `expected_chunks` warn-and-combine default is now a violation under the same
  control -- for a deliberate partial combine (interim look at a running
  array), pass `on_violation = "warn"`. Legacy (pre-schema-v2) frames skip the
  id-based audits with a warning, never an error.

## Bug fixes

* **Chunked runs: `replication` is now the GLOBAL rep id** (schema v2; #36,
  SPEC-medsim-chunked-run-gates P1). Previously each SLURM chunk emitted
  chunk-local ids (`1..chunk_size`), so a combined 4-chunk/nsim-20 run carried
  5 distinct `replication` values each appearing 4 times, rows were not
  uniquely identifiable, and `medsim_analyze()` reported the chunk size as
  `n_replications`. Standalone (non-chunked) runs are unchanged. Result frames
  now carry `medsim_schema = 2L` and a `medsim_meta_cols` provenance attribute
  recording which columns the runner wrote (consumed by `medsim_analyze()`;
  legacy frames fall back to the old name list).
* **`medsim_combine_chunks()` rebuilds `$summary` and `$config`** (P2).
  It previously returned chunk 1's slice statistics and chunk-sized
  `config$n_replications` as if they described the combined run.
* **One failed replication no longer crashes the run** (P3). Failure rows and
  success rows had different columns, so `rbind` errored
  ("names do not match previous names") -- in chunk mode converting one
  transient rep failure into a missing chunk file. All rows now share one
  schema: an `error` column on every row (`NA` on success), `NA` estimates and
  `converged = 0` on failure rows.
* **Logical method-contract fields are no longer dropped** (P3).
  `branch_switch`/`converged` returned as logicals (including logical `NA`)
  were silently excluded from `$results`, breaking
  `medsim_summarize_branch_switch()`.
* **Chunk mode no longer writes intermediate CSVs** (#38, P3). Concurrent
  array tasks sharing an `output_dir` overwrote each other's fixed-name
  `results_scenario_*.csv`/`all_results.csv` (last writer wins), leaving
  partial data that looked complete. The chunk `.rds` is the artifact;
  standalone runs still write their CSVs.

# medsim 0.4.0

## Bug fixes

* **Critical**: `medsim_run_chunk()` produced correlated, non-independent
  replications across SLURM array chunks. `chunk_config$rep_offset` was
  computed but never consumed, and every chunk called `set.seed(config$seed)`
  with the same scalar -- so every chunk regenerated the identical short
  sequence of "replications", collapsing e.g. a 1000-replication/60-chunk
  study to ~17 truly distinct outcomes. Fixed by seeding each replication
  deterministically from `(scenario_name, global_rep_id)` via the new
  internal `.medsim_det_seed()`, independent of chunk count, worker count,
  cluster type, or execution order. `config$seed`/`seed_stream` no longer
  affect `medsim_run()`/`medsim_run_chunk()` replication draws (they remain
  meaningful only for a direct `medsim_run_parallel(seed = ...)` call) --
  see updated docs on `medsim_config()` and `medsim_run_parallel()`.

## Testing infrastructure

* Two-tier test model for the simulation/parallel code. **Tier A**
  (`tests/testthat/`, always run by `R CMD check`) adds single-core
  correctness guards — coverage-instrument discrimination, cross-chunk RNG
  independence, truth-cache invalidation across the combine seam, failure-rate
  / NA-CI accounting, and chunk boundary cases. **Tier B**
  (`inst/hopper-tests/`, run only on a SLURM cluster, never by `R CMD check`)
  covers the at-scale and many-core-FORK properties CRAN cannot exercise:
  production-grid `.medsim_det_seed()` collision checks, real FORK-cluster RNG
  independence/reproducibility, and a self-contained can-fail coverage dogfood.
  Documented in the new `cluster-testing` vignette (all chunks non-evaluated,
  so it adds negligible check time).
* Building the Tier-A suite surfaced and fixed several latent defects: an
  interval-branch `failure_rate`, a stale truth-cache when the DGM changed, an
  empty-chunk phantom-replication footgun (`1:0`), missing-chunk detection in
  `medsim_combine_chunks()` (new `expected_chunks` argument), and an all-NA
  analyze crash.

## New features

* Gauge-residual estimand (#24): `medsim_scenario_gauge()` + `medsim_method_gauge()`
  route the P1 gauge `P_med`/`W` coverage grid through `medsim_run()`, mirroring
  Sobol; analytic + bootstrap (percentile) CI arms.

* ADEMP reporting (#25): per-cell coverage Monte Carlo SE (`coverage_mcse`),
  `medsim_nsim_for_mcse()` for sizing replications to a target coverage MCSE,
  failed-run logging (`n_failed`, `failure_rate`) in coverage output,
  `medsim_plot_se_vs_estimate()` for SE-vs-estimate diagnostic scatter plots,
  and `medsim_analyze_performance()` / `medsim_table_performance()` for full
  ADEMP performance summaries (bias, empirical SE, model SE, RMSE, and MCSEs).

# medsim 0.3.1 (2026-06-19)

## Test coverage

* Expanded test coverage from 93.2% to 95.1%, meeting the >80% target.
* New tests cover: `pbapply` progress path in `medsim_run_sequential` (via
  `with_mocked_bindings`), format-helper branches in `.format_time_latex`,
  `.format_pvalue_latex`, `.format_speedup_latex`, error-boxplot single- and
  multi-method branches in `medsim_plot_error_boxplot` (including the
  `parameter = NULL` all-columns path and RColorBrewer palette), timing warning
  path in `medsim_plot_timing`, coverage-plot stop in `medsim_plot_coverage`,
  combined-panel paths in `medsim_plot_combined_panel`, and `medsim_tables_workflow`
  tryCatch success and error-handler paths.
* Fix: `seed=` argument now honored in sequential fallback paths (R CMD check
  environment and single-core/few-task cases).

# medsim 0.3.0 (2026-06-19)

## New features

### Estimand-kind abstraction (spine)

* `medsim_estimand()` — first-class estimand descriptor that tags a scenario with
  a kind (`"point"`, `"interval"`, `"probabilistic"`, `"numeric"`). Enables
  kind-aware dispatch across the entire simulation pipeline.
* `medsim_scenario()` gains an `estimand=` argument (default `NULL` for full
  backward compatibility with all v0.2.x code).
* `medsim_validate_scenario()` skips hardcoded X/M/Y column checks for
  non-mediation kinds (numeric, interval).
* `medsim_analyze_coverage()` dispatches on estimand kind: `interval` → partial-ID
  / Imbens-Manski coverage branch; `probabilistic` → MBCO CI branch.

### Hopper / SLURM cluster harness

* `medsim_write_submit_script()` — generate a SBATCH array script for UNM CARC Hopper.
* `medsim_run_chunk()` — run one chunk of replications (auto-detects
  `SLURM_ARRAY_TASK_ID`); saves `chunk_<id>.rds` to `output_dir`.
* `medsim_combine_chunks()` — read all chunk RDS files and return a single
  `medsim_results` object with deduplicated truth rows.
* `medsim_config()` gains `chunk_id`, `n_chunks`, `array_size`, `seed_stream`,
  `partition`, `walltime`, `mem_per_cpu`, and `r_module` parameters.
* `medsim_run_parallel()` gains deterministic **L'Ecuyer-CMRG** per-worker seeding
  (`seed=` argument) so chunked array runs are bit-reproducible.

### P_med probabilistic mediation (`probabilistic` kind)

* `medsim_scenario_pmed()` — linear SEM scenario with cross-world potential-outcome
  ground truth computed at construction time (independent-residuals assumption).
* `medsim_method_pmed_mbco()` — two-branch MBCO CI for P_med; returns `pmed`,
  `pmed_ci_lower`, `pmed_ci_upper`, `pmed_p`, `branch_switch`, `converged`.

### Differential-misclassification bounds (`interval` kind)

* `medsim_scenario_dm()` — partial-ID bounds scenario for me-mediator / me-exposure
  studies; requires **medrobust** (Suggests); synthetic fallback when absent.
* `medsim_method_bounds()` — estimator adapter returning `{p}_lower/_upper`,
  `{p}_im_lower/_im_upper`, `feasible`, `falsified`.

### Numeric accuracy scenarios (`numeric` kind)

* `medsim_scenario_numeric()` — thin wrapper for accuracy/timing studies (product-of-three,
  approximation quality); `estimand$kind = "numeric"` disables coverage/power
  analysis; only `error`, `abs_error`, `elapsed_sec` result columns.

## Bug fixes

* D4 p-value acceptance test: loosened tolerance to 0.1 (relative) to accommodate
  F-distribution tail sensitivity when mice produces slightly different imputations
  across environments; the F-statistic check remains tight at 1e-3.

# medsim 0.2.1 (2026-06-11)

## Documentation

* Synced `CLAUDE.md` and `R-UNIVERSE-STANDARDS.md` to the v0.2.0 state: the
  missing-data DGM feature is documented as shipped, the validated D4-MBCO method
  is described, and dependencies are reconciled — `Remotes:` is now
  `Data-Wise/medfit` only (PR #18 dropped the unused `missingmed`/`rmediation`).

No code changes since 0.2.0 — documentation and metadata only.

# medsim 0.2.0 (2026-06-11)

## New features

### Missing-data + nonnormality DGM generators

Reusable data-generating utilities + missing-data mediation estimator adapters for the
Missing Effect study (MBCO-MI vs Monte-Carlo CI under missingness × nonnormality),
reusable by `sensitivity` / `measurement error`.

* `medsim_rnonnormal()` — draw values with a target marginal skew/excess kurtosis
  (Fleishman power method; pure base R, feasibility-guarded).
* `medsim_amputate()` — insert `NA`s under MCAR / MAR / MNAR via a rate-calibrated
  logistic amputer; multi-column targets; `mice` optional.
* `medsim_scenario_missing()` / `medsim_scenario_missing_grid()` — missing-data
  mediation scenarios (X→M→Y with optional nonnormal residuals → amputation) + factorial
  grid builder.
* `medsim_method_mbco_mi()` / `medsim_method_mc_ci()` / `medsim_method_ipw()` — estimator
  adapters returning the 6-field `method()` contract. `medsim_method_mbco_mi()` implements
  the validated **D4-stacked MBCO** union-null test (`mice` multiple imputation → MBCO
  likelihood-ratio statistic → Reiter/Chan–Meng D4 pooling → F reference), reproducing
  `mitml::testModels(method = "D4")` exactly; it degrades to the complete-case MBCO
  chi-square test when imputation is unavailable. `medsim_method_mc_ci()` uses
  `RMediation::medci()` when present, else a base-R product-of-normals interval.
* `medsim_summarize_branch_switch()` — summarize the MBCO union-null branch-switch rate
  per scenario.

The estimator adapters use `mice` (multiple imputation) + `RMediation` (Monte-Carlo CI),
both in `Suggests`; `mitml` is suggested for the D4 validation test. No new hard
dependencies. (The earlier `missingmed`/`rmediation` Suggests/Remotes were dropped — the
validated D4-MBCO method uses neither.)

## Bug fixes

* Vignette `getting-started.qmd`: the analysis step errored ("no ground truth available")
  because `medsim_run()` was called without `compute_truth`. Added a `compute_truth`
  example so `medsim_analyze()` computes accuracy metrics.

# medsim 0.1.1 (2026-05-11)

Cleanup release. No user-facing API changes; pure documentation,
metadata, and dependency-declaration hygiene.

## Dependencies

* Removed `probmed` and `medrobust` from `Suggests:` and `Remotes:`. The
  package's example code never referenced them and the cross-package
  integration is now documented as an ecosystem pattern rather than a
  declared dependency. Users who want to test probmed or medrobust
  methods via medsim can install them separately from GitHub.
* `medfit` and `RMediation` remain in `Suggests:` because they are
  exercised by method-testing examples (medfit) or available via CRAN
  (RMediation).

## Documentation

* `medsim_cache_init()`, `medsim_cache_info()`, and `medsim_cache_list()`
  examples now use `tempdir()`/`tempfile()` instead of writing to the
  working directory — makes the examples CRAN-clean and shows users a
  more idiomatic pattern.
* Vignette `getting-started.qmd`: the install chunk
  (`pak::pak("Data-Wise/medsim")`) is explicitly marked `eval: false` so
  it doesn't run during vignette builds or R CMD check. Also fixed the
  install command to use canonical case `Data-Wise`.
* `README.md`: fixed three broken URLs (Codecov moved to `app.codecov.io`,
  medrobust link → GitHub repo, Discussions link removed since the feature
  isn't enabled).
* `DESCRIPTION`: canonicalized GitHub URLs to use `Data-Wise` org case.
* Added `cran-comments.md` as a future-reference template (gitignored
  from package builds).

# medsim 0.1.0 (2026-05-11)

**Initial release.** First tagged version of medsim, providing standardized
infrastructure for Monte Carlo simulation studies in mediation analysis as
part of the mediationverse ecosystem.

## Features

### Core infrastructure

* `medsim_config()` — environment-aware configuration (test/local/cluster modes)
* `medsim_run()` — simulation execution with progress tracking
* `medsim_run_parallel()` — parallel execution with PSOCK/FORK clusters
* `medsim_scenario()` — define custom simulation scenarios
* `medsim_scenarios_mediation()` — standard mediation scenarios

### Analysis

* `medsim_analyze()` — summarize simulation results
* `medsim_analyze_coverage()` — coverage rate computation
* `medsim_analyze_power()` — power analysis
* `medsim_compare_methods()` — multi-method comparison

### Visualization

* `medsim_plot_coverage()`, `medsim_plot_error_boxplot()`,
  `medsim_plot_timing()`, `medsim_plot_combined_panel()` — publication-ready
  plots
* `medsim_figures()` — one-call wrapper that generates all standard figures

### LaTeX tables

* `medsim_table_accuracy()`, `medsim_table_coverage()`, `medsim_table_power()`,
  `medsim_table_timing()`, `medsim_table_comparison()` — publication-ready
  table generators
* `medsim_tables()` and `medsim_tables_workflow()` — one-call wrappers

### End-to-end workflow

* `medsim_workflow()` — single function from simulation to manuscript-ready
  output (analysis + figures + tables)

### HPC support

* Automatic SLURM / PBS / LSF environment detection
* Parallel processing with automatic core detection
* Ground truth caching for expensive computations

### Ecosystem integration

* Part of the mediationverse ecosystem
* Designed to test methods from medfit, probmed, RMediation, and medrobust
* GitHub-only siblings (medfit, medrobust, probmed) resolved via DESCRIPTION
  `Remotes:` field so `pak::pkg_install(".")` works

## Bug fixes

* `medsim_run_parallel(packages = ...)` on PSOCK clusters used to crash with
  `object 'packages' not found`. The `packages` argument was a free variable
  inside a `clusterEvalQ` call and never reached the workers. Switched to
  `clusterCall`, which serializes the argument to workers. Regression test
  added.

## Documentation

* Comprehensive pkgdown website at https://data-wise.github.io/medsim/
* Vignette: `getting-started.qmd` (custom-scenarios and HPC vignettes planned
  for a future release)
* All exported functions have roxygen2 documentation; high-level docs
  (README, NEWS, CLAUDE.md) audited and consistent with the actual API

## Testing

* Unit tests for all core modules including `R/runner.R`, `R/parallel.R`, and
  `R/visualize.R`. Test suite covers happy paths, edge cases, input
  validation, error handling, and HPC-environment detection.

## Infrastructure

* GitHub Actions CI/CD on macOS, Windows, Ubuntu release, and Ubuntu
  oldrel-1; PR-time R-CMD-check completes in ~4 minutes
* Weekly R-devel signal via cron (`R-CMD-check-devel.yaml`)
* R-hub workflow available via manual dispatch (with Quarto installed for
  vignette re-build)
* Codecov integration for coverage tracking
* Concurrency cancel-in-progress on R-CMD-check and test-coverage

## Compatibility

* Requires R >= 4.1.0
* Suggests: medfit, probmed, RMediation, medrobust (for method testing)

---

*medsim is in active development. Breaking changes between 0.x releases
remain possible until a 1.0.0 release.*
