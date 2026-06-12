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
