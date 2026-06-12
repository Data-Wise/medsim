# Changelog

## medsim (development version)

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

- [`medsim_rnonnormal()`](https://data-wise.github.io/medsim/dev/reference/medsim_rnonnormal.md)
  — draw values with a target marginal skew/excess kurtosis (Fleishman
  power method; pure base R, feasibility-guarded).
- [`medsim_amputate()`](https://data-wise.github.io/medsim/dev/reference/medsim_amputate.md)
  — insert `NA`s under MCAR / MAR / MNAR via a rate-calibrated logistic
  amputer; multi-column targets; `mice` optional.
- [`medsim_scenario_missing()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario_missing.md)
  /
  [`medsim_scenario_missing_grid()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario_missing_grid.md)
  — missing-data mediation scenarios (X→M→Y with optional nonnormal
  residuals → amputation) + factorial grid builder.
- [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mbco_mi.md)
  /
  [`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mc_ci.md)
  /
  [`medsim_method_ipw()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_ipw.md)
  — estimator adapters returning the 6-field `method()` contract.
  [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mbco_mi.md)
  implements the validated **D4-stacked MBCO** union-null test (`mice`
  multiple imputation → MBCO likelihood-ratio statistic →
  Reiter/Chan–Meng D4 pooling → F reference), reproducing
  `mitml::testModels(method = "D4")` exactly; it degrades to the
  complete-case MBCO chi-square test when imputation is unavailable.
  [`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mc_ci.md)
  uses
  [`RMediation::medci()`](https://data-wise.github.io/rmediation/reference/medci.html)
  when present, else a base-R product-of-normals interval.
- [`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/dev/reference/medsim_summarize_branch_switch.md)
  — summarize the MBCO union-null branch-switch rate per scenario.

The estimator adapters use `mice` (multiple imputation) + `RMediation`
(Monte-Carlo CI), both in `Suggests`; `mitml` is suggested for the D4
validation test. No new hard dependencies. (The earlier
`missingmed`/`rmediation` Suggests/Remotes were dropped — the validated
D4-MBCO method uses neither.)

### Bug fixes

- Vignette `getting-started.qmd`: the analysis step errored (“no ground
  truth available”) because
  [`medsim_run()`](https://data-wise.github.io/medsim/dev/reference/medsim_run.md)
  was called without `compute_truth`. Added a `compute_truth` example so
  [`medsim_analyze()`](https://data-wise.github.io/medsim/dev/reference/medsim_analyze.md)
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

- [`medsim_cache_init()`](https://data-wise.github.io/medsim/dev/reference/medsim_cache_init.md),
  [`medsim_cache_info()`](https://data-wise.github.io/medsim/dev/reference/medsim_cache_info.md),
  and
  [`medsim_cache_list()`](https://data-wise.github.io/medsim/dev/reference/medsim_cache_list.md)
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

- [`medsim_config()`](https://data-wise.github.io/medsim/dev/reference/medsim_config.md)
  — environment-aware configuration (test/local/cluster modes)
- [`medsim_run()`](https://data-wise.github.io/medsim/dev/reference/medsim_run.md)
  — simulation execution with progress tracking
- [`medsim_run_parallel()`](https://data-wise.github.io/medsim/dev/reference/medsim_run_parallel.md)
  — parallel execution with PSOCK/FORK clusters
- [`medsim_scenario()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario.md)
  — define custom simulation scenarios
- [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenarios_mediation.md)
  — standard mediation scenarios

#### Analysis

- [`medsim_analyze()`](https://data-wise.github.io/medsim/dev/reference/medsim_analyze.md)
  — summarize simulation results
- [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/dev/reference/medsim_analyze_coverage.md)
  — coverage rate computation
- [`medsim_analyze_power()`](https://data-wise.github.io/medsim/dev/reference/medsim_analyze_power.md)
  — power analysis
- [`medsim_compare_methods()`](https://data-wise.github.io/medsim/dev/reference/medsim_compare_methods.md)
  — multi-method comparison

#### Visualization

- [`medsim_plot_coverage()`](https://data-wise.github.io/medsim/dev/reference/medsim_plot_coverage.md),
  [`medsim_plot_error_boxplot()`](https://data-wise.github.io/medsim/dev/reference/medsim_plot_error_boxplot.md),
  [`medsim_plot_timing()`](https://data-wise.github.io/medsim/dev/reference/medsim_plot_timing.md),
  [`medsim_plot_combined_panel()`](https://data-wise.github.io/medsim/dev/reference/medsim_plot_combined_panel.md)
  — publication-ready plots
- [`medsim_figures()`](https://data-wise.github.io/medsim/dev/reference/medsim_figures.md)
  — one-call wrapper that generates all standard figures

#### LaTeX tables

- [`medsim_table_accuracy()`](https://data-wise.github.io/medsim/dev/reference/medsim_table_accuracy.md),
  [`medsim_table_coverage()`](https://data-wise.github.io/medsim/dev/reference/medsim_table_coverage.md),
  [`medsim_table_power()`](https://data-wise.github.io/medsim/dev/reference/medsim_table_power.md),
  [`medsim_table_timing()`](https://data-wise.github.io/medsim/dev/reference/medsim_table_timing.md),
  [`medsim_table_comparison()`](https://data-wise.github.io/medsim/dev/reference/medsim_table_comparison.md)
  — publication-ready table generators
- [`medsim_tables()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables.md)
  and
  [`medsim_tables_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables_workflow.md)
  — one-call wrappers

#### End-to-end workflow

- [`medsim_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_workflow.md)
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
