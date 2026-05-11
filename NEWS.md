# medsim (development version)

<!-- Add notes here for the next release cycle. -->

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
