# CLAUDE.md for medsim Package

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## About This Package

**medsim** provides standardized infrastructure for conducting Monte Carlo simulation studies in mediation analysis. It is part of the mediationverse ecosystem.

### Core Mission

Provide a complete, reusable simulation framework that eliminates the need to repeatedly implement parallel processing, progress reporting, result analysis, and visualization across different mediation research projects.

### Key Features

- Environment-aware execution (local vs HPC cluster)
- Three execution modes (test, local, cluster)
- Parallel processing with progress bars
- Ground truth caching
- Automated analysis and visualization
- Publication-ready output (figures and LaTeX tables)

---

## Common Development Commands

```r
# Install dependencies and check package
remotes::install_deps(dependencies = TRUE)
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")

# Development workflow
devtools::load_all()
devtools::document()
devtools::test()
```

---

## Code Architecture

### Core Functions

| Function | Purpose |
|----------|---------|
| `medsim_config()` | Environment-aware configuration |
| `medsim_run()` | Execute simulations with parallel processing |
| `medsim_scenario()` | Define custom simulation scenarios |
| `medsim_scenarios_mediation()` | Standard mediation scenarios |
| `medsim_analyze()` | Summarize simulation results |
| `medsim_figures()` | Generate publication-ready figures |
| `medsim_tables()` | Generate LaTeX tables |
| `medsim_workflow()` | Complete simulation-to-manuscript pipeline |

### Execution Modes

| Mode | Replications | Use Case |
|------|--------------|----------|
| `test` | 20 | Quick validation |
| `local` | 100 | Development |
| `cluster` | 1000+ | Production |

---

## Ecosystem Coordination

medsim is a **support package** in the mediationverse ecosystem.

### Role in Ecosystem

medsim tests and validates methods from other mediationverse packages:

| Package | What medsim tests |
|---------|-------------------|
| medfit | Model extraction, bootstrap inference |
| probmed | P_med coverage, accuracy |
| RMediation | DOP/MBCO CI coverage |
| medrobust | Sensitivity bounds |

### Central Planning

Ecosystem coordination managed in `/Users/dt/mediation-planning/`:
- `ECOSYSTEM-COORDINATION.md` - Version matrix, change propagation
- `MONTHLY-CHECKLIST.md` - Recurring ecosystem health checks

### Related Packages

| Package | Repository | Purpose |
|---------|-----------|---------|
| medfit | https://github.com/data-wise/medfit | Foundation (model fitting, extraction) |
| probmed | https://github.com/data-wise/probmed | P_med effect size |
| RMediation | https://github.com/data-wise/rmediation | Confidence intervals |
| medrobust | https://github.com/data-wise/medrobust | Sensitivity analysis |

---

## Development Guidelines

### Naming Conventions

- Functions: `medsim_*()` prefix for all exported functions
- Internal functions: `.medsim_*()` with dot prefix
- Arguments: `snake_case`

### Testing

- Target: >80% coverage
- Test all execution modes (test, local)
- Test HPC detection logic
- Test output generation (figures, tables)

---

**Last Updated**: 2025-12-04
