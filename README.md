# medsim <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/data-wise/medsim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/data-wise/medsim/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/data-wise/medsim/branch/main/graph/badge.svg)](https://app.codecov.io/gh/data-wise/medsim?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Standardized infrastructure for conducting Monte Carlo simulation studies in mediation analysis.

## Overview

**medsim** provides a complete toolkit for running rigorous simulation studies in mediation analysis. It eliminates the need to repeatedly implement parallel processing, progress reporting, result analysis, and visualization across different projects.

### Key Features

- **Environment-aware execution**: Automatically detects local machine vs HPC cluster
- **Three execution modes**: test (~30s), local (~15min), cluster (hours)
- **Parallel processing**: Built-in parallelization with progress bars
- **Ground truth caching**: Avoid expensive recomputation
- **Automated analysis**: Summary statistics, accuracy metrics, coverage rates
- **Publication-ready output**: Figures and LaTeX tables with one function

## Mediationverse Ecosystem

**medsim** is part of the **mediationverse** ecosystem for mediation analysis in R:

| Package | Purpose | Role |
|---------|---------|------|
| [**medfit**](https://github.com/data-wise/medfit) | Model fitting, extraction, bootstrap | Foundation |
| [**probmed**](https://github.com/data-wise/probmed) | Probabilistic effect size (P_med) | Application |
| [**RMediation**](https://github.com/data-wise/rmediation) | Confidence intervals (DOP, MBCO) | Application |
| [**medrobust**](https://github.com/data-wise/medrobust) | Sensitivity analysis | Application |
| **medsim** (this) | Simulation infrastructure | Support |

See [Ecosystem Coordination](https://github.com/data-wise/medfit/blob/main/planning/ECOSYSTEM.md) for version compatibility and development guidelines.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("data-wise/medsim")
```

## Quick Start

```r
library(medsim)

# 1. Define your method
my_method <- function(data, params) {
  fit_m <- lm(M ~ X, data = data)
  fit_y <- lm(Y ~ X + M, data = data)

  a <- coef(fit_m)["X"]
  b <- coef(fit_y)["M"]

  list(indirect = a * b, a = a, b = b)
}

# 2. Configure and run
config <- medsim_config("local")
scenarios <- medsim_scenarios_mediation()

results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = config
)

# 3. Analyze and visualize
medsim_workflow(results, output_dir = "results")
```

**Output**: Complete simulation results with figures and tables in `results/`

## Execution Modes

Three modes for different use cases:

| Mode | Replications | Runtime | Use Case |
|------|--------------|---------|----------|
| `test` | 20 | ~30s | Quick validation |
| `local` | 100 | ~15m | Development |
| `cluster` | 1000 | hours | Production |

```r
# Auto-detect environment
config <- medsim_config(mode = "auto")

# Or specify explicitly
config_test <- medsim_config(mode = "test")      # Quick check
config_local <- medsim_config(mode = "local")    # Development
config_cluster <- medsim_config(mode = "cluster")  # Production
```

## Standard Scenarios

Six standard mediation scenarios covering common patterns:

```r
scenarios <- medsim_scenarios_mediation()

# 1. Independent paths
# 2. Moderate correlation (ρ = 0.3)
# 3. High correlation (ρ = 0.7)
# 4. Suppression (mixed signs)
# 5. Non-zero effects (with direct path)
# 6. Unequal variances

# View scenario details
print(scenarios[[1]])

# Generate data from scenario
data <- scenarios[[1]]$data_generator(n = 200)
```

## Custom Scenarios

Define your own scenarios:

```r
my_scenario <- medsim_scenario(
  name = "Large Effects",
  description = "Both paths have large effects",
  data_generator = function(n = 200) {
    X <- rnorm(n)
    M <- 0.7 * X + rnorm(n)
    Y <- 0.7 * M + rnorm(n)
    data.frame(X = X, M = M, Y = Y)
  },
  params = list(
    a = 0.7,
    b = 0.7,
    indirect = 0.49
  )
)

# Use with standard scenarios
all_scenarios <- c(medsim_scenarios_mediation(), list(my_scenario))
```

## Complete Workflow

Single function for simulation → analysis → figures → tables:

```r
medsim_workflow(
  method = my_method,
  scenarios = scenarios,
  config = medsim_config("local"),
  output_dir = "manuscript",
  run_simulation = TRUE  # Set FALSE to use cached results
)
```

**Generates**:
- `manuscript/all_results.csv` - Complete simulation data
- `manuscript/summary_stats.csv` - Aggregated results
- `manuscript/figures/*.pdf` - Publication-ready figures
- `manuscript/tables/*.tex` - LaTeX tables

## HPC Cluster Support

**medsim** automatically detects HPC environments (SLURM, PBS, LSF):

```bash
#!/bin/bash
#SBATCH --job-name=medsim
#SBATCH --nodes=1
#SBATCH --cpus-per-task=20
#SBATCH --time=24:00:00

module load R/4.3.0
Rscript run_simulation.R
```

```r
# run_simulation.R
library(medsim)

# Auto-detects cluster and uses all allocated cores
config <- medsim_config(mode = "auto")

results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = config
)
```

## Integration with Mediation Ecosystem

**medsim** is designed to work seamlessly with:

- **medfit**: Model infrastructure and extraction
- **probmed**: Probabilistic effect size (P_med)
- **RMediation**: Confidence intervals (DOP, MBCO, MC)
- **medrobust**: Sensitivity analysis

```r
library(medsim)
library(medfit)
library(probmed)

# Simulate P_med coverage
pmed_method <- function(data, params) {
  fit_m <- lm(M ~ X, data = data)
  fit_y <- lm(Y ~ X + M, data = data)

  med_data <- medfit::extract_mediation(
    fit_m, model_y = fit_y,
    treatment = "X", mediator = "M"
  )

  p_med <- probmed::compute_pmed(med_data)

  # Bootstrap CI
  boot <- medfit::bootstrap_mediation(
    med_data,
    statistic = probmed::compute_pmed,
    n_boot = 1000
  )

  list(
    estimate = p_med,
    ci_lower = boot@ci_lower,
    ci_upper = boot@ci_upper,
    truth = params$true_pmed
  )
}

results <- medsim_run(pmed_method, scenarios, config)
coverage <- medsim_analyze_coverage(results)
```

## Example: Method Comparison

Compare methods from different packages:

```r
methods <- list(
  pmed = function(data, params) {
    # ... compute P_med ...
  },

  dop = function(data, params) {
    # ... compute DOP CI ...
  },

  bounds = function(data, params) {
    # ... compute sensitivity bounds ...
  }
)

results <- medsim_compare_methods(
  methods = methods,
  scenarios = scenarios,
  config = config
)

# Generate comparison table
medsim_comparison_table(results)
#>           Accuracy  Coverage  Power  Runtime
#> P_med     0.95      0.948     0.82   3.2s
#> DOP CI    0.94      0.952     0.81   0.1s
#> Bounds    0.93      0.945     0.78   1.5s
```

## Documentation

- [Getting Started](https://data-wise.github.io/medsim/articles/getting-started.html)
- [Custom Scenarios](https://data-wise.github.io/medsim/articles/custom-scenarios.html)
- [Parallel Computing & HPC](https://data-wise.github.io/medsim/articles/parallel-computing.html)
- [Mediation Simulations](https://data-wise.github.io/medsim/articles/mediation-sims.html)
- [Function Reference](https://data-wise.github.io/medsim/reference/index.html)

## Design Philosophy

**medsim** is inspired by the excellent simulation infrastructure in the [product-of-three distribution project](https://github.com/data-wise/prod3) and follows these principles:

1. **Configuration over repetition**: Write simulation logic once, run in multiple modes
2. **Environment awareness**: Seamlessly scale from laptop to cluster
3. **Reproducibility by design**: Automatic seed management, session tracking
4. **Publication ready**: One function generates manuscript-ready output

## Citation

If you use **medsim** in your research, please cite:

```r
citation("medsim")
```

## Getting Help

- [GitHub Issues](https://github.com/data-wise/medsim/issues) - Bug reports and feature requests
- [Documentation](https://data-wise.github.io/medsim/) - Comprehensive guides and examples
- [Discussions](https://github.com/data-wise/medsim/discussions) - Questions and community

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## License

GPL (>= 3)
