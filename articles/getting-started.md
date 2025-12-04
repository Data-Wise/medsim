# Getting Started with medsim

## Introduction

**medsim** provides standardized infrastructure for conducting Monte
Carlo simulation studies in mediation analysis. This vignette
demonstrates the basic workflow for running a simulation study.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("data-wise/medsim")
```

## Quick Start

### Step 1: Define Your Method

Create a function that takes simulated data and parameters, then returns
estimates:

``` r
library(medsim)

my_method <- function(data, params) {
  # Fit mediation models
  fit_m <- lm(M ~ X, data = data)
  fit_y <- lm(Y ~ X + M, data = data)
  
  # Extract path coefficients
  a <- coef(fit_m)["X"]
  b <- coef(fit_y)["M"]
  
  # Return results
  list(
    indirect = a * b,
    a_path = a,
    b_path = b
  )
}
```

### Step 2: Configure the Simulation

Choose an execution mode based on your needs:

``` r
# Quick validation (~30 seconds)
config_test <- medsim_config("test")

# Development (~15 minutes)
config_local <- medsim_config("local")

# Production (hours, for HPC clusters)
config_cluster <- medsim_config("cluster")
```

### Step 3: Select Scenarios

Use the built-in standard mediation scenarios:

``` r
scenarios <- medsim_scenarios_mediation()

# View available scenarios
names(scenarios)
#> [1] "Independent Paths"   "Moderate Correlation"
#> [3] "High Correlation"    "Suppression"
#> [5] "Non-zero Effects"    "Unequal Variances"
```

### Step 4: Run the Simulation

``` r
results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = medsim_config("test")  # Use "local" for real studies
)
```

### Step 5: Analyze Results

``` r
# Summary statistics
summary(results)

# Detailed analysis
analysis <- medsim_analyze(results)
print(analysis)
```

## Execution Modes

| Mode      | Replications | Runtime | Use Case         |
|-----------|--------------|---------|------------------|
| `test`    | 20           | ~30s    | Quick validation |
| `local`   | 100          | ~15m    | Development      |
| `cluster` | 1000         | hours   | Production       |

## Custom Scenarios

Define your own simulation scenarios:

``` r
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

# Combine with standard scenarios
all_scenarios <- c(medsim_scenarios_mediation(), list(my_scenario))
```

## HPC Cluster Support

medsim automatically detects HPC environments:

``` r
# Auto-detect environment
config <- medsim_config(mode = "auto")

# Check detected environment
medsim_detect_environment()
```

## Next Steps

- Explore the [Function
  Reference](https://data-wise.github.io/medsim/reference/index.md)
- Learn about [Custom
  Scenarios](https://data-wise.github.io/medsim/articles/custom-scenarios.md)
- See [Parallel
  Computing](https://data-wise.github.io/medsim/articles/parallel-computing.md)
  for HPC details
