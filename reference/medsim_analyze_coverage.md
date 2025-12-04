# Analyze Coverage Rates

Computes coverage rates for confidence intervals from simulation
results. Requires that the simulation method returns confidence interval
bounds (e.g., `ci_lower`, `ci_upper`).

## Usage

``` r
medsim_analyze_coverage(
  results,
  ci_levels = c(0.9, 0.95, 0.99),
  ci_suffix = "_ci",
  by_scenario = TRUE
)
```

## Arguments

- results:

  A medsim_results object from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)

- ci_levels:

  Numeric vector: nominal confidence levels to check (default: c(0.90,
  0.95, 0.99))

- ci_suffix:

  Character: suffix for CI columns (default: "\_ci") E.g., if parameter
  is "indirect", looks for "indirect_ci_lower" and "indirect_ci_upper"

- by_scenario:

  Logical: Compute coverage separately for each scenario (default TRUE)

## Value

A list with class "medsim_coverage" containing:

- `coverage`: data.frame with coverage rates

- `by_scenario`: data.frame with coverage by scenario (if
  by_scenario=TRUE)

- `summary`: Overall summary statistics

## Details

### Coverage Interpretation

Coverage rate is the proportion of confidence intervals that contain the
true parameter value. For a 95% CI, expect ~95% coverage in large
samples.

- Coverage \< nominal: CI too narrow (anti-conservative)

- Coverage ≈ nominal: CI has correct width

- Coverage \> nominal: CI too wide (conservative)

### Column Naming Conventions

The function looks for columns named:

- `{parameter}_ci_lower` and `{parameter}_ci_upper` (default)

- Or custom suffix: `{parameter}{ci_suffix}_lower` and
  `{parameter}{ci_suffix}_upper`

## See also

[`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md),
[`medsim_analyze_power()`](https://data-wise.github.io/medsim/reference/medsim_analyze_power.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Method that returns CIs
my_method <- function(data, params) {
  # ... estimation
  list(
    indirect = estimate,
    indirect_ci_lower = ci[1],
    indirect_ci_upper = ci[2]
  )
}

results <- medsim_run(my_method, scenarios, config)

# Analyze coverage
coverage <- medsim_analyze_coverage(results)

print(coverage$coverage)
#>   parameter  coverage_90  coverage_95  coverage_99
#>   indirect   0.902        0.951        0.991
} # }
```
