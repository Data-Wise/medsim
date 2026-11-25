# Analyze Statistical Power

Computes empirical power (rejection rate under alternative hypothesis)
from simulation results. Requires that the simulation method returns
p-values.

## Usage

``` r
medsim_analyze_power(
  results,
  alpha = 0.05,
  p_suffix = "_p",
  null_value = 0,
  by_scenario = TRUE
)
```

## Arguments

- results:

  A medsim_results object from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)

- alpha:

  Numeric: significance level (default: 0.05)

- p_suffix:

  Character: suffix for p-value columns (default: "\_p") E.g., if
  parameter is "indirect", looks for "indirect_p"

- null_value:

  Numeric: null hypothesis value (default: 0)

- by_scenario:

  Logical: Compute power separately for each scenario (default TRUE)

## Value

A list with class "medsim_power" containing:

- `power`: data.frame with power rates

- `by_scenario`: data.frame with power by scenario (if by_scenario=TRUE)

- `summary`: Overall summary statistics

## Details

### Power Interpretation

Power is the probability of correctly rejecting the null hypothesis when
it is false (i.e., detecting a true effect).

- Power = proportion of replications where p \< alpha

- Higher power = better ability to detect effects

- Power depends on: effect size, sample size, alpha level

### Notes

This function computes **empirical power** from simulations, not
theoretical power. For scenarios where the null is true (e.g., indirect
effect = 0), the rejection rate represents Type I error rate, not power.

## See also

[`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Method that returns p-values
my_method <- function(data, params) {
  # ... estimation and testing
  list(
    indirect = estimate,
    indirect_p = p_value
  )
}

results <- medsim_run(my_method, scenarios, config)

# Analyze power
power <- medsim_analyze_power(results, alpha = 0.05)

print(power$power)
#>   parameter  power    n_valid
#>   indirect   0.847    1000
} # }
```
