# Validate Scenario

Checks that a scenario object is valid and can generate data correctly.

## Usage

``` r
medsim_validate_scenario(scenario, n = 10)
```

## Arguments

- scenario:

  A medsim_scenario object

- n:

  Sample size to test (default: 10)

## Value

TRUE if valid, throws error otherwise

## Examples

``` r
scenarios <- medsim_scenarios_mediation()
medsim_validate_scenario(scenarios[[1]])
#> * Scenario 'Independent' is valid
#> [1] TRUE
```
