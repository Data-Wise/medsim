# Create Standard Mediation Scenarios

Creates a list of standard mediation scenarios for simulation studies.
These scenarios cover common patterns in mediation analysis including
independent paths, various correlation structures, suppression effects,
and non-standard conditions.

## Usage

``` r
medsim_scenarios_mediation()
```

## Value

A list of scenario objects, each with name, description, and
data_generator function

## Details

### Standard Scenarios

1.  **Independent Paths**: No correlation between X, M, Y

2.  **Moderate Correlation**: rho = 0.3 between all pairs

3.  **High Correlation**: rho = 0.7 between all pairs

4.  **Suppression**: Mixed positive and negative correlations

5.  **Non-zero Effects**: Small to moderate true effects

6.  **Unequal Variances**: Different residual variances

Each scenario generates data with:

- Sample size n (default: 200)

- Treatment X, Mediator M, Outcome Y

- Known population parameters for validation

## See also

[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
for creating custom scenarios

## Examples

``` r
# Get all standard scenarios
scenarios <- medsim_scenarios_mediation()

# See scenario names
sapply(scenarios, function(s) s$name)
#> [1] "Independent"          "Moderate Correlation" "High Correlation"    
#> [4] "Suppression"          "Non-zero Effects"     "Unequal Variances"   

# Generate data from first scenario
data <- scenarios[[1]]$data_generator(n = 100)
head(data)
#>            X          M           Y
#> 1  1.3054383  0.6181951 -0.03021532
#> 2  1.7953239  0.9602407  1.35105074
#> 3 -1.1678352 -0.3455126  0.51669274
#> 4 -1.1522876  0.2732976  0.75505594
#> 5 -0.9144555  0.1567973  2.68074875
#> 6  0.5287280  0.7169308  0.38119363

# Access scenario parameters
scenarios[[1]]$params
#> $a
#> [1] 0.3
#> 
#> $b
#> [1] 0.3
#> 
#> $c_prime
#> [1] 0
#> 
#> $indirect
#> [1] 0.09
#> 
#> $rho_xm
#> [1] 0
#> 
#> $rho_my
#> [1] 0
#> 
#> $rho_xy
#> [1] 0
#> 
```
