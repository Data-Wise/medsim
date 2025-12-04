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
#>            X          M          Y
#> 1  0.4078885 -0.5756385 -0.1152698
#> 2  1.3939778 -1.4759325 -3.2507903
#> 3  0.3602783  0.1843827 -0.8569449
#> 4  0.6545503  1.0716736 -0.4608771
#> 5  1.0521554  0.7694740 -0.4332627
#> 6 -1.9795551 -1.4445834  0.1929347

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
