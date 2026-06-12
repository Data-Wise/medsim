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

[`medsim_scenario()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario.md)
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
#> 1 -0.3561244  1.6292737 -0.22274202
#> 2 -1.0644642 -1.1645871  0.31280267
#> 3  1.0771165 -0.6384365  0.09959926
#> 4  1.1815756  1.3719637  0.60954712
#> 5  0.1983921 -1.4365361 -1.63452694
#> 6 -0.4004052 -1.3049403 -0.43129914

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
