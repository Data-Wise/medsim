# Compare Multiple Configurations

Creates a comparison table showing differences between test, local, and
cluster configurations.

## Usage

``` r
medsim_compare_configs()
```

## Value

A data.frame with configuration comparison

## Examples

``` r
comparison <- medsim_compare_configs()
print(comparison)
#>          Parameter                    Test              Local
#> 1     Replications                      20                100
#> 2            Cores                       4                  2
#> 3        Scenarios                    test                all
#> 4 Output Directory simulation_results_test simulation_results
#>              Cluster
#> 1               1000
#> 2                  2
#> 3                all
#> 4 simulation_results
```
