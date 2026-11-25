# Estimate Parallel Speedup

Estimates the expected speedup from parallel processing based on number
of cores and task characteristics.

## Usage

``` r
medsim_estimate_speedup(n_tasks, n_cores, overhead = 0.1)
```

## Arguments

- n_tasks:

  Integer: Number of tasks

- n_cores:

  Integer: Number of cores

- overhead:

  Numeric: Overhead per core (default 0.1 = 10%)

## Value

Named list with:

- `speedup`: Expected speedup factor

- `efficiency`: Parallel efficiency (0-1)

- `recommendation`: Character recommendation

## Details

Speedup is estimated using Amdahl's law with overhead:

Speedup = n_cores / (1 + overhead \* n_cores)

Efficiency = Speedup / n_cores

## Examples

``` r
# For 100 tasks on 10 cores
est <- medsim_estimate_speedup(100, 10)
cat(sprintf("Expected speedup: %.1fx\n", est$speedup))
#> Expected speedup: 5.0x
cat(sprintf("Efficiency: %.0f%%\n", est$efficiency * 100))
#> Efficiency: 50%
```
