# Get Optimal Number of Cores

Determines the optimal number of cores to use for parallel processing
based on system capabilities and task requirements.

## Usage

``` r
medsim_get_optimal_cores(n_tasks, reserve = 2, max_cores = NULL)
```

## Arguments

- n_tasks:

  Integer: Number of tasks to process. If fewer tasks than cores,
  returns `n_tasks`.

- reserve:

  Integer: Number of cores to reserve for system (default 2). Only
  applied on local machines, not clusters.

- max_cores:

  Integer: Maximum number of cores to use. If NULL, uses all available
  (minus reserve).

## Value

Integer: Optimal number of cores to use

## Examples

``` r
# For 100 tasks
medsim_get_optimal_cores(n_tasks = 100)
#> [1] 2

# For few tasks (returns n_tasks)
medsim_get_optimal_cores(n_tasks = 3)
#> [1] 2

# With maximum limit
medsim_get_optimal_cores(n_tasks = 1000, max_cores = 8)
#> [1] 2
```
