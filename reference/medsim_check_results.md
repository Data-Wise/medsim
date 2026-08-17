# Check Results for Errors

Checks a list of results from parallel execution for errors and provides
a summary.

## Usage

``` r
medsim_check_results(results, stop_on_error = FALSE)
```

## Arguments

- results:

  List of results from
  [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)

- stop_on_error:

  Logical: If TRUE, stops with error if any task failed

## Value

Invisibly returns indices of failed tasks

## Examples

``` r
if (FALSE) { # \dontrun{
results <- medsim_run_parallel(...)
medsim_check_results(results)
} # }
```
