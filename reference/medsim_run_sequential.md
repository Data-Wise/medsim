# Run Tasks Sequentially

Runs tasks sequentially (no parallelization). Used as fallback when
parallel processing is not needed or not available.

## Usage

``` r
medsim_run_sequential(tasks, fun, progress = TRUE)
```

## Arguments

- tasks:

  Vector or list of tasks

- fun:

  Function to apply to each task

- progress:

  Show progress bar

## Value

List of results
