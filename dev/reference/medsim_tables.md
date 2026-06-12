# Generate all standard LaTeX tables from simulation results

Convenience wrapper around
[`medsim_tables_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables_workflow.md)
with sensible defaults.

## Usage

``` r
medsim_tables(results, output_dir = "tables", format = "latex", ...)
```

## Arguments

- results:

  A `medsim_results` object from
  [`medsim_run()`](https://data-wise.github.io/medsim/dev/reference/medsim_run.md).

- output_dir:

  Directory to write tables to. Created if it doesn't exist.

- format:

  Output format passed to
  [`medsim_tables_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables_workflow.md)
  (e.g. `"latex"`, `"markdown"`).

- ...:

  Additional arguments passed to
  [`medsim_tables_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables_workflow.md).

## Value

Invisibly, the result of
[`medsim_tables_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables_workflow.md).
