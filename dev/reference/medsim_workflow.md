# Generate analysis, figures, and tables from simulation results

End-to-end output pipeline: runs
[`medsim_analyze()`](https://data-wise.github.io/medsim/dev/reference/medsim_analyze.md),
[`medsim_figures()`](https://data-wise.github.io/medsim/dev/reference/medsim_figures.md),
and
[`medsim_tables()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables.md)
and writes everything to `output_dir`.

## Usage

``` r
medsim_workflow(
  results,
  output_dir = "results",
  figures_format = "pdf",
  tables_format = "latex",
  ...
)
```

## Arguments

- results:

  A `medsim_results` object from
  [`medsim_run()`](https://data-wise.github.io/medsim/dev/reference/medsim_run.md).

- output_dir:

  Top-level directory for all outputs. Subdirectories `figures/` and
  `tables/` are created underneath. Created if it doesn't exist.

- figures_format:

  File format for figures (e.g. `"pdf"`, `"png"`).

- tables_format:

  Output format for tables (e.g. `"latex"`, `"markdown"`).

- ...:

  Additional arguments passed to
  [`medsim_figures()`](https://data-wise.github.io/medsim/dev/reference/medsim_figures.md).

## Value

Invisibly, a list with elements `analysis`, `figures` (file paths), and
`tables` (return value of
[`medsim_tables_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables_workflow.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
config <- medsim_config("test")
scenarios <- medsim_scenarios_mediation()
results <- medsim_run(my_method, scenarios, config)
medsim_workflow(results, output_dir = "results")
} # }
```
