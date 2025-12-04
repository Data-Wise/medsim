# Generate All Tables

Convenience function to generate all tables from simulation results and
save them to an output directory.

## Usage

``` r
medsim_tables_workflow(
  results,
  output_dir,
  tables = "all",
  format = "latex",
  prefix = "table_"
)
```

## Arguments

- results:

  A medsim_results object from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)

- output_dir:

  Character: output directory for tables

- tables:

  Character vector: which tables to generate ("accuracy", "coverage",
  "power", "all")

- format:

  Character: output format ("latex", "csv", "markdown")

- prefix:

  Character: prefix for file names (default: "table\_")

## Value

Invisibly returns a list of generated file paths

## Examples

``` r
if (FALSE) { # \dontrun{
results <- medsim_run(method, scenarios, config)

# Generate all tables
files <- medsim_tables_workflow(
  results,
  output_dir = "manuscript/tables",
  format = "latex"
)
} # }
```
