# Generate Timing Comparison Table

Creates a publication-ready LaTeX table comparing computational time
across methods.

## Usage

``` r
medsim_table_timing(
  results_list,
  metric = "mean",
  include_speedup = TRUE,
  reference_method = NULL,
  by_scenario = FALSE,
  caption = NULL,
  label = "tab:timing"
)
```

## Arguments

- results_list:

  Named list of medsim_results objects, one per method

- metric:

  Character: timing metric to use ("mean", "median", "total")

- include_speedup:

  Logical: include speedup column relative to slowest method

- reference_method:

  Character: method name to use as reference for speedup (default: NULL
  uses slowest method)

- by_scenario:

  Logical: show timing by scenario (default: FALSE)

- caption:

  Character: table caption

- label:

  Character: LaTeX label

## Value

A medsim_table object

## Examples

``` r
if (FALSE) { # \dontrun{
table <- medsim_table_timing(list(
  Proposed = results1,
  Delta = results2,
  Bootstrap = results3
))
} # }
```
