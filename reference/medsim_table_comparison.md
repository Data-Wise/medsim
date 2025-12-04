# Generate Method Comparison Table

Creates a publication-ready LaTeX table comparing multiple methods
across accuracy, timing, and coverage metrics.

## Usage

``` r
medsim_table_comparison(
  comparison,
  metrics = c("accuracy", "timing"),
  caption = NULL,
  label = "tab:comparison"
)
```

## Arguments

- comparison:

  A medsim_comparison object from
  [`medsim_compare_methods()`](https://data-wise.github.io/medsim/reference/medsim_compare_methods.md)

- metrics:

  Character vector: which metrics to include ("accuracy", "timing",
  "coverage")

- caption:

  Character: table caption

- label:

  Character: LaTeX label

## Value

A medsim_table object
