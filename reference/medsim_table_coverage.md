# Generate Coverage Table

Creates a publication-ready LaTeX table showing confidence interval
coverage rates.

## Usage

``` r
medsim_table_coverage(
  coverage,
  expected = 0.95,
  by_scenario = TRUE,
  caption = NULL,
  label = "tab:coverage"
)
```

## Arguments

- coverage:

  A medsim_coverage object from
  [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)

- expected:

  Numeric: expected/nominal coverage rate (default: 0.95)

- by_scenario:

  Logical: show coverage by scenario (default: TRUE)

- caption:

  Character: table caption

- label:

  Character: LaTeX label

## Value

A medsim_table object
