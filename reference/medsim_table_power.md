# Generate Power Table

Creates a publication-ready LaTeX table showing empirical power
(rejection rates) from simulation analysis.

## Usage

``` r
medsim_table_power(
  power,
  by_scenario = TRUE,
  caption = NULL,
  label = "tab:power"
)
```

## Arguments

- power:

  A medsim_power object from
  [`medsim_analyze_power()`](https://data-wise.github.io/medsim/reference/medsim_analyze_power.md)

- by_scenario:

  Logical: show power by scenario (default: TRUE)

- caption:

  Character: table caption

- label:

  Character: LaTeX label

## Value

A medsim_table object
