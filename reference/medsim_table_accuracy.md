# Generate Accuracy Table

Creates a publication-ready LaTeX table showing accuracy metrics (bias,
MAE, RMSE) from simulation analysis results.

## Usage

``` r
medsim_table_accuracy(
  analysis,
  digits = 3,
  metrics = c("bias", "mae", "rmse"),
  by_scenario = TRUE,
  caption = NULL,
  label = "tab:accuracy"
)
```

## Arguments

- analysis:

  A medsim_analysis object from
  [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)

- digits:

  Integer: number of decimal places (default: 3)

- metrics:

  Character vector: which metrics to include. Options: "bias", "mae",
  "rmse", "median_ae", "max_ae", "relative_bias" (default: c("bias",
  "mae", "rmse"))

- by_scenario:

  Logical: include scenario breakdown (default: TRUE)

- caption:

  Character: table caption (default: auto-generated)

- label:

  Character: LaTeX label for cross-referencing (default: "tab:accuracy")

## Value

A medsim_table object (character vector with LaTeX code)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- medsim_run(method, scenarios, config)
analysis <- medsim_analyze(results)

# Generate table
table <- medsim_table_accuracy(analysis)
print(table)  # Preview

# Write to file
medsim_write_table(table, "tables/accuracy.tex")
} # }
```
