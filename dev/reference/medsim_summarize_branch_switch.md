# Summarize the MBCO branch-switch rate per scenario

Reads the `branch_switch` column emitted by
[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mbco_mi.md)
(the union-null / ab=0 diagnostic from MEMO-MBCO-MI-derivation) and
returns a per-scenario summary suitable for joining as a column onto a
[`medsim_table_coverage()`](https://data-wise.github.io/medsim/dev/reference/medsim_table_coverage.md)
table. Non-converged rows (`converged == 0`) are dropped before
summarizing.

## Usage

``` r
medsim_summarize_branch_switch(results, by = "scenario")
```

## Arguments

- results:

  A `medsim_results` object from
  [`medsim_run()`](https://data-wise.github.io/medsim/dev/reference/medsim_run.md).

- by:

  Grouping columns (default `"scenario"`).

## Value

A data.frame: one row per group with `branch_switch_rate` and `n_valid`.
