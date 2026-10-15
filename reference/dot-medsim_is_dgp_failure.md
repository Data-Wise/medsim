# Test whether failure-row error messages are data_generator() failures

[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)/[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
tag a `data_generator()` failure differently from a `method()` failure
(message prefix `"data_generator error: "`) so combine-time diagnostics
can distinguish "the estimator failed" from "the estimator never ran."
Contract: `NULL` in (a legacy or user-built frame with no `error`
column) returns `logical(0)`, matching the empty-input behavior callers
already rely on for `error`.

## Usage

``` r
.medsim_is_dgp_failure(error)
```

## Arguments

- error:

  Character vector (typically `df$error`), or `NULL`.

## Value

Logical vector the same length as `error`.
