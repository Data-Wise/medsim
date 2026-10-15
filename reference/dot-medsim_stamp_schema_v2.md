# Stamp a combined results frame with the schema-v2 attributes

Shared between
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)'s
normal completion and its `medsim_run_failure` partial payload (#43) –
both need the SAME schema-v2 stamp (`replication` is the GLOBAL rep
id) + `medsim_meta_cols` provenance, so combine-time auditing treats a
partial run exactly like a complete one.

## Usage

``` r
.medsim_stamp_schema_v2(df)
```

## Arguments

- df:

  A combined results data.frame, or `NULL`.

## Value

`df` with the schema-v2 attributes set, or `NULL` unchanged.
