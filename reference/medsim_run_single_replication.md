# Run Single Simulation Replication

Internal function to run a single replication of a simulation. This is
called repeatedly by
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md).

## Usage

``` r
medsim_run_single_replication(scenario, rep_id, method, config)
```

## Arguments

- scenario:

  Scenario object

- rep_id:

  Replication ID number (local to the current chunk, if any – offset by
  `config$rep_offset` internally to get the global id, which is both
  used for seeding and recorded as the row's `replication` value (schema
  v2: `replication` is the global rep id, never chunk-local))

- method:

  User-defined method function

- config:

  Configuration object

## Value

data.frame with results for this replication. A `data_generator()`
failure returns a failure row (`error = "data_generator error: <msg>"`,
`elapsed = NA_real_`, no `converged`) without ever calling `method()`. A
`method()` failure returns a failure row with the plain error message
(no prefix). Anything else escaping this function (a non-function
`data_generator`, an error inside
[`.medsim_det_seed()`](https://data-wise.github.io/medsim/reference/dot-medsim_det_seed.md)/[`set.seed()`](https://rdrr.io/r/base/Random.html),
or one during result-row assembly) is a contract/infrastructure error
and propagates uncaught –
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)'s
per-task sweep is what turns that into a `medsim_run_failure` condition,
not this function.
