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

data.frame with results for this replication
