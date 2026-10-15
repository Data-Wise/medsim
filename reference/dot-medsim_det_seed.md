# Deterministic Per-Replication Seed

Derives a seed from `(scenario_name, rep_id)` alone, independent of
chunk count, worker count, cluster type, or execution order. This is
what makes
[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
safe to split across any number of SLURM array tasks: replication `k` of
a given scenario draws the same data regardless of which chunk or worker
happens to process it.

## Usage

``` r
.medsim_det_seed(scenario_name, rep_id)
```

## Arguments

- scenario_name:

  Character: scenario name.

- rep_id:

  Integer: **global** replication id (i.e. already offset by
  `config$rep_offset` for chunked runs – see
  [`medsim_run_single_replication()`](https://data-wise.github.io/medsim/reference/medsim_run_single_replication.md)).

## Value

Integer in `[1, .Machine$integer.max]`.
