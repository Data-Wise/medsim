# Run one chunk of a chunked SLURM array simulation

Runs replications `[(chunk_id-1)*chunk_size+1 .. chunk_id*chunk_size]`
and saves a single RDS file named `chunk_<chunk_id>.rds` in
`output_dir`. Designed to be called from inside a SLURM array task
script; `chunk_id` and `n_chunks` are auto-detected from `config` (which
auto-reads `SLURM_ARRAY_TASK_ID`).

The chunk `.rds` is the sole artifact: in chunk mode the intermediate
per-scenario/summary CSVs that a standalone
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
writes are skipped (concurrent array tasks sharing an `output_dir` would
clobber the fixed-name files, leaving partial data that looks complete).
Result rows record the **global** replication id (schema v2): chunk 2 of
a 4-chunk/nsim-20 run writes `replication` 6..10, not 1..5, so ids never
collide across chunks and
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
can audit the combined grid.

Each chunk file also carries a provenance attribute (Gate C): R version,
medsim + key dependency versions, hostname, a code SHA, and
seconds-per-replication timing. The SHA is auto-detected from the run
script's git tree (falling back to a `pkg:medsim-<version>` tag outside
git); pass `code_sha` to stamp explicitly.
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
asserts a single SHA across all chunks – catching a mid-run code edit
plus partial resubmit, which would silently mix results from two
different code states.

## Usage

``` r
medsim_run_chunk(scenarios, method, config, verbose = TRUE, code_sha = NULL)
```

## Arguments

- scenarios:

  A list of
  [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
  objects.

- method:

  A function with signature `method(data, params)`.

- config:

  A `medsim_config` object with `chunk_id` and `n_chunks` set.

- verbose:

  Logical: print progress messages.

- code_sha:

  Character or `NULL`: code-state identifier stamped into the chunk's
  provenance. `NULL` (default) auto-detects via `git rev-parse HEAD` in
  the running script's directory, degrading to `pkg:medsim-<version>`
  outside a git tree.

## Value

Invisibly, the path of the RDS file written.

## See also

[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md),
[`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md),
[`medsim_write_submit_script()`](https://data-wise.github.io/medsim/reference/medsim_write_submit_script.md)
