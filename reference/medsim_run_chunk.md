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
medsim_run_chunk(
  scenarios,
  method,
  config,
  verbose = TRUE,
  code_sha = NULL,
  compute_truth = NULL,
  truth_on_failure = c("stop", "warn")
)
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

- compute_truth:

  Optional function with signature `(data, params)`, passed to
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md).
  Without it the combined result carries no `$truth`, so it supports
  power analysis but not coverage or bias. Only **chunk 1** computes
  truth; every other chunk ignores `compute_truth`, so truth is computed
  once, not once per chunk. Set
  `config$compute_truth_every_chunk <- TRUE` to compute it in every
  chunk (useful only when truth is cheap and chunk 1 may not survive).
  Truth is seeded from `config$seed`, so chunks that compute it agree,
  and
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  keeps one row per scenario from whichever chunks carry it. The chunk's
  provenance records `truth_requested`, so a combine that should have
  truth but does not raises a `truth_coverage` violation.

  Chunk 1 then runs longer than the others (truth plus its
  replications); size `--time` for it, not from a pilot of another
  chunk. To keep chunk 1 balanced, compute truth once before submitting:
  a short
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  over the same scenarios, `compute_truth` and `output_dir` warms the
  truth cache, and chunk 1 then loads the truth instead of computing it.
  Keep passing `compute_truth` to the chunks: it is what records
  `truth_requested`, so a lost truth is still reported at combine.

- truth_on_failure:

  `"stop"` (default) or `"warn"`, passed to
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md).
  Under `"stop"` a failed truth stops chunk 1 before any replication
  runs, so no chunk file is written and the array task exits nonzero.

## Value

Invisibly, the path of the RDS file written. If a task-level error (a
`medsim_run_failure` from
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md))
interrupts the chunk, whatever replications completed are still written
– atomically, with provenance `partial = TRUE` – before this function
re-[`stop()`](https://rdrr.io/r/base/stop.html)s so the array task still
exits nonzero (`sacct` visibility, not a `--requeue` signal). **This
holds even when NO replication completed**: a chunk whose every
replication failed writes a flagged, zero-row file rather than no file
at all. That file is the only carrier of the `partial = TRUE` marker,
and without it a trailing all-failed chunk would combine with zero
violations (the contiguity audit cannot see it – the surviving ids are a
clean `1..k` block).
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)/[`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md)
flag any such chunk via the `partial_chunk` violation.

## See also

[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md),
[`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md),
[`medsim_write_submit_script()`](https://data-wise.github.io/medsim/reference/medsim_write_submit_script.md)
