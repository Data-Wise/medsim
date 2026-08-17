# Combine chunk RDS files produced by medsim_run_chunk()

Reads all `chunk_<k>.rds` files from `output_dir`, merges their
`$results` and `$truth` data frames, and returns a single
`medsim_results` object that is equivalent to running all replications
in one call.

## Usage

``` r
medsim_combine_chunks(
  output_dir,
  pattern = "chunk_*.rds",
  expected_chunks = NULL,
  on_violation = c("stop", "warn", "ignore"),
  nsim = NULL,
  collapse_threshold = 0.9,
  collapse_digits = 12L,
  collapse_min_cell = 30L,
  collapse_exclude = .medsim_collapse_exclude_default,
  pilot_reference = NULL,
  pilot_tol = 1e-09,
  verbose = TRUE
)
```

## Arguments

- output_dir:

  Character: directory containing `chunk_*.rds` files.

- pattern:

  Character: glob pattern for chunk files. Default `"chunk_*.rds"`.

- expected_chunks:

  Integer or `NULL`: if supplied (e.g. the SLURM array size), a
  shortfall in chunk files is reported as an integrity violation (see
  `on_violation`) – catching a failed/timed-out task instead of silently
  combining a partial grid. Default `NULL` (no check).

- on_violation:

  One of `"stop"` (default), `"warn"`, `"ignore"`: what to do when an
  integrity violation is found (missing chunks, duplicate or gapped
  replication ids, ragged cells, output collapse, all-failed cells,
  cross-scenario seed collisions). `"stop"` signals a
  `medsim_combine_violation` condition that CARRIES the combined results
  – recover an hours-long run's good cells with
  `tryCatch(medsim_combine_chunks(...), medsim_combine_violation = function(e) e$results)`.
  For a deliberate partial combine (an interim look at a still-running
  array), pass `on_violation = "warn"`.

- nsim:

  Integer or `NULL`: if supplied, additionally assert that every
  scenario has exactly `nsim` replications (the contiguity audit is
  otherwise self-validating and needs no external count).

- collapse_threshold, collapse_digits, collapse_min_cell:

  Tuning for the collapse-signature audit: per scenario and per
  continuous estimate column, require
  `n_distinct(round(x, collapse_digits)) > collapse_threshold * n_ok`
  (defaults 0.9 and 12; calibration: the 0.3.1 seed-collapse produced
  ~17 distinct outcomes in 1000). Cells with fewer than
  `collapse_min_cell` non-NA values (default 30) are skipped as too
  small to diagnose.

- collapse_exclude:

  Character: DISCRETE method-contract fields excluded from the collapse
  audit BY NAME (default: `converged`, `branch_switch`, plus the
  [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  branch diagnostics `branch_mix`, `stacked_branch`, `p_branch_a`, `r4`,
  `r4_fixed` – 0/1 flags, an `m`-valued share, and ARIVs with point mass
  at 0, all legitimately low-cardinality). Name-based (not
  distinctness-based) so a totally collapsed estimate column cannot
  exempt itself; add any custom discrete field your method emits.

- pilot_reference:

  Character path (or `medsim_results` object): an archived pilot run to
  use as a positive control (Gate D). Because seeds depend only on
  `(scenario, replication)`, the full run's replications `1..B_pilot`
  are draw-identical to the pilot – a free regression check that
  harness, environment, and seeding are unchanged since the pilot
  passed. Identity is asserted FIRST (same sample size `n`, same
  scenario generator/params via fingerprint) so a stale or
  mis-configured pilot fails loud as `pilot_config_differs` instead of
  masquerading as a seeding regression; then estimate columns (only –
  never `elapsed` or other metadata) are compared within `pilot_tol`.
  Default `NULL` (no pilot check).

- pilot_tol:

  Numeric: absolute per-value tolerance for the pilot comparison
  (default `1e-9`). Byte-equality is deliberately NOT required: a
  different BLAS/R build can legitimately differ in low-order bits on
  correct code (FORK-reproducibility scope). For a rebuilt environment,
  loosen the tolerance or use `on_violation = "warn"`.

- verbose:

  Logical: print file counts.

## Value

A `medsim_results` object with combined `$results` and `$truth`.

## See also

[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md),
[`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)

## Examples

``` r
# After all SLURM chunks complete:
# combined <- medsim_combine_chunks("simulation_results/")
# coverage <- medsim_analyze_coverage(combined)
```
