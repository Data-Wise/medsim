# Audit a combined results object for chunked-run integrity violations

Standalone entry point for Gate A of the chunked-run integrity layer
(SPEC-medsim-chunked-run-gates-2026-07-31): run the same audit
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
performs, on an already-combined `medsim_results` object. Checks, per
scenario:

1.  **Contiguity** – `replication` ids form a gapless, duplicate-free
    `1..max` run, with the same `max` in every scenario
    (self-validating: needs no external replication count).

2.  **Collapse signature** – every continuous estimate column has more
    than `collapse_threshold * n_ok` distinct values (the 0.3.1
    seed-collapse produced ~17 distinct outcomes in 1000 while every
    chunk exited 0). An all-failed cell (`n_ok == 0`) is reported as
    `cell_failed`, not collapse.

3.  **Seed-space collisions** – no two scenarios map onto overlapping
    [`.medsim_det_seed()`](https://data-wise.github.io/medsim/reference/dot-medsim_det_seed.md)
    sequences (the ~1e6-bucket name hash can collide).

Frames without the schema-v2 stamp (produced before medsim 0.5.0, when
`replication` was chunk-LOCAL and collided across chunks) skip checks 1
and 3 with a warning – they cannot be audited for id integrity.

## Usage

``` r
medsim_audit_results(
  results,
  on_violation = c("stop", "warn", "ignore"),
  nsim = NULL,
  collapse_threshold = 0.9,
  collapse_digits = 12L,
  collapse_min_cell = 30L,
  collapse_exclude = .medsim_collapse_exclude_default
)
```

## Arguments

- results:

  A `medsim_results` object (e.g. from
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)),
  or a bare results data.frame.

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

## Value

Invisibly, the list of violations found (empty if clean). Signalling
follows `on_violation`, exactly as in
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md).

## See also

[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md),
[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
