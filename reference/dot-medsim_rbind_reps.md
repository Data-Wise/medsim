# Schema-harmonized rbind of replication rows

Failed replications produce rows whose columns differ from success rows
(a failure row carries `error` but no estimate columns), and a bare
`do.call(rbind, ...)` crashes on the mismatch – one transient rep
failure used to kill the entire scenario (chunked: the whole chunk
file).

This helper unions the column sets, fills absent columns with `NA`,
guarantees an `error` character column on EVERY row (`NA` on success),
and sets `converged = 0` on failure rows when a `converged` column
exists – so all rows share one schema and downstream code sees the
documented failure semantics instead of a crash.

## Usage

``` r
.medsim_rbind_reps(rows)
```

## Arguments

- rows:

  List of single-row (or multi-row) result data.frames.

## Value

One combined data.frame (0-row frames are dropped).
