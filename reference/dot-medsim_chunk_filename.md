# Chunk-file naming authority

The single source of the `chunk_%04d.rds` convention, shared by the
writer
([`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md))
and matched by
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)'s
default `pattern = "chunk_*.rds"` – change the convention here (and that
default) or the writer and the combiner silently stop finding each
other's files.

## Usage

``` r
.medsim_chunk_filename(chunk_id)
```

## Arguments

- chunk_id:

  Integer chunk id.

## Value

Character filename, e.g. `"chunk_0007.rds"`.
