# Seed-provenance audit over a combined results frame (Gate A)

Seed-provenance audit over a combined results frame (Gate A)

## Usage

``` r
.medsim_audit_seed_provenance(
  df,
  nsim = NULL,
  collapse_threshold = 0.9,
  collapse_digits = 12L,
  collapse_min_cell = 30L,
  collapse_exclude = .medsim_collapse_exclude_default
)
```

## Arguments

- df:

  Combined results data.frame.

- nsim:

  Optional integer: pin the absolute per-scenario rep count.

- collapse_threshold, collapse_digits, collapse_min_cell:

  See
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md).

## Value

List of violations, each `list(type =, scenario =, message =)`.
