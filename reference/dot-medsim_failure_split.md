# Split a failure count into data_generator() vs method() failures (#43 G3)

A `data_generator()` failure row (`error` prefixed
`"data_generator error: "`) means the estimator never ran for that
replication – lumping it into the same `failure_rate` as a `method()`
failure (estimator ran, produced no usable CI) conflates two different
reliability signals. `failure_rate` and `dgp_failure_rate` deliberately
use DIFFERENT denominators and do not sum to an overall failure
fraction: `failure_rate` is conditional on the DGP having succeeded
(excludes DGP-failure reps from both numerator and denominator);
`dgp_failure_rate` is unconditional (always over `n_total`).

## Usage

``` r
.medsim_failure_split(error_vec, n_valid, n_total)
```

## Arguments

- error_vec:

  The (possibly `NULL`) `error` column for these rows, BEFORE any
  NA-based valid/invalid subsetting.

- n_valid:

  Count of rows with a usable (non-NA) CI/truth (post-filter).

- n_total:

  Total row count for this parameter/scenario, before filtering (same
  basis `error_vec` is indexed against).

## Value

`list(n_failed, failure_rate, n_dgp_failed, dgp_failure_rate)`.
