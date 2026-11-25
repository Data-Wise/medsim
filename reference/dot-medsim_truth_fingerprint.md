# Content fingerprint of a scenario's truth inputs

Serialized identity of everything that determines a scenario's ground
truth: name, params (by value), and the *deparsed source* of the
data-generator and the truth function. Used by
[`medsim_compute_all_truth()`](https://data-wise.github.io/medsim/reference/medsim_compute_all_truth.md)
to invalidate an index-keyed truth cache on any content change (guards
against stale-truth reuse when an `output_dir` is reused after a
scenario changes).

Deparse (source text) is used rather than serializing the live closures
on purpose: a closure's
[`serialize()`](https://rdrr.io/r/base/serialize.html) includes its
whole enclosing environment, which mutates as the caller adds unrelated
bindings – that would make the fingerprint unstable across two identical
calls and defeat legitimate cache reuse. Deparse is
environment-independent, and the *values* a data-generator depends on
live in `params` (medsim convention), which is fingerprinted by value.
Residual limitation: a generator that closes over a value NOT in
`params` (e.g. `local({ m <- 5; function(n) rnorm(n, m) })`) with an
identical body and identical `params` would not be distinguished by a
change to `m`; put such parameters in `params` for them to be tracked.

## Usage

``` r
.medsim_truth_fingerprint(scenario, truth_function)
```

## Arguments

- scenario:

  A `medsim_scenario`.

- truth_function:

  The truth function passed to
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md).

## Value

A raw vector (compared with
[`identical()`](https://rdrr.io/r/base/identical.html)).
