# Content fingerprint of a scenario (name + params + generator source)

Like
[`.medsim_truth_fingerprint()`](https://data-wise.github.io/medsim/reference/dot-medsim_truth_fingerprint.md)
but without a truth function – the identity Gate D asserts between a
pilot run's scenarios and the full run's. Deparse (not closure
serialization) for environment-independence; values a generator depends
on live in `params` by medsim convention.

## Usage

``` r
.medsim_scenario_fingerprint(scenario)
```

## Arguments

- scenario:

  A `medsim_scenario`.

## Value

A raw vector (compare with
[`identical()`](https://rdrr.io/r/base/identical.html)).
