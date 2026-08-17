# Signal collected integrity violations per the on_violation control

Signal collected integrity violations per the on_violation control

## Usage

``` r
.medsim_signal_violations(
  violations,
  results,
  on_violation,
  context = "medsim_combine_chunks"
)
```

## Arguments

- violations:

  List of violations from
  [`.medsim_audit_seed_provenance()`](https://data-wise.github.io/medsim/reference/dot-medsim_audit_seed_provenance.md).

- results:

  The combined object to attach to a stop condition.

- on_violation:

  `"stop"`, `"warn"`, or `"ignore"`.

- context:

  Character: calling-function name for messages.
