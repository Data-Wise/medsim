# Pilot-subset positive control (Gate D)

Asserts a full run's replications `1..B_pilot` reproduce an archived
pilot: (1) IDENTITY – same sample size `n` and same scenario
fingerprints, so a stale/mis-configured pilot fails as
`pilot_config_differs` rather than masquerading as a seeding regression
(the seed ignores `n`: same rep id at a different `n` draws
different-length data on CORRECT code); then (2) VALUES – estimate
columns only (never `elapsed`/metadata), joined on
`(scenario, replication)`, within `pilot_tol`.

## Usage

``` r
.medsim_pilot_check(combined, pilot_reference, pilot_tol = 1e-09)
```

## Arguments

- combined:

  The combined `medsim_results` object.

- pilot_reference:

  Path to a pilot RDS, or a `medsim_results` object.

- pilot_tol:

  Absolute tolerance for value agreement.

## Value

List of violations (empty if the control passes).
