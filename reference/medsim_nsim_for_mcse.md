# Replications needed for a target coverage Monte Carlo SE

Inverts `MCSE = sqrt(p(1-p)/nsim)` to give the `nsim` such that a
coverage cell near `p` has Monte Carlo SE no larger than `target_mcse`.

## Usage

``` r
medsim_nsim_for_mcse(target_mcse, p = 0.95)
```

## Arguments

- target_mcse:

  Positive numeric target Monte Carlo SE.

- p:

  Assumed coverage proportion (default 0.95, the nominal level).

## Value

Integer number of replications (ceiling).

## Examples

``` r
medsim_nsim_for_mcse(0.005, 0.95)  # 1900
#> [1] 1900
```
