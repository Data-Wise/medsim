# Draw nonnormal values with a target marginal skew/kurtosis

Fleishman / Vale–Maurelli power-method draws. Pure base R; no hard
dependency. Warns/errors on (skew, kurtosis) outside the
Fleishman-feasible region.

## Usage

``` r
medsim_rnonnormal(n, mean = 0, sd = 1, skew = 0, kurtosis = 0)
```

## Arguments

- n:

  Integer sample size.

- mean, sd:

  Target marginal mean and SD (applied after standardizing).

- skew:

  Target marginal skewness (third standardized moment).

- kurtosis:

  Target marginal excess kurtosis (fourth standardized moment minus 3).

## Value

Numeric vector of length `n` with the requested marginal moments.
