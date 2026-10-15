# MBCO confidence interval for P_med (two-branch method)

Estimates P_med from observed data using OLS point estimation and a
**two-branch MBCO** (maximally balanced constrained optimisation)
confidence interval. Returns the 6-field contract expected by
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md):

|                 |                                                |
|-----------------|------------------------------------------------|
| Field           | Description                                    |
| `pmed`          | Point estimate of P_med                        |
| `pmed_ci_lower` | Lower bound of MBCO CI                         |
| `pmed_ci_upper` | Upper bound of MBCO CI                         |
| `pmed_p`        | Two-sided p-value (H0: P_med = 0.5)            |
| `branch_switch` | 1 if the MBCO union-null LRT switched branches |
| `converged`     | 1 if optimisation converged                    |

**Algorithm:**

1.  Fit linear SEM: `M ~ A` and `Y ~ A + M`.

2.  Estimate path coefficients alpha (A-\>M) and beta (M-\>Y).

3.  Estimate P_med via a parametric bootstrap PO draw under the
    estimated SEM.

4.  Build a delta-method normal CI and clamp to \[0, 1\].

5.  The "branch_switch" flag records whether the union-null test
    selected the constrained branch (alpha*beta \<= 0) over the
    unconstrained branch (alpha*beta \> 0).

## Usage

``` r
medsim_method_pmed_mbco(data, params, alpha = 0.05, n_boot = 2000L)
```

## Arguments

- data:

  A `data.frame` with columns `A`, `M`, `Y`.

- params:

  Named list from
  [`medsim_scenario_pmed()`](https://data-wise.github.io/medsim/reference/medsim_scenario_pmed.md);
  must contain at minimum `alpha_ax`, `beta_my`, `beta_ay`, `sigma_m`,
  `sigma_y`.

- alpha:

  Significance level for CI (default 0.05).

- n_boot:

  Integer: parametric bootstrap size for P_med point estimate (default
  2000).

## Value

A named list with fields `pmed`, `pmed_ci_lower`, `pmed_ci_upper`,
`pmed_p`, `branch_switch`, `converged`.

## See also

[`medsim_scenario_pmed()`](https://data-wise.github.io/medsim/reference/medsim_scenario_pmed.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
