# Differential-misclassification (DM) bounds estimator adapter
# Spec: PROPOSAL-medsim-dm-integration.md + Phase 1 of broad overhaul spec
#
# NOTE: Live medrobust::bound_ne() integration is DEFERRED pending medrobust-CRAN
# and a stable slot/accessor API (the current S7 object exposes no named slots).
# This file delivers the interval-kind result CONTRACT so the analysis machinery
# (medsim_analyze_coverage interval branch, coverage table, pkgdown docs) can be
# tested and shipped independently.  The medrobust wiring is a one-line swap once
# the package stabilises -- replace the synthetic compute block with bound_ne().

#' Bounds estimator adapter for differential-misclassification (DM) studies
#'
#' @description
#' Returns partial-identification bounds (NDE/NIE) following the `interval`-kind
#' result contract expected by [medsim_run()], so that
#' [medsim_analyze_coverage()] dispatches the Imbens-Manski / partial-ID
#' interval coverage branch.
#'
#' **Current implementation:** OLS decomposition + inflated interval (synthetic
#' bounds for unit-testing the harness).  A future version will call
#' `medrobust::bound_ne()` once its accessor API stabilises on CRAN.
#'
#' Result columns returned (flat named list):
#' \itemize{
#'   \item `NDE_lower`, `NDE_upper` -- bounds on natural direct effect
#'   \item `NDE_im_lower`, `NDE_im_upper` -- Imbens-Manski CI for NDE
#'   \item `NIE_lower`, `NIE_upper` -- bounds on natural indirect effect
#'   \item `NIE_im_lower`, `NIE_im_upper` -- Imbens-Manski CI for NIE
#'   \item `feasible` -- logical: bounds are non-empty / consistent
#'   \item `falsified` -- logical: data rejects identifying assumptions
#' }
#'
#' @param data A data.frame produced by the scenario's `data_generator`.
#'   Expected columns: `A` (binary treatment), `M` or `M_star` (mediator,
#'   possibly misclassified), `Y` (outcome).
#' @param params Named list of scenario parameters.  Must include `NDE` and
#'   `NIE` (ground-truth natural effects) so truth can be tracked.
#' @param misclass_type `"mediator"` (default) or `"exposure"`.
#' @param alpha Nominal level; `1 - alpha` CI (default `0.05`).
#'
#' @return Named list with elements `NDE_lower`, `NDE_upper`,
#'   `NDE_im_lower`, `NDE_im_upper`, `NIE_lower`, `NIE_upper`,
#'   `NIE_im_lower`, `NIE_im_upper`, `feasible`, `falsified`.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(A = rbinom(200, 1, 0.5), M = rnorm(200), Y = rnorm(200))
#' result <- medsim_method_bounds(d, list(NDE = 0.2, NIE = 0.3))
#' str(result)
#'
#' @seealso [medsim_scenario_dm()] for the companion scenario factory;
#'   [medsim_analyze_coverage()] for interval-kind coverage analysis.
#'
#' @export
medsim_method_bounds <- function(data, params,
                                 misclass_type = c("mediator", "exposure"),
                                 alpha = 0.05) {
  misclass_type <- match.arg(misclass_type)

  n <- nrow(data)

  # Resolve treatment, mediator columns
  trt <- if ("A" %in% names(data)) {
    data[["A"]]
  } else if ("A_star" %in% names(data)) {
    data[["A_star"]]
  } else {
    stop("data must contain column 'A' or 'A_star'")
  }

  med_col <- if ("M_star" %in% names(data)) "M_star" else "M"
  if (!med_col %in% names(data)) stop("data must contain column 'M' or 'M_star'")
  m_col <- data[[med_col]]

  if (!"Y" %in% names(data)) stop("data must contain column 'Y'")
  y_col <- data[["Y"]]

  # OLS decomposition (point estimates) via the shared a/b-path fitter
  # (.medsim_md_fit_ab, R/methods_missing.R) -- named-coefficient lookup.
  d_fit  <- data.frame(A = trt, M = m_col, Y = y_col)
  fit    <- .medsim_md_fit_ab(d_fit, treatment = "A",
                              mediator = "M", outcome = "Y")
  a_m    <- fit$a         # effect of A on M
  b_my   <- fit$b         # effect of M on Y | A
  b_ay   <- fit$cprime    # direct effect of A on Y | M

  nde_pt <- b_ay
  nie_pt <- a_m * b_my

  # Inflate interval width to simulate partial-ID uncertainty from
  # misclassification sensitivity analysis (deferred: this becomes bound_ne()).
  total  <- abs(nde_pt) + abs(nie_pt) + 0.1
  delta  <- 0.15 * total
  z_alpha <- qnorm(1 - alpha / 2)
  # SE-based widening for IM CI (outer envelope of the bounds)
  se_nde <- fit$se_cprime
  se_nie <- abs(b_my) * fit$se_a

  list(
    NDE_lower    = nde_pt - delta,
    NDE_upper    = nde_pt + delta,
    NDE_im_lower = nde_pt - delta - z_alpha * se_nde,
    NDE_im_upper = nde_pt + delta + z_alpha * se_nde,
    NIE_lower    = nie_pt - delta,
    NIE_upper    = nie_pt + delta,
    NIE_im_lower = nie_pt - delta - z_alpha * se_nie,
    NIE_im_upper = nie_pt + delta + z_alpha * se_nie,
    feasible     = TRUE,
    falsified    = FALSE
  )
}
