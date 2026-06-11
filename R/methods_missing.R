# ── INTERFACE FREEZE (Step 0) ─────────────────────────────────────────────────
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md
# These are SIGNATURE STUBS only — bodies are filled by their workstreams (WS-*).
# Do not change a signature without updating the spec's "Interface freeze" section.
#
# SHARED method() RETURN CONTRACT (consumed by medsim_analyze_coverage/_power):
#   every estimator adapter returns a flat named list of numeric scalars:
#     list(
#       indirect          = <num>,   # point estimate of the indirect effect (a*b / TNIE)
#       indirect_ci_lower = <num>,   # -> medsim_analyze_coverage()
#       indirect_ci_upper = <num>,
#       indirect_p        = <num>,   # -> medsim_analyze_power()
#       branch_switch     = <0/1>,   # MBCO union-null diagnostic (ab=0); NA for MC-CI/IPW
#       converged         = <0/1>    # drop non-converged rows in analyze
#     )
#
# VERIFIED 2026-06-11 (R/runner.R:329): method is invoked as `method(data, scenario$params)`,
#   i.e. the contract is `function(data, params)` where `params` IS the scenario's params list.
# ──────────────────────────────────────────────────────────────────────────────

#' MBCO-MI estimator adapter (WS-D)
#'
#' Multiple imputation (via `missingmed`) + MBCO likelihood-ratio inference (via
#' `rmediation::mbco()`). During prototyping this wraps
#' `Missing Effect/code/prototype-d4-mbco.R`.
#'
#' @param model lavaan/OpenMx mediation model syntax or spec.
#' @param m Integer number of imputations.
#' @param ... Passed to the underlying MI / MBCO routines.
#' @return A `function(data, params)` returning the shared method contract (see file header).
#' @export
medsim_method_mbco_mi <- function(model, m = 20L, ...) {
  stop("WS-D not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}

#' Monte-Carlo CI estimator adapter (WS-D)
#'
#' Multiple imputation + Monte-Carlo confidence interval (via `rmediation::medci()`).
#'
#' @inheritParams medsim_method_mbco_mi
#' @return A `function(data, params)` returning the shared method contract (`branch_switch = NA`).
#' @export
medsim_method_mc_ci <- function(model, m = 20L, ...) {
  stop("WS-D not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}

#' Thin IPW estimator adapter (WS-D, robustness appendix)
#'
#' Inverse-probability-weighting comparator. Positioned as a robustness appendix, not a
#' co-equal main-results arm (decided 2026-06-11).
#'
#' @param model Mediation model spec.
#' @param ... Passed to the underlying weighting / fitting routines.
#' @return A `function(data, params)` returning the shared method contract (`branch_switch = NA`).
#' @export
medsim_method_ipw <- function(model, ...) {
  stop("WS-D not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}
