# INTERFACE FREEZE (Step 0) — signature stub for WS-A.
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md  (§"1. Nonnormality generator")

#' Draw nonnormal values with a target marginal skew/kurtosis (WS-A)
#'
#' Fleishman / Vale–Maurelli power-method draws. Pure base R; no hard dependency.
#' Warns/errors on (skew, kurtosis) outside the Fleishman-feasible region.
#'
#' @param n Integer sample size.
#' @param mean,sd Target marginal mean and SD (applied after standardizing).
#' @param skew Target marginal skewness (3rd standardized moment).
#' @param kurtosis Target marginal excess kurtosis (4th standardized moment − 3).
#' @return Numeric vector of length `n` with the requested marginal moments.
#' @export
medsim_rnonnormal <- function(n, mean = 0, sd = 1, skew = 0, kurtosis = 0) {
  stop("WS-A not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}
