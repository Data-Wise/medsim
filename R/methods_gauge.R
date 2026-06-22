# R/methods_gauge.R
# Gauge-residual estimator adapter — wraps probmed::ward_residual() into the
# flat-field contract medsim_run()/medsim_analyze_coverage() expect.

# Read a field from either an S7 GaugePmedResult (@/prop) or a plain list.
#' @noRd
.medsim_gauge_get <- function(res, field) {
  if (is.list(res) && !isS4(res)) return(res[[field]])
  S7::prop(res, field)
}

#' Gauge-residual (P_med + W) estimator wrapper
#'
#' @description
#' Adapter that calls an external `ward_residual()` estimator and returns the
#' flat field contract for two parameters:
#'
#' | Field | Description |
#' |-------|-------------|
#' | `pmed`, `pmed_ci_lower/_upper` | interventional proportion mediated `IIE/OE` |
#' | `w`, `w_ci_lower/_upper` | gauge residual `R/OE` |
#'
#' The estimator is **not vendored** (medsim keeps estimators external). Pass it
#' via `estimator`, or default to an object named `ward_residual` on the search
#' path / `probmed::ward_residual`. The estimator must return an S7
#' `GaugePmedResult` (read via `@p_med/@p_med_ci/@W/@W_ci`) or a plain list with
#' fields `p_med`, `p_med_ci` (length 2), `W`, `W_ci` (length 2).
#'
#' @param data data.frame with columns `A`, `M`, `Y`, `C`.
#' @param params Named list from [medsim_scenario_gauge()] (accepted for the
#'   `method(data, params)` signature; unused by the estimator).
#' @param estimator Function; default `get0("ward_residual")`.
#' @param covars Character covariate names (default `"C"`).
#' @param K Integer cross-fitting folds (default 5).
#' @param ci_level Numeric CI level (default 0.95).
#' @param seed Integer seed (default 1).
#' @param reps Integer repeated cross-fitting fold draws (default 1).
#' @param se_method `"analytic"` (default) or `"bootstrap"` (percentile).
#' @param B Integer bootstrap resamples when `se_method = "bootstrap"` (default 200).
#' @param fieller Logical: compute the estimator's Fieller bounds (default
#'   `FALSE`). medsim reports only the Wald/percentile `pmed`/`w` CIs, so the
#'   Fieller set is discarded — disabling it avoids wasted compute per
#'   replication across a large grid.
#' @return Named list `pmed, pmed_ci_lower, pmed_ci_upper, w, w_ci_lower, w_ci_upper`.
#' @seealso [medsim_scenario_gauge()], [medsim_analyze_coverage()]
#' @export
medsim_method_gauge <- function(data, params = list(),
                                estimator = get0("ward_residual"),
                                covars = "C", K = 5L, ci_level = 0.95,
                                seed = 1L, reps = 1L,
                                se_method = "analytic", B = 200L,
                                fieller = FALSE) {
  if (!all(c("A", "M", "Y", "C") %in% names(data))) {
    stop("data must have columns A, M, Y, C")
  }
  if (!is.function(estimator)) {
    stop("estimator must be a function. Provide ward_residual() via the ",
         "'estimator' argument or load probmed (see ?medsim_method_gauge).")
  }

  res <- estimator(data, covars = covars, K = K, ci_level = ci_level,
                   seed = seed, reps = reps, se_method = se_method, B = B,
                   fieller = fieller)

  pmed    <- .medsim_gauge_get(res, "p_med")
  pmed_ci <- .medsim_gauge_get(res, "p_med_ci")
  w       <- .medsim_gauge_get(res, "W")
  w_ci    <- .medsim_gauge_get(res, "W_ci")
  if (length(pmed_ci) != 2L || length(w_ci) != 2L) {
    stop("estimator must return p_med_ci and W_ci as length-2 numerics")
  }

  list(pmed = unname(pmed),
       pmed_ci_lower = unname(pmed_ci[1]), pmed_ci_upper = unname(pmed_ci[2]),
       w = unname(w),
       w_ci_lower = unname(w_ci[1]), w_ci_upper = unname(w_ci[2]))
}
