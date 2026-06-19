# Sobol / functional-ANOVA share estimator adapter — variance_share kind
# Wraps an external sobol_pmed() estimator into the flat-numeric contract that
# medsim_run() / medsim_analyze_coverage() expect.

#' Sobol / functional-ANOVA variance-share estimator wrapper
#'
#' @description
#' Adapter that calls an external `sobol_pmed()` estimator and returns the flat
#' field contract keyed by the `pmed_sobol` token:
#'
#' | Field | Description |
#' |-------|-------------|
#' | `pmed_sobol` | Point estimate of the Sobol share \eqn{V_{med}/V_T} |
#' | `pmed_sobol_ci_lower` | Lower Wald CI bound |
#' | `pmed_sobol_ci_upper` | Upper Wald CI bound |
#'
#' Following medsim convention, the actual estimator is **not vendored** into
#' the package (medsim keeps estimators external — it `Suggests` companion
#' packages, never hard-depends on them).  The prototype lives at
#' `pmed-modern-sobol/03-sobol-pmed/sims/sobol_pmed.R`; `source()` it (or load
#' the future `probmed` export) before running, or pass it via `estimator`.
#'
#' The external `sobol_pmed(d, ...)` is expected to return a list with
#' `P_med_sobol` (scalar) and `ci` (length-2 numeric `c(lower, upper)`).
#'
#' @param data A `data.frame` with columns `A`, `M`, `Y`, `C`.
#' @param params Named list from [medsim_scenario_sobol()] (unused by the
#'   estimator itself, but accepted for the standard `method(data, params)`
#'   signature; may carry `covars`, `K`, `level`).
#' @param estimator Function implementing the Sobol estimator.  Defaults to
#'   `get0("sobol_pmed")`, i.e. an object named `sobol_pmed` visible on the
#'   search path (e.g. after `source()`-ing the prototype).
#' @param covars Character vector of covariate column names passed to the
#'   estimator (default `"C"`).
#' @param K Integer cross-fitting folds passed to the estimator (default 5).
#' @param level Numeric CI level passed to the estimator (default 0.95).
#' @param seed Integer seed passed to the estimator (default 1).
#'
#' @return A named list with fields `pmed_sobol`, `pmed_sobol_ci_lower`,
#'   `pmed_sobol_ci_upper`.
#'
#' @seealso [medsim_scenario_sobol()], [medsim_analyze_coverage()]
#'
#' @export
medsim_method_sobol <- function(data, params = list(),
                                 estimator = get0("sobol_pmed"),
                                 covars = "C",
                                 K      = 5L,
                                 level  = 0.95,
                                 seed   = 1L) {
  if (!all(c("A", "M", "Y", "C") %in% names(data))) {
    stop("data must have columns A, M, Y, C")
  }
  if (!is.function(estimator)) {
    stop("estimator must be a function. Provide sobol_pmed() via the ",
         "'estimator' argument or source() it onto the search path ",
         "(see ?medsim_method_sobol).")
  }

  res <- estimator(data, covars = covars, K = K, level = level, seed = seed)

  if (is.null(res$P_med_sobol) || is.null(res$ci) || length(res$ci) != 2L) {
    stop("estimator must return a list with `P_med_sobol` (scalar) and ",
         "`ci` (length-2 numeric c(lower, upper))")
  }

  list(pmed_sobol          = unname(res$P_med_sobol),
       pmed_sobol_ci_lower = unname(res$ci[1]),
       pmed_sobol_ci_upper = unname(res$ci[2]))
}
