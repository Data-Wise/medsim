# INTERFACE FREEZE (Step 0) — signature stubs for WS-C.
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md  (§"3. Missing-data scenario constructor")
# Builds on the WS-A (medsim_rnonnormal) and WS-B (medsim_amputate) interfaces.

#' Construct a missing-data mediation scenario (WS-C)
#'
#' Wraps [medsim_scenario()] with a `data_generator` that (1) draws complete `X -> M -> Y`
#' with optional nonnormal residuals ([medsim_rnonnormal()]), then (2) amputes `target`
#' under `mechanism` ([medsim_amputate()]).
#'
#' @param name Scenario name.
#' @param true_params List of true generating parameters: `a`, `b`, `cp`, residual SDs.
#' @param mechanism One of `"MCAR"`, `"MAR"`, `"MNAR"`.
#' @param prop Target missingness proportion.
#' @param target Column(s) to make missing (default `"M"`).
#' @param nonnormal `NULL` for normal residuals, or `list(skew=, kurtosis=)`.
#' @return A `medsim_scenario` object.
#' @export
medsim_scenario_missing <- function(name, true_params, mechanism, prop = 0.2,
                                    target = "M", nonnormal = NULL) {
  stop("WS-C not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}

#' Build the full factorial of missing-data scenarios (WS-C, optional)
#'
#' Convenience: expand `SPEC-simulation-design` cells (mechanism × prop × n × effect ×
#' nonnormality) into a list of [medsim_scenario_missing()] objects in one call.
#'
#' @param true_params_list List of `true_params` lists (one per effect-size cell).
#' @param mechanisms,props Character/numeric vectors crossed factorially.
#' @param nonnormal_list List of `nonnormal` specs (incl. `NULL` for normal).
#' @return A list of `medsim_scenario` objects.
#' @export
medsim_scenario_missing_grid <- function(true_params_list, mechanisms, props,
                                         nonnormal_list = list(NULL)) {
  stop("WS-C not yet implemented — see SPEC-medsim-missingdata-generators-2026-06-11.md")
}
