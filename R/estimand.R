# Estimand-kind abstraction -- the spine of the broad overhaul.
# Spec: ~/.claude/plans/max-d-s-check-velvet-moth.md (Phase 0)

#' Declare the estimand kind for a simulation scenario
#'
#' @description
#' Attach a first-class estimand descriptor to a [medsim_scenario()].  Every
#' downstream command (`medsim_run`, `medsim_analyze_coverage`,
#' `medsim_tables`, `medsim_figures`, `medsim_workflow`) dispatches on
#' `estimand$kind` rather than assuming a scalar point-estimand.
#'
#' Four kinds are recognised:
#'
#' | kind | what `method()` returns | coverage notion |
#' |------|-------------------------|-----------------|
#' | `"point"` | `{p}`, `{p}_ci_lower/_upper`, `{p}_p` | truth ? CI |
#' | `"interval"` | `{p}_lower/_upper`, `{p}_im_lower/_im_upper`, `feasible`, `falsified` | truth ? \[lower, upper\]; IM-CI coverage |
#' | `"probabilistic"` | `pmed`, `pmed_ci_lower/_upper`, `pmed_p`, `branch_switch` | truth ? CI (truth from potential outcomes) |
#' | `"numeric"` | `error`, `abs_error`, `elapsed_sec` | none |
#'
#' Scenarios with `estimand = NULL` (the default in [medsim_scenario()]) are
#' treated as `kind = "point"` throughout the package -- full back-compatibility
#' with all existing code.
#'
#' @param kind Estimand kind: `"point"` (default), `"interval"`,
#'   `"probabilistic"`, or `"numeric"`.
#' @param params Character vector of estimand parameter names -- e.g.
#'   `c("indirect")` for point, `c("NDE", "NIE")` for interval,
#'   `c("pmed")` for probabilistic.  Defaults to `character()` (auto-inferred
#'   downstream).
#' @param truth Optional truth-extractor `function(scenario)` returning a
#'   **named** numeric vector keyed by the names in `params`.  Required for
#'   `kind = "interval"` (bounds truth differs from the estimand itself).
#' @param ci CI method: `"standard"` (Wald/bootstrap), `"imbens_manski"`
#'   (partial-ID dual CI), `"mbco"` (constrained optimisation), or `"none"`.
#' @param extra Character vector of additional result columns beyond `params`
#'   that `method()` returns -- e.g. `c("feasible", "branch_switch", "timing")`.
#'
#' @return An object of class `c("medsim_estimand", "list")`.
#'
#' @examples
#' # Point estimand (the historic default -- back-compat)
#' medsim_estimand("point")
#'
#' # Interval (partial-ID bounds)
#' medsim_estimand("interval",
#'   params = c("NDE", "NIE"),
#'   ci     = "imbens_manski",
#'   extra  = c("feasible", "falsified"))
#'
#' # Probabilistic (P_med)
#' medsim_estimand("probabilistic",
#'   params = "pmed",
#'   ci     = "mbco",
#'   extra  = "branch_switch")
#'
#' # Numerical accuracy (no coverage/power)
#' medsim_estimand("numeric", params = "error", ci = "none",
#'   extra = "elapsed_sec")
#'
#' @seealso [medsim_scenario()] for attaching an estimand to a scenario;
#'   [medsim_analyze_coverage()] for the keystone dispatch on kind.
#'
#' @export
medsim_estimand <- function(
    kind   = c("point", "interval", "probabilistic", "numeric"),
    params = character(),
    truth  = NULL,
    ci     = c("standard", "imbens_manski", "mbco", "none"),
    extra  = character()) {

  kind <- match.arg(kind)
  ci   <- match.arg(ci)

  if (!is.character(params)) {
    stop("params must be a character vector")
  }
  if (!is.null(truth) && !is.function(truth)) {
    stop("truth must be NULL or a function(scenario)")
  }
  if (!is.character(extra)) {
    stop("extra must be a character vector")
  }

  structure(
    list(kind = kind, params = params, truth = truth, ci = ci, extra = extra),
    class = c("medsim_estimand", "list")
  )
}

#' @export
print.medsim_estimand <- function(x, ...) {
  cat(sprintf("<medsim_estimand> kind = %s  ci = %s\n", x$kind, x$ci))
  if (length(x$params) > 0L) {
    cat("  params:", paste(x$params, collapse = ", "), "\n")
  }
  if (length(x$extra) > 0L) {
    cat("  extra: ", paste(x$extra, collapse = ", "), "\n")
  }
  invisible(x)
}

# Internal helper ---------------------------------------------------------

#' Resolve the effective estimand kind from a scenario (or NULL)
#'
#' Returns `"point"` when the scenario has no `estimand` field (back-compat).
#' @noRd
.medsim_estimand_kind <- function(scenario) {
  if (is.null(scenario$estimand)) "point" else scenario$estimand$kind
}
