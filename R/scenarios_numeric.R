# Numeric accuracy scenario factory — numeric estimand kind
# Designed for the product-of-three study: CI method timing and abs-error
# comparisons where there is no DGM, no coverage, and no power analysis.

#' Create a numeric accuracy simulation scenario
#'
#' @description
#' Thin wrapper over [medsim_scenario()] for studies that evaluate numerical
#' **accuracy** or **timing** of a computation — not coverage or power.  The
#' estimand kind is `"numeric"`, which signals downstream commands to skip
#' coverage/power analysis and produce accuracy/timing tables instead.
#'
#' Typical use-cases:
#' - Product-of-three CI: compare DOP, MBCO, Monte-Carlo CI point accuracy
#'   and computation time across parameter cells.
#' - Approximation quality: absolute error and relative error vs. analytic truth.
#'
#' The `data_generator` for numeric scenarios is optional (default: returns
#' `data.frame()` with `n` rows of `NA`s as a placeholder).  Methods that
#' only need `params` can ignore the `data` argument.
#'
#' @param name Character: scenario name.
#' @param true_params Named list of ground-truth parameter values.  These are
#'   passed to `method(data, params)` as `params`; what counts as "truth" is
#'   method-defined.  For accuracy studies this typically includes the analytic
#'   reference value (e.g. `list(ci_true = 0.95)`).
#' @param data_generator Function `function(n)` returning a data frame.
#'   Defaults to a no-op that returns an empty data frame (suitable for methods
#'   that derive everything from `params` rather than a random sample).
#' @param extra Character vector of additional result columns beyond the
#'   mandatory `error`/`abs_error`/`elapsed_sec` columns.  These are passed to
#'   [medsim_estimand()] and stored on the scenario.
#'
#' @return A `medsim_scenario` object with `estimand$kind = "numeric"`.
#'
#' @examples
#' # Product-of-three scenario cell
#' sc <- medsim_scenario_numeric(
#'   name        = "dop_small_n",
#'   true_params = list(a  = 0.5, b = 0.3, se_a = 0.1, se_b = 0.1,
#'                      ci_true = 0.01),
#'   extra       = c("elapsed_sec")
#' )
#' str(sc$estimand)
#'
#' @seealso [medsim_estimand()], [medsim_scenario()], [medsim_analyze()]
#'
#' @export
medsim_scenario_numeric <- function(name,
                                     true_params    = list(),
                                     data_generator = NULL,
                                     extra          = character()) {
  if (!is.list(true_params)) {
    stop("true_params must be a named list")
  }

  # Default no-op data generator — returns a zero-row data frame
  if (is.null(data_generator)) {
    data_generator <- function(n) {
      data.frame(placeholder = rep(NA_real_, n))
    }
  }

  if (!is.function(data_generator)) {
    stop("data_generator must be a function(n) or NULL")
  }

  # The numeric kind: no CI concept, no coverage; only error + timing
  extra_cols <- union(c("error", "abs_error", "elapsed_sec"), extra)
  estimand   <- medsim_estimand("numeric",
                                 params = character(),
                                 ci     = "none",
                                 extra  = extra_cols)

  medsim_scenario(
    name           = name,
    description    = sprintf("Numeric accuracy scenario: %s", name),
    data_generator = data_generator,
    params         = true_params,
    estimand       = estimand
  )
}
