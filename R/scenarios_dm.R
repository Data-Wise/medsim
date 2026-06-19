# Differential-misclassification (DM) scenario constructor
# Spec: PROPOSAL-medsim-dm-integration.md + Phase 1 of broad overhaul spec

#' Construct a differential-misclassification (DM) mediation scenario
#'
#' @description
#' Builds a [medsim_scenario()] for the bounds-pair simulation studies
#' (me-mediator-bounds / me-exposure-recall).  The `data_generator` wraps
#' `medrobust::simulate_dm_data(...)@observed`; if \pkg{medrobust} is not
#' installed a lightweight synthetic fallback is used so that the harness and
#' `interval`-kind analysis machinery can be unit-tested without the package.
#'
#' The scenario is tagged with `estimand = medsim_estimand("interval", ...)` so
#' that [medsim_analyze_coverage()] automatically dispatches the
#' Imbens-Manski / partial-ID interval branch.
#'
#' @param name Scenario name (character).
#' @param true_params List of true generating parameters passed through to
#'   `medrobust::simulate_dm_data()` and stored in `scenario$params`.
#'   Must include at minimum `NDE` and `NIE` (the true natural direct/indirect
#'   effects) so that coverage against ground truth can be computed.
#' @param dm_params List of differential-misclassification parameters passed
#'   through to `medrobust::simulate_dm_data()`.
#' @param misclass_type `"mediator"` (default, me-mediator-bounds study) or
#'   `"exposure"` (me-exposure-recall study).
#'
#' @return A [medsim_scenario()] object with
#'   `estimand = medsim_estimand("interval", params = c("NDE", "NIE"), ...)`.
#'
#' @examples
#' # Synthetic scenario (no medrobust required):
#' sc <- medsim_scenario_dm(
#'   name        = "small_misclass",
#'   true_params = list(NDE = 0.2, NIE = 0.3, n = 200),
#'   dm_params   = list(delta = 0.1),
#'   misclass_type = "mediator"
#' )
#' # Data generator uses synthetic fallback when medrobust is absent:
#' d <- sc$data_generator(n = 50)
#' head(d)
#'
#' @seealso [medsim_method_bounds()] for the corresponding estimator adapter;
#'   [medsim_analyze_coverage()] for interval-kind coverage analysis.
#'
#' @export
medsim_scenario_dm <- function(name,
                                true_params,
                                dm_params   = list(),
                                misclass_type = c("mediator", "exposure")) {
  misclass_type <- match.arg(misclass_type)

  if (!is.character(name) || length(name) != 1L) {
    stop("name must be a single character string")
  }
  if (!is.list(true_params)) {
    stop("true_params must be a list (must include 'NDE' and 'NIE')")
  }
  if (!all(c("NDE", "NIE") %in% names(true_params))) {
    stop("true_params must include 'NDE' and 'NIE' (the ground-truth natural effects)")
  }
  if (!is.list(dm_params)) {
    stop("dm_params must be a list")
  }

  # Bake everything into the closure so data_generator(n) needs only n
  # medrobust requires specific DGM parameter keys; detect whether we have them
  medrobust_keys <- c("beta_AM", "theta_AY", "theta_MY")
  has_medrobust_params <- all(medrobust_keys %in% names(true_params))

  gen_fn <- local({
    tp    <- true_params
    dp    <- dm_params
    mt    <- misclass_type
    use_pkg <- has_medrobust_params

    function(n) {
      if (use_pkg && requireNamespace("medrobust", quietly = TRUE)) {
        sim <- medrobust::simulate_dm_data(
          n             = n,
          true_params   = tp,
          dm_params     = dp,
          misclass_type = mt
        )
        return(sim@observed)
      }
      # Lightweight synthetic fallback: used when medrobust is absent OR
      # when true_params uses NDE/NIE keys (synthetic scenario for unit tests).
      nde   <- if (!is.null(tp$NDE))      tp$NDE      else 0.3
      nie   <- if (!is.null(tp$NIE))      tp$NIE      else 0.3
      delta <- if (!is.null(dp$delta))    dp$delta    else 0.1
      a  <- rbinom(n, 1L, 0.5)
      m  <- a * nde + rnorm(n)
      y  <- m * nie + rnorm(n)
      data.frame(A = a, M = m, Y = y,
                 A_star = a + rbinom(n, 1L, delta) * (1L - 2L * a))
    }
  })

  estimand <- medsim_estimand(
    "interval",
    params = c("NDE", "NIE"),
    ci     = "imbens_manski",
    extra  = c("feasible", "falsified")
  )

  medsim_scenario(
    name          = name,
    description   = sprintf("DM scenario (%s misclassification)", misclass_type),
    data_generator = gen_fn,
    params        = true_params,
    estimand      = estimand
  )
}
