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
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }
  if (!is.list(true_params)) {
    stop("true_params must be a list (a, b, cp, residual SDs)")
  }
  if (!is.character(mechanism) || length(mechanism) != 1) {
    stop("mechanism must be a single character string")
  }
  mechanism <- match.arg(mechanism, c("MCAR", "MAR", "MNAR"))
  if (!is.numeric(prop) || length(prop) != 1 || prop < 0 || prop > 1) {
    stop("prop must be a single number in [0, 1]")
  }
  if (!is.character(target) || length(target) < 1) {
    stop("target must be a non-empty character vector of column names")
  }
  if (!is.null(nonnormal)) {
    if (!is.list(nonnormal) ||
        !all(c("skew", "kurtosis") %in% names(nonnormal))) {
      stop("nonnormal must be NULL or a list with `skew` and `kurtosis`")
    }
  }

  medsim_scenario(
    name = name,
    description = sprintf(
      "%s missing on %s (prop=%.2f)%s",
      mechanism, paste(target, collapse = "+"), prop,
      if (is.null(nonnormal)) "" else ", nonnormal"
    ),
    data_generator = function(n) {
      d <- .gen_complete_med(n, true_params, nonnormal)
      medsim_amputate(
        d,
        target = target,
        mechanism = mechanism,
        prop = prop,
        predictors = setdiff(names(d), target)
      )
    },
    params = list(
      true_params = true_params,
      mechanism = mechanism,
      prop = prop,
      target = target,
      nonnormal = nonnormal
    )
  )
}

# Internal: draw a complete X -> M -> Y mediation data.frame from `true_params`,
# with optional nonnormal residuals on M and Y. Returns columns X, M, Y.
.gen_complete_med <- function(n, true_params, nonnormal = NULL) {
  a  <- if (!is.null(true_params$a))  true_params$a  else 0
  b  <- if (!is.null(true_params$b))  true_params$b  else 0
  cp <- if (!is.null(true_params$cp)) true_params$cp else 0
  sd_M <- if (!is.null(true_params$sd_M)) true_params$sd_M else 1
  sd_Y <- if (!is.null(true_params$sd_Y)) true_params$sd_Y else 1

  .draw_resid <- function(sd) {
    if (is.null(nonnormal)) {
      stats::rnorm(n, mean = 0, sd = sd)
    } else {
      medsim_rnonnormal(
        n,
        mean = 0,
        sd = sd,
        skew = nonnormal$skew,
        kurtosis = nonnormal$kurtosis
      )
    }
  }

  X <- stats::rnorm(n)
  M <- a * X + .draw_resid(sd_M)
  Y <- b * M + cp * X + .draw_resid(sd_Y)

  data.frame(X = X, M = M, Y = Y)
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
  if (!is.list(true_params_list) || length(true_params_list) < 1) {
    stop("true_params_list must be a non-empty list of true_params lists")
  }
  if (!is.character(mechanisms) || length(mechanisms) < 1) {
    stop("mechanisms must be a non-empty character vector")
  }
  if (!is.numeric(props) || length(props) < 1) {
    stop("props must be a non-empty numeric vector")
  }
  if (!is.list(nonnormal_list) || length(nonnormal_list) < 1) {
    stop("nonnormal_list must be a non-empty list (use list(NULL) for normal)")
  }

  # Use names when present so scenario names are informative.
  tp_names <- names(true_params_list)
  if (is.null(tp_names)) {
    tp_names <- as.character(seq_along(true_params_list))
  }
  nn_names <- names(nonnormal_list)

  scenarios <- list()
  for (i in seq_along(true_params_list)) {
    for (mech in mechanisms) {
      for (p in props) {
        for (j in seq_along(nonnormal_list)) {
          nn <- nonnormal_list[[j]]
          nn_tag <- if (is.null(nn)) {
            ""
          } else if (!is.null(nn_names) && nzchar(nn_names[[j]])) {
            paste0("_", nn_names[[j]])
          } else {
            "_nonnormal"
          }
          nm <- sprintf("%s_%s_p%g%s", tp_names[[i]], mech, p, nn_tag)
          scenarios[[length(scenarios) + 1L]] <- medsim_scenario_missing(
            name = nm,
            true_params = true_params_list[[i]],
            mechanism = mech,
            prop = p,
            nonnormal = nn
          )
        }
      }
    }
  }

  scenarios
}
