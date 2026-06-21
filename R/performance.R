#' ADEMP performance summary (bias, SEs, RMSE) with Monte Carlo SEs
#'
#' Per scenario, computes bias, empirical SE, mean model SE, and RMSE for a
#' parameter, each with its Monte Carlo SE (Morris, White & Crowther 2019).
#'
#' @param results Results data.frame from [medsim_run()]; expects `<param>`,
#'   `<param>_truth`, and (optionally) `<param>_se` columns.
#' @param param Parameter name (default "indirect").
#' @return A data.frame, one row per scenario.
#' @export
medsim_analyze_performance <- function(results, param = "indirect") {
  est_col   <- param
  truth_col <- paste0(param, "_truth")
  se_col    <- paste0(param, "_se")
  stopifnot(est_col %in% names(results), truth_col %in% names(results))
  split(results, results$scenario) |> lapply(function(d) {
    est   <- d[[est_col]]
    keep  <- !is.na(est)
    n     <- sum(keep)
    est   <- est[keep]
    truth <- d[[truth_col]][keep][1]
    emp_se <- stats::sd(est)
    bias   <- mean(est) - truth
    data.frame(
      scenario     = d$scenario[1],
      parameter    = param,
      bias         = bias,
      bias_mcse    = emp_se / sqrt(n),
      empirical_se = emp_se,
      model_se     = if (se_col %in% names(d)) mean(d[[se_col]][keep], na.rm = TRUE) else NA_real_,
      rmse         = sqrt(mean((est - truth)^2)),
      n_converged  = as.integer(n),
      n_failed     = as.integer(nrow(d) - n),
      stringsAsFactors = FALSE
    )
  }) |> (\(x) do.call(rbind, x))() |> `rownames<-`(NULL)
}

#' Render a performance summary as a LaTeX table
#' @param perf Output of [medsim_analyze_performance()].
#' @return list(latex=, type="performance").
#' @export
medsim_table_performance <- function(perf) {
  cols <- c("scenario", "bias", "empirical_se", "model_se", "rmse", "n_failed")
  body <- apply(perf[cols], 1, function(r) paste(r, collapse = " & "))
  latex <- paste0(
    "\\begin{tabular}{lrrrrr}\n\\hline\n",
    "Scenario & Bias & Emp.\\ SE & Model SE & RMSE & $n_{fail}$ \\\\\n\\hline\n",
    paste(body, collapse = " \\\\\n"), " \\\\\n\\hline\n\\end{tabular}\n"
  )
  list(latex = latex, type = "performance")
}
