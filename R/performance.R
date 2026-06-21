#' ADEMP performance summary (bias, SEs, RMSE) with Monte Carlo SEs
#'
#' Per scenario, computes bias, empirical SE, mean model SE, and RMSE for a
#' parameter, each with its Monte Carlo SE (Morris, White & Crowther 2019).
#'
#' @param results Results data.frame from [medsim_run()]; expects `<param>`,
#'   `<param>_truth`, and (optionally) `<param>_se` columns.
#' @param param Parameter name (default "indirect").
#' @return A data.frame, one row per scenario.
#' @details Truth is taken as the first converged row's `<param>_truth` value
#'   and assumed constant within scenario. Rows with NA estimates are excluded
#'   from computations; if all estimates are NA, numeric columns return NA.
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
    # Edge case: all estimates NA
    if (n == 0L) {
      return(data.frame(
        scenario     = d$scenario[1],
        parameter    = param,
        bias         = NA_real_,
        bias_mcse    = NA_real_,
        empirical_se = NA_real_,
        model_se     = NA_real_,
        rmse         = NA_real_,
        n_converged  = 0L,
        n_failed     = as.integer(nrow(d)),
        stringsAsFactors = FALSE
      ))
    }
    est   <- est[keep]
    truth <- d[[truth_col]][keep][1]
    emp_se <- stats::sd(est)
    # Edge case: truth is NA
    bias      <- if (is.na(truth)) NA_real_ else mean(est) - truth
    bias_mcse <- if (is.na(truth)) NA_real_ else emp_se / sqrt(n)
    rmse      <- if (is.na(truth)) NA_real_ else sqrt(mean((est - truth)^2))
    data.frame(
      scenario     = d$scenario[1],
      parameter    = param,
      bias         = bias,
      bias_mcse    = bias_mcse,
      empirical_se = emp_se,
      model_se     = if (se_col %in% names(d)) mean(d[[se_col]][keep], na.rm = TRUE) else NA_real_,
      rmse         = rmse,
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
  fmt4 <- function(x) ifelse(is.na(x), "NA", sprintf("%.4f", x))
  rows <- apply(perf, 1, function(r) {
    paste(
      r[["scenario"]],
      fmt4(as.numeric(r[["bias"]])),
      fmt4(as.numeric(r[["bias_mcse"]])),
      fmt4(as.numeric(r[["empirical_se"]])),
      fmt4(as.numeric(r[["model_se"]])),
      fmt4(as.numeric(r[["rmse"]])),
      as.integer(r[["n_failed"]]),
      sep = " & "
    )
  })
  latex <- paste0(
    "\\begin{tabular}{lrrrrrr}\n\\hline\n",
    "Scenario & Bias & MCSE & Emp.\\ SE & Model SE & RMSE & $n_{fail}$ \\\\\n\\hline\n",
    paste(rows, collapse = " \\\\\n"), " \\\\\n\\hline\n\\end{tabular}\n"
  )
  list(latex = latex, type = "performance")
}
