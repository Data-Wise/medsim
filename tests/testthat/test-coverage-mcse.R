# Helper: wrap a plain data.frame as a medsim_results object with truth table
make_medsim_results_with_truth <- function(results_df, truth_df) {
  r <- list(
    results     = results_df,
    truth       = truth_df,
    config      = list(),
    scenarios   = list(),
    method_name = "test",
    timestamp   = Sys.time()
  )
  class(r) <- c("medsim_results", "list")
  r
}

test_that("coverage_mcse equals sqrt(p(1-p)/n_valid)", {
  # Results df must include the estimate column so merge produces estimate_truth suffix
  res <- make_medsim_results_with_truth(
    results_df = data.frame(
      scenario          = "s1",
      estimate          = c(-0.5, -0.3, 0.2, 2.5),   # point estimates (needed for merge suffix)
      estimate_ci_lower = c(-1, -1, -1, 2),           # 3 cover 0, 1 misses
      estimate_ci_upper = c( 1,  1,  1, 3),
      stringsAsFactors  = FALSE
    ),
    truth_df = data.frame(
      scenario = "s1",
      estimate = 0,
      stringsAsFactors = FALSE
    )
  )
  cov <- medsim_analyze_coverage(res, ci_suffix = "_ci", ci_levels = 0.95)
  row <- cov$coverage[cov$coverage$parameter == "estimate", ]
  p <- 0.75; n <- 4
  expect_equal(row$coverage, p)
  expect_equal(row$coverage_mcse, sqrt(p * (1 - p) / n))
})

test_that("n_failed counts NA-dropped runs; denominator unchanged", {
  res <- make_medsim_results_with_truth(
    results_df = data.frame(
      scenario          = "s1",
      estimate          = c(-0.3, -0.2, NA, -0.1),
      estimate_ci_lower = c(-1, -1, NA, -1),
      estimate_ci_upper = c( 1,  1, NA,  1),
      stringsAsFactors  = FALSE
    ),
    truth_df = data.frame(
      scenario = "s1",
      estimate = 0,
      stringsAsFactors = FALSE
    )
  )
  cov <- medsim_analyze_coverage(res, ci_suffix = "_ci", ci_levels = 0.95)
  row <- cov$coverage[cov$coverage$parameter == "estimate", ]
  expect_equal(row$n_valid, 3L)
  expect_equal(row$n_failed, 1L)
  expect_equal(row$failure_rate, 1 / 4)
  expect_equal(row$coverage, 1)          # 3/3 converged cover -> coverage unchanged
})
