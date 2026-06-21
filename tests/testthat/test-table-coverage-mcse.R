test_that("coverage table renders MCSE and failures", {
  # Build a medsim_coverage object with the new columns (as produced by Task 1)
  p <- 0.75
  n <- 4L
  coverage_df <- data.frame(
    parameter     = "estimate",
    coverage      = p,
    coverage_mcse = sqrt(p * (1 - p) / n),
    n_valid       = n,
    n_failed      = 0L,
    failure_rate  = 0,
    stringsAsFactors = FALSE
  )
  cov <- structure(
    list(
      coverage    = coverage_df,
      by_scenario = coverage_df,
      summary     = data.frame(n_scenarios = 1, n_parameters = 1, overall_coverage = p),
      ci_levels_expected = 0.95
    ),
    class = c("medsim_coverage", "list")
  )
  tbl <- medsim_table_coverage(cov, by_scenario = FALSE)
  latex <- paste(tbl, collapse = "\n")
  expect_match(latex, "MCSE", fixed = TRUE)
  expect_match(latex, "75.0", fixed = TRUE)   # coverage 0.75 rendered as 75.0%
})
