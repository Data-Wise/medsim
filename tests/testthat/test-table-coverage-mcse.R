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

test_that("coverage table end-to-end via medsim_analyze_coverage includes MCSE value", {
  # Build a minimal medsim_results fixture with CI columns and truth
  n_reps <- 8L
  set.seed(42)
  results_df <- data.frame(
    scenario            = rep("s1", n_reps),
    estimate_ci_lower   = c(0.1, 0.2, 0.15, 0.05, 0.12, 0.18, 0.08, 0.14),
    estimate_ci_upper   = c(0.5, 0.6, 0.55, 0.45, 0.52, 0.58, 0.48, 0.54),
    stringsAsFactors = FALSE
  )
  truth_df <- data.frame(
    scenario         = "s1",
    estimate_truth   = 0.30,   # falls inside all CIs → coverage = 1.0
    stringsAsFactors = FALSE
  )
  res <- structure(
    list(
      results  = results_df,
      truth    = truth_df,
      summary  = data.frame(n_scenarios = 1, n_replications = n_reps),
      config   = list(),
      scenarios = list(),
      method_name = "test_method",
      timestamp = Sys.time()
    ),
    class = c("medsim_results", "list")
  )

  cov <- medsim_analyze_coverage(res, by_scenario = FALSE)
  tbl <- medsim_table_coverage(cov, by_scenario = FALSE)
  latex <- paste(tbl, collapse = "\n")

  # Coverage MCSE should appear (0.000 when coverage = 1.0)
  expect_match(latex, "MCSE", fixed = TRUE)
  expect_match(latex, "0.000", fixed = TRUE)
  # Table must have exactly 4 & separators per data row (5 columns)
  data_rows <- grep(" \\\\\\\\$", strsplit(latex, "\n")[[1]], value = TRUE)
  data_rows <- data_rows[!grepl("toprule|midrule|bottomrule|hline", data_rows)]
  for (row in data_rows) {
    n_amp <- nchar(row) - nchar(gsub("&", "", row, fixed = TRUE))
    expect_equal(n_amp, 4L,
      info = paste("Row should have 4 '&' separators (5 columns):", row))
  }
})

test_that("coverage table handles legacy coverage frame without MCSE/n_failed columns", {
  # Legacy / hand-built coverage frame — no coverage_mcse or n_failed columns
  coverage_df <- data.frame(
    parameter = c("indirect", "direct"),
    coverage  = c(0.94, 0.93),
    n_valid   = c(100L, 100L),
    stringsAsFactors = FALSE
  )
  # Strip new columns to simulate legacy object
  cov_legacy <- structure(
    list(
      coverage    = coverage_df,
      by_scenario = NULL,
      summary     = data.frame(n_scenarios = 1, n_parameters = 2, overall_coverage = 0.935),
      ci_levels_expected = 0.95
    ),
    class = c("medsim_coverage", "list")
  )

  # Must not error
  tbl <- medsim_table_coverage(cov_legacy, by_scenario = FALSE)
  latex <- paste(tbl, collapse = "\n")

  # Must contain "NA" sentinels rather than malformed rows
  expect_match(latex, "NA", fixed = TRUE)

  # Each data row must still have exactly 4 '&' separators (5 columns)
  data_rows <- grep(" \\\\\\\\$", strsplit(latex, "\n")[[1]], value = TRUE)
  data_rows <- data_rows[!grepl("toprule|midrule|bottomrule|hline", data_rows)]
  for (row in data_rows) {
    n_amp <- nchar(row) - nchar(gsub("&", "", row, fixed = TRUE))
    expect_equal(n_amp, 4L,
      info = paste("Row should have 4 '&' separators (5 columns):", row))
  }
})
