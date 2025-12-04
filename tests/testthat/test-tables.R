# Tests for table generation functions

# Helper to create mock analysis objects
create_mock_analysis <- function() {
  accuracy <- data.frame(
    parameter = c("indirect", "a_path", "b_path"),
    bias = c(0.001, 0.002, -0.001),
    mae = c(0.015, 0.020, 0.018),
    rmse = c(0.018, 0.025, 0.022),
    median_ae = c(0.012, 0.015, 0.014),
    max_ae = c(0.050, 0.060, 0.055),
    relative_bias = c(1.2, 2.5, -1.5),
    stringsAsFactors = FALSE
  )

  by_scenario <- data.frame(
    scenario = rep(c("Independent", "High Correlation"), each = 3),
    parameter = rep(c("indirect", "a_path", "b_path"), 2),
    bias = c(0.001, 0.002, -0.001, 0.002, 0.003, -0.002),
    mae = c(0.015, 0.020, 0.018, 0.012, 0.018, 0.016),
    rmse = c(0.018, 0.025, 0.022, 0.015, 0.022, 0.019),
    median_ae = c(0.012, 0.015, 0.014, 0.010, 0.014, 0.013),
    max_ae = c(0.050, 0.060, 0.055, 0.040, 0.050, 0.045),
    relative_bias = c(1.2, 2.5, -1.5, 2.0, 3.0, -2.0),
    stringsAsFactors = FALSE
  )

  analysis <- list(
    accuracy = accuracy,
    by_scenario = by_scenario,
    summary = data.frame(n_scenarios = 2, n_replications = 100, n_parameters = 3),
    metrics_computed = c("bias", "mae", "rmse", "median_ae", "max_ae", "relative_bias")
  )

  class(analysis) <- c("medsim_analysis", "list")
  return(analysis)
}

create_mock_coverage <- function() {
  coverage <- data.frame(
    parameter = "indirect",
    coverage = 0.94,
    n_valid = 100,
    stringsAsFactors = FALSE
  )

  by_scenario <- data.frame(
    scenario = c("Independent", "High Correlation"),
    parameter = c("indirect", "indirect"),
    coverage = c(0.95, 0.93),
    n_valid = c(50, 50),
    stringsAsFactors = FALSE
  )

  cov <- list(
    coverage = coverage,
    by_scenario = by_scenario,
    summary = data.frame(n_scenarios = 2, n_parameters = 1, overall_coverage = 0.94),
    ci_levels_expected = c(0.90, 0.95, 0.99)
  )

  class(cov) <- c("medsim_coverage", "list")
  return(cov)
}

create_mock_power <- function() {
  power <- data.frame(
    parameter = "indirect",
    power = 0.85,
    n_valid = 100,
    alpha = 0.05,
    stringsAsFactors = FALSE
  )

  by_scenario <- data.frame(
    scenario = c("Independent", "High Correlation"),
    parameter = c("indirect", "indirect"),
    power = c(0.82, 0.88),
    n_valid = c(50, 50),
    alpha = c(0.05, 0.05),
    stringsAsFactors = FALSE
  )

  pwr <- list(
    power = power,
    by_scenario = by_scenario,
    summary = data.frame(n_scenarios = 2, n_parameters = 1, overall_power = 0.85, alpha = 0.05),
    alpha = 0.05
  )

  class(pwr) <- c("medsim_power", "list")
  return(pwr)
}

create_mock_results <- function(name = "test") {
  results <- list(
    results = data.frame(
      scenario = rep("Test", 20),
      replication = 1:20,
      indirect = rnorm(20, 0.09, 0.02),
      elapsed = runif(20, 0.01, 0.1),
      indirect_ci_lower = rnorm(20, 0.05, 0.01),
      indirect_ci_upper = rnorm(20, 0.13, 0.01),
      indirect_p = runif(20, 0, 0.1),
      stringsAsFactors = FALSE
    ),
    truth = data.frame(
      scenario = "Test",
      indirect = 0.09,
      stringsAsFactors = FALSE
    ),
    config = list(n_replications = 20),
    method_name = name,
    timestamp = Sys.time()
  )

  class(results) <- c("medsim_results", "list")
  return(results)
}

create_mock_comparison <- function() {
  accuracy_comparison <- data.frame(
    method = c("Method1", "Method1", "Method2", "Method2"),
    parameter = c("indirect", "a_path", "indirect", "a_path"),
    mae = c(0.015, 0.020, 0.025, 0.030),
    rmse = c(0.018, 0.025, 0.030, 0.035),
    bias = c(0.001, 0.002, 0.005, 0.008),
    stringsAsFactors = FALSE
  )

  timing_comparison <- data.frame(
    method = c("Method1", "Method2"),
    mean_time = c(0.05, 0.10),
    median_time = c(0.04, 0.09),
    min_time = c(0.01, 0.05),
    max_time = c(0.10, 0.20),
    total_time = c(5.0, 10.0),
    n_runs = c(100, 100),
    stringsAsFactors = FALSE
  )

  coverage_comparison <- data.frame(
    method = c("Method1", "Method2"),
    parameter = c("indirect", "indirect"),
    coverage = c(0.95, 0.92),
    n_valid = c(100, 100),
    stringsAsFactors = FALSE
  )

  comparison <- list(
    accuracy_comparison = accuracy_comparison,
    timing_comparison = timing_comparison,
    coverage_comparison = coverage_comparison,
    summary = data.frame(n_methods = 2, methods = "Method1, Method2"),
    metrics_compared = c("accuracy", "timing", "coverage")
  )

  class(comparison) <- c("medsim_comparison", "list")
  return(comparison)
}

# ==============================================================================
# FORMATTING HELPER TESTS
# ==============================================================================

test_that(".format_number_latex produces valid LaTeX", {
  expect_equal(medsim:::.format_number_latex(0.123, digits = 3), "$0.123$")
  expect_equal(medsim:::.format_number_latex(0.1, digits = 2), "$0.10$")
  expect_equal(medsim:::.format_number_latex(NA), "---")
  expect_equal(medsim:::.format_number_latex(Inf), "---")
})

test_that(".format_scientific_latex produces valid scientific notation", {
  result <- medsim:::.format_scientific_latex(0.00012, digits = 1)
  expect_true(grepl("\\\\times", result))
  expect_true(grepl("10\\^", result))

  # Normal numbers should not be scientific
  result_normal <- medsim:::.format_scientific_latex(0.5, digits = 1)
  expect_false(grepl("\\\\times", result_normal))

  expect_equal(medsim:::.format_scientific_latex(NA), "---")
})

test_that(".format_time_latex auto-scales correctly", {
  # Milliseconds
  ms_result <- medsim:::.format_time_latex(0.005)
  expect_true(grepl("ms", ms_result))

  # Seconds
  s_result <- medsim:::.format_time_latex(5)
  expect_true(grepl(" s", s_result))

  # Minutes
  min_result <- medsim:::.format_time_latex(120)
  expect_true(grepl("min", min_result))

  # Hours
  h_result <- medsim:::.format_time_latex(7200)
  expect_true(grepl(" h", h_result))

  expect_equal(medsim:::.format_time_latex(NA), "---")
})

test_that(".format_percentage_latex produces valid percentages", {
  result <- medsim:::.format_percentage_latex(0.95, digits = 1)
  expect_true(grepl("95", result))
  expect_true(grepl("\\\\%", result))

  expect_equal(medsim:::.format_percentage_latex(NA), "---")
})

test_that(".format_pvalue_latex handles edge cases", {
  expect_equal(medsim:::.format_pvalue_latex(0.0001), "$< 0.001$")
  expect_true(grepl("0\\.05", medsim:::.format_pvalue_latex(0.05)))
  expect_equal(medsim:::.format_pvalue_latex(NA), "---")
})

test_that(".format_speedup_latex produces valid speedup", {
  result <- medsim:::.format_speedup_latex(2.5, digits = 1)
  expect_true(grepl("2\\.5", result))
  expect_true(grepl("\\\\times", result))
})

# ==============================================================================
# TABLE GENERATION TESTS
# ==============================================================================

test_that("medsim_table_accuracy creates valid LaTeX", {
  analysis <- create_mock_analysis()
  table <- medsim_table_accuracy(analysis)

  expect_s3_class(table, "medsim_table")
  expect_true(any(grepl("\\\\begin\\{table\\}", table)))
  expect_true(any(grepl("\\\\end\\{table\\}", table)))
  expect_true(any(grepl("\\\\toprule", table)))
  expect_true(any(grepl("\\\\bottomrule", table)))
  expect_true(any(grepl("indirect", table)))
})

test_that("medsim_table_accuracy handles by_scenario option", {
  analysis <- create_mock_analysis()

  # With scenarios
  table_with <- medsim_table_accuracy(analysis, by_scenario = TRUE)
  expect_true(any(grepl("Independent", table_with)))

  # Without scenarios
  table_without <- medsim_table_accuracy(analysis, by_scenario = FALSE)
  expect_false(any(grepl("Independent", table_without)))
})

test_that("medsim_table_accuracy respects metrics argument", {
  analysis <- create_mock_analysis()

  table <- medsim_table_accuracy(analysis, metrics = c("mae", "rmse"))
  expect_true(any(grepl("MAE", table)))
  expect_true(any(grepl("RMSE", table)))
  expect_false(any(grepl("Bias", table)))  # Not included
})

test_that("medsim_table_accuracy validates inputs", {
  expect_error(medsim_table_accuracy("not an analysis"), "medsim_analysis")
  expect_error(medsim_table_accuracy(create_mock_analysis(), metrics = "invalid"), "Invalid metrics")
})

test_that("medsim_table_timing creates valid LaTeX", {
  results1 <- create_mock_results("Method1")
  results2 <- create_mock_results("Method2")

  table <- medsim_table_timing(list(Method1 = results1, Method2 = results2))

  expect_s3_class(table, "medsim_table")
  expect_true(any(grepl("Method1", table)))
  expect_true(any(grepl("Method2", table)))
  expect_true(any(grepl("Speedup", table)))
})

test_that("medsim_table_timing handles include_speedup option", {
  results1 <- create_mock_results("Fast")
  results2 <- create_mock_results("Slow")
  results2$results$elapsed <- results2$results$elapsed * 2

  table_with <- medsim_table_timing(list(Fast = results1, Slow = results2), include_speedup = TRUE)
  expect_true(any(grepl("Speedup", table_with)))

  table_without <- medsim_table_timing(list(Fast = results1, Slow = results2), include_speedup = FALSE)
  expect_false(any(grepl("Speedup", table_without)))
})

test_that("medsim_table_coverage creates valid LaTeX", {
  coverage <- create_mock_coverage()
  table <- medsim_table_coverage(coverage)

  expect_s3_class(table, "medsim_table")
  expect_true(any(grepl("Coverage", table)))
  expect_true(any(grepl("indirect", table)))
})

test_that("medsim_table_power creates valid LaTeX", {
  power <- create_mock_power()
  table <- medsim_table_power(power)

  expect_s3_class(table, "medsim_table")
  expect_true(any(grepl("Power", table)))
  expect_true(any(grepl("alpha", table, ignore.case = TRUE)))
})

test_that("medsim_table_comparison creates valid LaTeX", {
  comparison <- create_mock_comparison()
  table <- medsim_table_comparison(comparison)

  expect_s3_class(table, "medsim_table")
  expect_true(any(grepl("Method1", table)))
  expect_true(any(grepl("Method2", table)))
})

# ==============================================================================
# OUTPUT FUNCTION TESTS
# ==============================================================================

test_that("medsim_write_table writes LaTeX file", {
  analysis <- create_mock_analysis()
  table <- medsim_table_accuracy(analysis)

  temp_file <- tempfile(fileext = ".tex")
  medsim_write_table(table, temp_file)

  expect_true(file.exists(temp_file))

  content <- readLines(temp_file)
  expect_true(any(grepl("\\\\begin\\{table\\}", content)))

  unlink(temp_file)
})

test_that("medsim_write_table infers format from extension", {
  analysis <- create_mock_analysis()
  table <- medsim_table_accuracy(analysis)

  # CSV
  csv_file <- tempfile(fileext = ".csv")
  expect_warning(medsim_write_table(table, csv_file), "CSV export")
  expect_true(file.exists(csv_file))
  unlink(csv_file)

  # Markdown
  md_file <- tempfile(fileext = ".md")
  expect_warning(medsim_write_table(table, md_file), "Markdown export")
  expect_true(file.exists(md_file))
  unlink(md_file)
})

test_that("medsim_write_table creates directories", {
  analysis <- create_mock_analysis()
  table <- medsim_table_accuracy(analysis)

  temp_dir <- file.path(tempdir(), "nested", "dir")
  temp_file <- file.path(temp_dir, "table.tex")

  medsim_write_table(table, temp_file)
  expect_true(file.exists(temp_file))

  unlink(temp_dir, recursive = TRUE)
})

test_that("print.medsim_table outputs to console", {
  analysis <- create_mock_analysis()
  table <- medsim_table_accuracy(analysis)

  expect_output(print(table), "medsim LaTeX Table")
  expect_output(print(table), "accuracy")
})

# ==============================================================================
# EDGE CASE TESTS
# ==============================================================================

test_that("tables handle NA values gracefully", {
  analysis <- create_mock_analysis()
  analysis$accuracy$bias[1] <- NA

  table <- medsim_table_accuracy(analysis, by_scenario = FALSE)
  expect_true(any(grepl("---", table)))
})

test_that("tables handle very small numbers", {
  analysis <- create_mock_analysis()
  analysis$accuracy$bias[1] <- 1e-8

  table <- medsim_table_accuracy(analysis, by_scenario = FALSE)
  expect_true(any(grepl("\\\\times", table)))  # Scientific notation
})

test_that("tables handle zero values", {
  analysis <- create_mock_analysis()
  analysis$accuracy$bias[1] <- 0

  table <- medsim_table_accuracy(analysis, by_scenario = FALSE)
  expect_s3_class(table, "medsim_table")
  # Zero should not be in scientific notation
  expect_true(any(grepl("\\$0\\.000\\$", table)))
})
