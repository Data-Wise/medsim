# Tests for analysis functions

# Helper function to create mock results
create_mock_results <- function(include_ci = FALSE, include_p = FALSE) {
  # Create simple scenario
  scenario <- medsim_scenario(
    name = "Test",
    data_generator = function(n = 100) {
      X <- rnorm(n)
      M <- 0.3 * X + rnorm(n)
      Y <- 0.3 * M + rnorm(n)
      data.frame(X = X, M = M, Y = Y)
    },
    params = list(a = 0.3, b = 0.3, indirect = 0.09)
  )

  # Create config
  config <- medsim_config("test")
  config$n_replications <- 20  # Small for testing

  # Generate results data
  set.seed(123)
  results_data <- data.frame(
    scenario = rep("Test", 20),
    replication = 1:20,
    indirect = rnorm(20, mean = 0.09, sd = 0.02),
    elapsed = runif(20, 0.01, 0.1),
    stringsAsFactors = FALSE
  )

  # Add CI columns if requested
  if (include_ci) {
    results_data$indirect_ci_lower <- results_data$indirect - 1.96 * 0.02
    results_data$indirect_ci_upper <- results_data$indirect + 1.96 * 0.02
  }

  # Add p-value columns if requested
  if (include_p) {
    # Simulate p-values (most significant since true effect exists)
    results_data$indirect_p <- runif(20, 0, 0.1)
  }

  # Create truth data
  truth_data <- data.frame(
    scenario = "Test",
    indirect = 0.09,
    stringsAsFactors = FALSE
  )

  # Create mock results object
  results <- list(
    results = results_data,
    summary = data.frame(
      scenario = "Test",
      indirect_mean = mean(results_data$indirect),
      indirect_sd = sd(results_data$indirect),
      stringsAsFactors = FALSE
    ),
    truth = truth_data,
    config = config,
    scenarios = list(scenario),
    method_name = "mock_method",
    timestamp = Sys.time()
  )

  class(results) <- c("medsim_results", "list")

  return(results)
}

# Tests for medsim_analyze()
test_that("medsim_analyze computes basic accuracy metrics", {
  results <- create_mock_results()

  analysis <- medsim_analyze(results, metrics = c("bias", "mae", "rmse"))

  expect_s3_class(analysis, "medsim_analysis")
  expect_s3_class(analysis, "list")

  # Check structure
  expect_true("accuracy" %in% names(analysis))
  expect_true("summary" %in% names(analysis))

  # Check accuracy data.frame
  expect_s3_class(analysis$accuracy, "data.frame")
  expect_true("parameter" %in% names(analysis$accuracy))
  expect_true("bias" %in% names(analysis$accuracy))
  expect_true("mae" %in% names(analysis$accuracy))
  expect_true("rmse" %in% names(analysis$accuracy))

  # Check that metrics are numeric
  expect_type(analysis$accuracy$bias, "double")
  expect_type(analysis$accuracy$mae, "double")
  expect_type(analysis$accuracy$rmse, "double")
})

test_that("medsim_analyze computes all metrics when requested", {
  results <- create_mock_results()

  analysis <- medsim_analyze(results, metrics = "all")

  expect_true("bias" %in% names(analysis$accuracy))
  expect_true("mae" %in% names(analysis$accuracy))
  expect_true("rmse" %in% names(analysis$accuracy))
  expect_true("median_ae" %in% names(analysis$accuracy))
  expect_true("max_ae" %in% names(analysis$accuracy))
  expect_true("relative_bias" %in% names(analysis$accuracy))
})

test_that("medsim_analyze computes by-scenario metrics", {
  results <- create_mock_results()

  analysis <- medsim_analyze(results, by_scenario = TRUE)

  expect_true("by_scenario" %in% names(analysis))
  expect_s3_class(analysis$by_scenario, "data.frame")
  expect_true("scenario" %in% names(analysis$by_scenario))
  expect_true("parameter" %in% names(analysis$by_scenario))
})

test_that("medsim_analyze skips by-scenario when requested", {
  results <- create_mock_results()

  analysis <- medsim_analyze(results, by_scenario = FALSE)

  expect_null(analysis$by_scenario)
})

test_that("medsim_analyze errors without ground truth", {
  results <- create_mock_results()
  results$truth <- NULL

  expect_error(
    medsim_analyze(results),
    "no ground truth available"
  )
})

test_that("medsim_analyze errors with invalid metrics", {
  results <- create_mock_results()

  expect_error(
    medsim_analyze(results, metrics = "invalid_metric"),
    "Invalid metrics"
  )
})

test_that("medsim_analyze handles missing values", {
  results <- create_mock_results()

  # Add some NAs
  results$results$indirect[1:3] <- NA

  analysis <- medsim_analyze(results)

  # Should still work, just with fewer valid observations
  expect_s3_class(analysis, "medsim_analysis")
  expect_true(nrow(analysis$accuracy) > 0)
})

# Tests for medsim_analyze_coverage()
test_that("medsim_analyze_coverage computes coverage rates", {
  results <- create_mock_results(include_ci = TRUE)

  coverage <- medsim_analyze_coverage(results)

  expect_s3_class(coverage, "medsim_coverage")
  expect_s3_class(coverage, "list")

  # Check structure
  expect_true("coverage" %in% names(coverage))
  expect_true("summary" %in% names(coverage))

  # Check coverage data.frame
  expect_s3_class(coverage$coverage, "data.frame")
  expect_true("parameter" %in% names(coverage$coverage))
  expect_true("coverage" %in% names(coverage$coverage))
  expect_true("n_valid" %in% names(coverage$coverage))

  # Coverage should be between 0 and 1
  expect_true(all(coverage$coverage$coverage >= 0))
  expect_true(all(coverage$coverage$coverage <= 1))
})

test_that("medsim_analyze_coverage errors without CI columns", {
  results <- create_mock_results(include_ci = FALSE)

  expect_error(
    medsim_analyze_coverage(results),
    "No confidence interval columns found"
  )
})

test_that("medsim_analyze_coverage errors without ground truth", {
  results <- create_mock_results(include_ci = TRUE)
  results$truth <- NULL

  expect_error(
    medsim_analyze_coverage(results),
    "no ground truth available"
  )
})

test_that("medsim_analyze_coverage computes by-scenario coverage", {
  results <- create_mock_results(include_ci = TRUE)

  coverage <- medsim_analyze_coverage(results, by_scenario = TRUE)

  expect_true("by_scenario" %in% names(coverage))
  expect_s3_class(coverage$by_scenario, "data.frame")
  expect_true("scenario" %in% names(coverage$by_scenario))
})

test_that("medsim_analyze_coverage handles missing values", {
  results <- create_mock_results(include_ci = TRUE)

  # Add some NAs
  results$results$indirect_ci_lower[1:3] <- NA

  coverage <- medsim_analyze_coverage(results)

  # Should still work
  expect_s3_class(coverage, "medsim_coverage")
  expect_true(coverage$coverage$n_valid < 20)
})

# Tests for medsim_analyze_power()
test_that("medsim_analyze_power computes power rates", {
  results <- create_mock_results(include_p = TRUE)

  power <- medsim_analyze_power(results, alpha = 0.05)

  expect_s3_class(power, "medsim_power")
  expect_s3_class(power, "list")

  # Check structure
  expect_true("power" %in% names(power))
  expect_true("summary" %in% names(power))

  # Check power data.frame
  expect_s3_class(power$power, "data.frame")
  expect_true("parameter" %in% names(power$power))
  expect_true("power" %in% names(power$power))
  expect_true("alpha" %in% names(power$power))

  # Power should be between 0 and 1
  expect_true(all(power$power$power >= 0))
  expect_true(all(power$power$power <= 1))
})

test_that("medsim_analyze_power errors without p-value columns", {
  results <- create_mock_results(include_p = FALSE)

  expect_error(
    medsim_analyze_power(results),
    "No p-value columns found"
  )
})

test_that("medsim_analyze_power computes by-scenario power", {
  results <- create_mock_results(include_p = TRUE)

  power <- medsim_analyze_power(results, by_scenario = TRUE)

  expect_true("by_scenario" %in% names(power))
  expect_s3_class(power$by_scenario, "data.frame")
  expect_true("scenario" %in% names(power$by_scenario))
})

test_that("medsim_analyze_power handles different alpha levels", {
  results <- create_mock_results(include_p = TRUE)

  power_05 <- medsim_analyze_power(results, alpha = 0.05)
  power_01 <- medsim_analyze_power(results, alpha = 0.01)

  # Different alphas should give different power
  expect_equal(power_05$alpha, 0.05)
  expect_equal(power_01$alpha, 0.01)

  # Power should be lower with stricter alpha (usually)
  # Note: This might not always be true with random data, so just check structure
  expect_type(power_05$power$power, "double")
  expect_type(power_01$power$power, "double")
})

# Tests for medsim_compare_methods()
test_that("medsim_compare_methods compares accuracy", {
  results1 <- create_mock_results()
  results2 <- create_mock_results()

  # Make results2 slightly different
  results2$results$indirect <- results2$results$indirect + 0.01

  comparison <- medsim_compare_methods(
    method1 = results1,
    method2 = results2,
    metrics = "accuracy"
  )

  expect_s3_class(comparison, "medsim_comparison")
  expect_true("accuracy_comparison" %in% names(comparison))
  expect_s3_class(comparison$accuracy_comparison, "data.frame")
  expect_true("method" %in% names(comparison$accuracy_comparison))

  # Should have 2 methods
  expect_equal(length(unique(comparison$accuracy_comparison$method)), 2)
})

test_that("medsim_compare_methods compares timing", {
  results1 <- create_mock_results()
  results2 <- create_mock_results()

  # Make results2 slower
  results2$results$elapsed <- results2$results$elapsed * 2

  comparison <- medsim_compare_methods(
    method1 = results1,
    method2 = results2,
    metrics = "timing"
  )

  expect_true("timing_comparison" %in% names(comparison))
  expect_s3_class(comparison$timing_comparison, "data.frame")
  expect_true("mean_time" %in% names(comparison$timing_comparison))
  expect_true("median_time" %in% names(comparison$timing_comparison))

  # Method2 should be slower
  method1_time <- comparison$timing_comparison$mean_time[comparison$timing_comparison$method == "method1"]
  method2_time <- comparison$timing_comparison$mean_time[comparison$timing_comparison$method == "method2"]
  expect_true(method2_time > method1_time)
})

test_that("medsim_compare_methods compares coverage", {
  results1 <- create_mock_results(include_ci = TRUE)
  results2 <- create_mock_results(include_ci = TRUE)

  comparison <- medsim_compare_methods(
    method1 = results1,
    method2 = results2,
    metrics = "coverage"
  )

  expect_true("coverage_comparison" %in% names(comparison))
  expect_s3_class(comparison$coverage_comparison, "data.frame")
  expect_true("coverage" %in% names(comparison$coverage_comparison))
})

test_that("medsim_compare_methods requires at least 2 methods", {
  results1 <- create_mock_results()

  expect_error(
    medsim_compare_methods(method1 = results1),
    "At least two medsim_results objects required"
  )
})

test_that("medsim_compare_methods requires named arguments", {
  results1 <- create_mock_results()
  results2 <- create_mock_results()

  expect_error(
    medsim_compare_methods(results1, results2),
    "All results must be named"
  )
})

test_that("medsim_compare_methods computes all metrics", {
  results1 <- create_mock_results(include_ci = TRUE)
  results2 <- create_mock_results(include_ci = TRUE)

  comparison <- medsim_compare_methods(
    method1 = results1,
    method2 = results2,
    metrics = "all"
  )

  expect_true("accuracy_comparison" %in% names(comparison))
  expect_true("timing_comparison" %in% names(comparison))
  expect_true("coverage_comparison" %in% names(comparison))
})

# Tests for print methods
test_that("print.medsim_analysis works", {
  results <- create_mock_results()
  analysis <- medsim_analyze(results)

  expect_output(print(analysis), "Simulation Analysis")
  expect_output(print(analysis), "Accuracy Metrics")
})

test_that("print.medsim_coverage works", {
  results <- create_mock_results(include_ci = TRUE)
  coverage <- medsim_analyze_coverage(results)

  expect_output(print(coverage), "Coverage Analysis")
  expect_output(print(coverage), "Coverage Rates")
})

test_that("print.medsim_power works", {
  results <- create_mock_results(include_p = TRUE)
  power <- medsim_analyze_power(results)

  expect_output(print(power), "Power Analysis")
  expect_output(print(power), "Power Rates")
})

test_that("print.medsim_comparison works", {
  results1 <- create_mock_results()
  results2 <- create_mock_results()

  comparison <- medsim_compare_methods(
    method1 = results1,
    method2 = results2
  )

  expect_output(print(comparison), "Method Comparison")
})
