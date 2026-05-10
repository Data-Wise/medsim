# Tests for umbrella workflow functions (medsim_workflow, medsim_figures, medsim_tables)

mock_workflow_results <- function() {
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
  config <- medsim_config("test")
  config$n_replications <- 20

  set.seed(42)
  results_data <- data.frame(
    scenario = rep("Test", 20),
    replication = 1:20,
    indirect = rnorm(20, mean = 0.09, sd = 0.02),
    indirect_ci_lower = NA_real_,
    indirect_ci_upper = NA_real_,
    elapsed = runif(20, 0.01, 0.1),
    stringsAsFactors = FALSE
  )
  results_data$indirect_ci_lower <- results_data$indirect - 1.96 * 0.02
  results_data$indirect_ci_upper <- results_data$indirect + 1.96 * 0.02

  results <- list(
    results = results_data,
    summary = data.frame(
      scenario = "Test",
      indirect_mean = mean(results_data$indirect),
      indirect_sd = sd(results_data$indirect),
      stringsAsFactors = FALSE
    ),
    truth = data.frame(scenario = "Test", indirect = 0.09, stringsAsFactors = FALSE),
    config = config,
    scenarios = list(scenario),
    method_name = "mock_method",
    timestamp = Sys.time()
  )
  class(results) <- c("medsim_results", "list")
  results
}

test_that("medsim_figures writes expected files to output_dir", {
  skip_if_not_installed("ggplot2")
  results <- mock_workflow_results()
  out <- withr::local_tempdir()

  paths <- medsim_figures(results, output_dir = out, format = "pdf")

  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(out, "error_boxplot.pdf")))
  expect_true(file.exists(file.path(out, "timing.pdf")))
  expect_named(paths)
})

test_that("medsim_figures creates output_dir if missing", {
  skip_if_not_installed("ggplot2")
  results <- mock_workflow_results()
  out <- file.path(withr::local_tempdir(), "nested", "deep")

  expect_false(dir.exists(out))
  medsim_figures(results, output_dir = out, format = "pdf")
  expect_true(dir.exists(out))
})

test_that("medsim_tables delegates to medsim_tables_workflow", {
  results <- mock_workflow_results()
  out <- withr::local_tempdir()

  expect_no_error(medsim_tables(results, output_dir = out, format = "latex"))
  expect_true(dir.exists(out))
})

test_that("medsim_workflow runs the full output pipeline", {
  skip_if_not_installed("ggplot2")
  results <- mock_workflow_results()
  out <- withr::local_tempdir()

  ret <- medsim_workflow(results, output_dir = out)

  expect_true(dir.exists(file.path(out, "figures")))
  expect_true(dir.exists(file.path(out, "tables")))
  expect_named(ret, c("analysis", "figures", "tables"))
  expect_s3_class(ret$analysis, "medsim_analysis")
})
