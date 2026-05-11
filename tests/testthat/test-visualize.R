# Tests for visualization functions in R/visualize.R

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

make_mock_results <- function(n_reps = 20, seed = 123) {
  set.seed(seed)
  results_data <- data.frame(
    scenario  = rep("Test", n_reps),
    replication = seq_len(n_reps),
    indirect  = rnorm(n_reps, mean = 0.09, sd = 0.02),
    elapsed   = runif(n_reps, 0.01, 0.1),
    stringsAsFactors = FALSE
  )
  truth_data <- data.frame(
    scenario = "Test",
    indirect = 0.09,
    stringsAsFactors = FALSE
  )
  res <- list(
    results   = results_data,
    summary   = data.frame(scenario = "Test", indirect_mean = 0.09, stringsAsFactors = FALSE),
    truth     = truth_data,
    config    = medsim_config("test"),
    scenarios = list(),
    method_name = "mock_method",
    timestamp = Sys.time()
  )
  class(res) <- c("medsim_results", "list")
  res
}

make_mock_results_ci <- function(n_reps = 20, seed = 42) {
  res <- make_mock_results(n_reps = n_reps, seed = seed)
  res$results$indirect_ci_lower <- res$results$indirect - 0.04
  res$results$indirect_ci_upper <- res$results$indirect + 0.04
  res
}

make_mock_coverage <- function() {
  cov_df <- data.frame(
    parameter = "indirect",
    coverage  = 0.95,
    n_valid   = 20L,
    n_total   = 20L,
    stringsAsFactors = FALSE
  )
  by_scen <- data.frame(
    scenario  = "Test",
    parameter = "indirect",
    coverage  = 0.95,
    n_valid   = 20L,
    stringsAsFactors = FALSE
  )
  obj <- list(
    coverage   = cov_df,
    by_scenario = by_scen,
    summary    = "mock"
  )
  class(obj) <- c("medsim_coverage", "list")
  obj
}

# ---------------------------------------------------------------------------
# medsim_plot_error_boxplot — single method
# ---------------------------------------------------------------------------

test_that("medsim_plot_error_boxplot returns a ggplot for valid input", {
  skip_if_not_installed("ggplot2")
  res <- make_mock_results()
  p <- suppressWarnings(medsim_plot_error_boxplot(res))
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_error_boxplot uses custom title", {
  skip_if_not_installed("ggplot2")
  res <- make_mock_results()
  p <- suppressWarnings(medsim_plot_error_boxplot(res, title = "My Custom Title"))
  expect_equal(p$labels$title, "My Custom Title")
})

test_that("medsim_plot_error_boxplot default title is auto-generated", {
  skip_if_not_installed("ggplot2")
  res <- make_mock_results()
  p <- suppressWarnings(medsim_plot_error_boxplot(res))
  expect_false(is.null(p$labels$title))
  expect_true(nchar(p$labels$title) > 0)
})

test_that("medsim_plot_error_boxplot accepts log_scale = TRUE", {
  skip_if_not_installed("ggplot2")
  res <- make_mock_results()
  p <- suppressWarnings(medsim_plot_error_boxplot(res, log_scale = TRUE))
  expect_s3_class(p, "ggplot")
  # log scale label contains 'log'
  expect_match(p$labels$y, "log", ignore.case = TRUE)
})

test_that("medsim_plot_error_boxplot errors without ground truth", {
  res <- make_mock_results()
  res$truth <- NULL
  expect_error(
    medsim_plot_error_boxplot(res),
    "no ground truth"
  )
})

test_that("medsim_plot_error_boxplot errors for non-medsim_results input", {
  expect_error(
    medsim_plot_error_boxplot(list(x = 1)),
    "medsim_results"
  )
})

test_that("medsim_plot_error_boxplot accepts color_palette viridis", {
  skip_if_not_installed("ggplot2")
  res <- make_mock_results()
  p <- suppressWarnings(medsim_plot_error_boxplot(res, color_palette = "viridis"))
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_error_boxplot accepts parameter = NULL (all params)", {
  skip_if_not_installed("ggplot2")
  res <- make_mock_results()
  p <- suppressWarnings(medsim_plot_error_boxplot(res, parameter = NULL))
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# medsim_plot_error_boxplot — named list (multi-method)
# ---------------------------------------------------------------------------

test_that("medsim_plot_error_boxplot works with named list of results", {
  skip_if_not_installed("ggplot2")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_error_boxplot(list(Method1 = res1, Method2 = res2))
  )
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_error_boxplot errors on unnamed list", {
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  expect_error(
    medsim_plot_error_boxplot(list(res1, res2)),
    "must be named"
  )
})

test_that("medsim_plot_error_boxplot errors if list element not medsim_results", {
  res1 <- make_mock_results()
  expect_error(
    medsim_plot_error_boxplot(list(good = res1, bad = list(x = 1))),
    "medsim_results"
  )
})

# ---------------------------------------------------------------------------
# medsim_plot_timing
# ---------------------------------------------------------------------------

test_that("medsim_plot_timing returns a ggplot for named list input", {
  skip_if_not_installed("ggplot2")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_timing(list(MethodA = res1, MethodB = res2))
  )
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_timing uses custom title", {
  skip_if_not_installed("ggplot2")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_timing(list(A = res1, B = res2), title = "Timing Title")
  )
  expect_equal(p$labels$title, "Timing Title")
})

test_that("medsim_plot_timing errors on single medsim_results object", {
  res <- make_mock_results()
  expect_error(
    medsim_plot_timing(res),
    "named list"
  )
})

test_that("medsim_plot_timing errors on unnamed list", {
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  expect_error(
    medsim_plot_timing(list(res1, res2)),
    "named"
  )
})

test_that("medsim_plot_timing metric = 'median' works", {
  skip_if_not_installed("ggplot2")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_timing(list(A = res1, B = res2), metric = "median")
  )
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$y, "Median", ignore.case = TRUE)
})

test_that("medsim_plot_timing metric = 'total' works", {
  skip_if_not_installed("ggplot2")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_timing(list(A = res1, B = res2), metric = "total")
  )
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_timing errors on invalid metric", {
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  expect_error(
    medsim_plot_timing(list(A = res1, B = res2), metric = "nonsense"),
    "metric"
  )
})

test_that("medsim_plot_timing accepts log_scale = TRUE", {
  skip_if_not_installed("ggplot2")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_timing(list(A = res1, B = res2), log_scale = TRUE)
  )
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# medsim_plot_coverage — single medsim_coverage object
# ---------------------------------------------------------------------------

test_that("medsim_plot_coverage returns a ggplot for valid coverage object", {
  skip_if_not_installed("ggplot2")
  cov <- make_mock_coverage()
  p <- suppressWarnings(medsim_plot_coverage(cov))
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_coverage uses custom title", {
  skip_if_not_installed("ggplot2")
  cov <- make_mock_coverage()
  p <- suppressWarnings(medsim_plot_coverage(cov, title = "Coverage Plot"))
  expect_equal(p$labels$title, "Coverage Plot")
})

test_that("medsim_plot_coverage default title contains 'Coverage'", {
  skip_if_not_installed("ggplot2")
  cov <- make_mock_coverage()
  p <- suppressWarnings(medsim_plot_coverage(cov))
  expect_match(p$labels$title, "Coverage", ignore.case = TRUE)
})

test_that("medsim_plot_coverage accepts different expected_coverage", {
  skip_if_not_installed("ggplot2")
  cov <- make_mock_coverage()
  p <- suppressWarnings(medsim_plot_coverage(cov, expected_coverage = 0.90))
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_coverage accepts different tolerance", {
  skip_if_not_installed("ggplot2")
  cov <- make_mock_coverage()
  p <- suppressWarnings(medsim_plot_coverage(cov, tolerance = 0.05))
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_coverage errors on non-medsim_coverage input", {
  expect_error(
    medsim_plot_coverage(list(x = 1)),
    "medsim_coverage"
  )
})

test_that("medsim_plot_coverage works without by_scenario (uses coverage directly)", {
  skip_if_not_installed("ggplot2")
  cov <- make_mock_coverage()
  cov$by_scenario <- NULL  # force fallback path
  p <- suppressWarnings(medsim_plot_coverage(cov))
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# medsim_plot_coverage — named list of coverage objects
# ---------------------------------------------------------------------------

test_that("medsim_plot_coverage works with named list of coverage objects", {
  skip_if_not_installed("ggplot2")
  cov1 <- make_mock_coverage()
  cov2 <- make_mock_coverage()
  cov2$by_scenario$coverage <- 0.92
  p <- suppressWarnings(
    medsim_plot_coverage(list(MethodA = cov1, MethodB = cov2))
  )
  expect_s3_class(p, "ggplot")
})

test_that("medsim_plot_coverage errors on unnamed list of coverage objects", {
  cov1 <- make_mock_coverage()
  cov2 <- make_mock_coverage()
  expect_error(
    medsim_plot_coverage(list(cov1, cov2)),
    "named"
  )
})

test_that("medsim_plot_coverage errors if list element not medsim_coverage", {
  cov1 <- make_mock_coverage()
  expect_error(
    medsim_plot_coverage(list(good = cov1, bad = list(x = 1))),
    "medsim_coverage"
  )
})

# ---------------------------------------------------------------------------
# medsim_plot_combined_panel
# ---------------------------------------------------------------------------

test_that("medsim_plot_combined_panel returns an object with error panel", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  res <- make_mock_results()
  p <- suppressWarnings(
    medsim_plot_combined_panel(res, panels = "error")
  )
  expect_false(is.null(p))
})

test_that("medsim_plot_combined_panel errors without patchwork or gridExtra", {
  # This test is meaningful only when neither package is installed; skip otherwise.
  skip_if(
    requireNamespace("patchwork", quietly = TRUE) ||
      requireNamespace("gridExtra", quietly = TRUE),
    "patchwork or gridExtra is installed"
  )
  res <- make_mock_results()
  expect_error(
    medsim_plot_combined_panel(res, panels = "error"),
    "patchwork.*gridExtra|gridExtra.*patchwork"
  )
})

test_that("medsim_plot_combined_panel error+timing panels from named list", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_combined_panel(
      list(A = res1, B = res2),
      panels = c("error", "timing")
    )
  )
  expect_false(is.null(p))
})

test_that("medsim_plot_combined_panel warns when timing panel needs multiple methods", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  res <- make_mock_results()
  # Timing panel is skipped for single results — should warn and still return
  expect_warning(
    medsim_plot_combined_panel(res, panels = c("error", "timing")),
    "Timing"
  )
})

test_that("medsim_plot_combined_panel accepts explicit layout", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  res1 <- make_mock_results(seed = 1)
  res2 <- make_mock_results(seed = 2)
  p <- suppressWarnings(
    medsim_plot_combined_panel(
      list(A = res1, B = res2),
      panels = c("error", "timing"),
      layout = "1x2"
    )
  )
  expect_false(is.null(p))
})

test_that("medsim_plot_combined_panel errors when no plots generated", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  # coverage panel on a single results object with no CI will warn+skip;
  # timing panel also warns+skips; error panel should remain.
  res <- make_mock_results()
  # Ask only for timing (will be skipped) to trigger "no plots" error
  expect_error(
    suppressWarnings(
      medsim_plot_combined_panel(res, panels = "timing")
    ),
    "No plots could be generated"
  )
})
