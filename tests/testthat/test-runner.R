# Tests for simulation runner functions (R/runner.R)

# ---- Helpers ----------------------------------------------------------------

make_scenario <- function(name = "Test Scenario") {
  medsim_scenario(
    name = name,
    data_generator = function(n = 100) {
      X <- rnorm(n)
      M <- 0.3 * X + rnorm(n)
      Y <- 0.3 * M + rnorm(n)
      data.frame(X = X, M = M, Y = Y)
    },
    params = list(a = 0.3, b = 0.3, indirect = 0.09)
  )
}

# Trivial method: returns named scalar
simple_method <- function(data, params) {
  list(indirect = mean(data$X * data$Y))
}

# Method returning multiple named values
multi_method <- function(data, params) {
  list(
    indirect = mean(data$X * data$Y),
    a_path   = cor(data$X, data$M)
  )
}

# Method that always errors
error_method <- function(data, params) {
  stop("deliberate error")
}

# Minimal fast config (5 reps, no parallel, temp output dir)
make_config <- function(out_dir) {
  cfg <- medsim_config("test")
  cfg$n_replications <- 5
  cfg$output_dir     <- out_dir
  cfg
}

# ---- medsim_run: happy path -------------------------------------------------

test_that("medsim_run returns medsim_results with expected fields", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_s3_class(res, "medsim_results")
  expect_s3_class(res, "list")
  expect_named(res, c("results", "summary", "truth", "config",
                      "scenarios", "method_name", "timestamp"),
               ignore.order = TRUE)
})

test_that("medsim_run results data.frame has correct row count", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_s3_class(res$results, "data.frame")
  expect_equal(nrow(res$results), cfg$n_replications)
})

test_that("medsim_run with multiple scenarios has rows for each", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario("S1"), make_scenario("S2"), make_scenario("S3"))

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_equal(nrow(res$results), 3 * cfg$n_replications)
  expect_equal(sort(unique(res$results$scenario)), c("S1", "S2", "S3"))
})

test_that("medsim_run result contains method output columns", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_true("indirect" %in% names(res$results))
  expect_type(res$results$indirect, "double")
})

test_that("medsim_run result contains elapsed timing column", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_true("elapsed" %in% names(res$results))
  expect_true(all(res$results$elapsed >= 0))
})

test_that("medsim_run multi-value method yields all columns", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(multi_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_true("indirect" %in% names(res$results))
  expect_true("a_path"   %in% names(res$results))
})

test_that("medsim_run result$truth is NULL when compute_truth omitted", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_null(res$truth)
})

test_that("medsim_run result$config equals input config", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_identical(res$config, cfg)
})

test_that("medsim_run result$scenarios equals input scenarios list", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_equal(length(res$scenarios), 1L)
  expect_identical(res$scenarios[[1]]$name, sc[[1]]$name)
})

test_that("medsim_run result$timestamp is a POSIXct", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_s3_class(res$timestamp, "POSIXct")
})

# ---- medsim_run: reproducibility --------------------------------------------

test_that("medsim_run is reproducible given same seed", {
  out1 <- withr::local_tempdir()
  out2 <- withr::local_tempdir()

  cfg1 <- make_config(out1); cfg1$seed <- 99L
  cfg2 <- make_config(out2); cfg2$seed <- 99L

  sc <- list(make_scenario())

  res1 <- suppressMessages(
    medsim_run(simple_method, sc, cfg1, parallel = FALSE, verbose = FALSE)
  )
  res2 <- suppressMessages(
    medsim_run(simple_method, sc, cfg2, parallel = FALSE, verbose = FALSE)
  )

  expect_equal(res1$results$indirect, res2$results$indirect)
})

# ---- medsim_run: input validation -------------------------------------------

test_that("medsim_run errors when method is not a function", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  expect_error(
    medsim_run("not_a_function", sc, cfg, parallel = FALSE, verbose = FALSE),
    "method must be a function"
  )
})

test_that("medsim_run errors when scenarios is not a list", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)

  expect_error(
    medsim_run(simple_method, "bad", cfg, parallel = FALSE, verbose = FALSE),
    "scenarios must be a list"
  )
})

test_that("medsim_run errors when config is not a medsim_config", {
  sc <- list(make_scenario())

  expect_error(
    medsim_run(simple_method, sc, list(), parallel = FALSE, verbose = FALSE),
    "medsim_config"
  )
})

# ---- medsim_run: error-method handling --------------------------------------

test_that("medsim_run captures method errors and completes", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  # Should warn (one per rep), not stop. Capture all warnings then check.
  res <- suppressMessages(suppressWarnings(
    medsim_run(error_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  ))

  # Results still returned; error column present
  expect_s3_class(res, "medsim_results")
  expect_equal(nrow(res$results), cfg$n_replications)
  expect_true("error" %in% names(res$results))
})

# ---- medsim_run: output files -----------------------------------------------

test_that("medsim_run writes CSV files to output_dir", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_true(file.exists(file.path(out, "all_results.csv")))
  expect_true(file.exists(file.path(out, "summary_stats.csv")))
  expect_true(file.exists(file.path(out, "results_scenario_1.csv")))
})

test_that("medsim_run writes truth.csv when compute_truth provided", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  truth_fn <- function(data, params) list(indirect = params$indirect)

  suppressMessages(
    medsim_run(simple_method, sc, cfg,
               compute_truth = truth_fn,
               parallel = FALSE, verbose = FALSE)
  )

  expect_true(file.exists(file.path(out, "truth.csv")))
})

# ---- medsim_run: compute_truth integration ----------------------------------

test_that("medsim_run populates result$truth when compute_truth provided", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  truth_fn <- function(data, params) list(indirect = params$indirect)

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg,
               compute_truth = truth_fn,
               parallel = FALSE, verbose = FALSE)
  )

  expect_s3_class(res$truth, "data.frame")
  expect_true("scenario" %in% names(res$truth))
})

# ---- medsim_run: verbose output ---------------------------------------------

test_that("medsim_run prints messages when verbose = TRUE", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  expect_output(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = TRUE),
    "Simulation"
  )
})

# ---- print/summary S3 methods -----------------------------------------------

test_that("print.medsim_results outputs key fields", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_output(print(res), "Simulation Results")
  expect_output(print(res), "Replications")
})

test_that("summary.medsim_results outputs summary statistics", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  res <- suppressMessages(
    medsim_run(simple_method, sc, cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_output(summary(res), "Summary Statistics")
})

# ---- medsim_run_single_replication ------------------------------------------

test_that("medsim_run_single_replication returns a data.frame", {
  sc  <- make_scenario()
  cfg <- medsim_config("test"); cfg$n_replications <- 5

  row <- medsim_run_single_replication(sc, rep_id = 1L, simple_method, cfg)

  expect_s3_class(row, "data.frame")
  expect_equal(nrow(row), 1L)
})

test_that("medsim_run_single_replication includes scenario, replication, elapsed", {
  sc  <- make_scenario()
  cfg <- medsim_config("test")

  row <- medsim_run_single_replication(sc, rep_id = 3L, simple_method, cfg)

  expect_true("scenario"    %in% names(row))
  expect_true("replication" %in% names(row))
  expect_true("elapsed"     %in% names(row))
  expect_equal(row$scenario,    sc$name)
  expect_equal(row$replication, 3L)
  expect_gte(row$elapsed, 0)
})

test_that("medsim_run_single_replication includes method output column", {
  sc  <- make_scenario()
  cfg <- medsim_config("test")

  row <- medsim_run_single_replication(sc, rep_id = 1L, simple_method, cfg)

  expect_true("indirect" %in% names(row))
})

test_that("medsim_run_single_replication captures errors into error column", {
  sc  <- make_scenario()
  cfg <- medsim_config("test")

  expect_warning(
    row <- medsim_run_single_replication(sc, rep_id = 1L, error_method, cfg),
    "Method failed"
  )

  expect_true("error" %in% names(row))
  expect_match(row$error, "deliberate error")
})

# ---- medsim_summarize_results -----------------------------------------------

test_that("medsim_summarize_results returns a data.frame", {
  results_df <- data.frame(
    scenario    = c("A", "A", "B", "B"),
    replication = 1:4,
    elapsed     = runif(4),
    indirect    = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )

  summ <- medsim_summarize_results(results_df)

  expect_s3_class(summ, "data.frame")
})

test_that("medsim_summarize_results groups by scenario", {
  results_df <- data.frame(
    scenario    = c("A", "A", "B", "B"),
    replication = 1:4,
    elapsed     = runif(4),
    indirect    = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )

  summ <- medsim_summarize_results(results_df, by = "scenario")

  expect_equal(nrow(summ), 2L)
  expect_true("scenario" %in% names(summ))
})

test_that("medsim_summarize_results produces expected statistic columns", {
  results_df <- data.frame(
    scenario    = rep("A", 10),
    replication = 1:10,
    elapsed     = runif(10),
    indirect    = rnorm(10, 0.09, 0.02),
    stringsAsFactors = FALSE
  )

  summ <- medsim_summarize_results(results_df)

  stat_cols <- c("indirect_mean", "indirect_median", "indirect_sd",
                 "indirect_min", "indirect_max")
  for (col in stat_cols) {
    expect_true(col %in% names(summ),
                label = paste("missing column:", col))
  }
})

test_that("medsim_summarize_results warns and returns empty df when no numeric cols", {
  results_df <- data.frame(
    scenario    = "A",
    replication = 1L,
    stringsAsFactors = FALSE
  )

  expect_warning(
    summ <- medsim_summarize_results(results_df),
    "No numeric columns"
  )
  expect_equal(nrow(summ), 0L)
})

# ---- medsim_compute_all_truth -----------------------------------------------

test_that("medsim_compute_all_truth returns a data.frame with one row per scenario", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario("S1"), make_scenario("S2"))

  truth_fn <- function(data, params) list(indirect = params$indirect)

  truth_df <- suppressMessages(
    medsim_compute_all_truth(sc, truth_function = truth_fn,
                             config = cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_s3_class(truth_df, "data.frame")
  expect_equal(nrow(truth_df), 2L)
  expect_true("scenario" %in% names(truth_df))
})

test_that("medsim_compute_all_truth includes truth value columns", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  truth_fn <- function(data, params) list(indirect = params$indirect)

  truth_df <- suppressMessages(
    medsim_compute_all_truth(sc, truth_function = truth_fn,
                             config = cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_true("indirect" %in% names(truth_df))
})

test_that("medsim_compute_all_truth caches result on disk", {
  out <- withr::local_tempdir()
  cfg <- make_config(out)
  sc  <- list(make_scenario())

  truth_fn <- function(data, params) list(indirect = params$indirect)

  suppressMessages(
    medsim_compute_all_truth(sc, truth_function = truth_fn,
                             config = cfg, parallel = FALSE, verbose = FALSE)
  )

  expect_true(file.exists(file.path(out, "truth_scenario_1.rds")))
})
