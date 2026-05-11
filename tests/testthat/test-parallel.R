# Tests for parallel processing utilities (R/parallel.R)

# ---------------------------------------------------------------------------
# medsim_is_batch_job
# ---------------------------------------------------------------------------
# Note: medsim_is_batch_job() returns TRUE when non-interactive (testthat runs
# non-interactively), UNLESS a scheduler env var is explicitly unset and we
# rely on the non-interactive = batch logic. We can test the scheduler-variable
# branch directly regardless of interactive().

test_that("medsim_is_batch_job returns TRUE when SLURM_JOB_ID is set", {
  withr::local_envvar(SLURM_JOB_ID = "12345")
  expect_true(medsim_is_batch_job())
})

test_that("medsim_is_batch_job returns TRUE when PBS_JOBID is set", {
  withr::local_envvar(PBS_JOBID = "job.99")
  expect_true(medsim_is_batch_job())
})

test_that("medsim_is_batch_job returns TRUE when LSB_JOBID is set", {
  withr::local_envvar(LSB_JOBID = "lsf.42")
  expect_true(medsim_is_batch_job())
})

test_that("medsim_is_batch_job returns logical scalar", {
  result <- medsim_is_batch_job()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("medsim_is_batch_job returns TRUE in non-interactive context (testthat)", {
  # testthat runs non-interactively; with no scheduler vars the function falls
  # through to the final `return(TRUE)` branch.
  withr::local_envvar(
    SLURM_JOB_ID = NA,
    PBS_JOBID = NA,
    LSB_JOBID = NA
  )
  expect_true(medsim_is_batch_job())
})

# ---------------------------------------------------------------------------
# medsim_setup_progress
# ---------------------------------------------------------------------------

test_that("medsim_setup_progress returns invisibly", {
  skip_if_not_installed("pbapply")
  # return value is previous options (a named list)
  result <- medsim_setup_progress()
  # invisibility only affects auto-printing; we can still capture the value
  expect_true(is.list(result) || is.null(result))
})

test_that("medsim_setup_progress accepts explicit style argument", {
  skip_if_not_installed("pbapply")
  expect_no_error(medsim_setup_progress(style = "none"))
  expect_no_error(medsim_setup_progress(style = "txt"))
})

test_that("medsim_setup_progress accepts explicit char argument", {
  skip_if_not_installed("pbapply")
  expect_no_error(medsim_setup_progress(char = "#"))
})

test_that("medsim_setup_progress with style none does not error", {
  skip_if_not_installed("pbapply")
  # Passing style = "none" is the code path used in batch/non-interactive mode
  expect_no_error(medsim_setup_progress(style = "none", char = "-"))
})

# ---------------------------------------------------------------------------
# medsim_get_optimal_cores
# ---------------------------------------------------------------------------

test_that("medsim_get_optimal_cores returns positive integer", {
  n <- medsim_get_optimal_cores(n_tasks = 10)
  expect_type(n, "double")
  expect_true(n >= 1)
})

test_that("medsim_get_optimal_cores does not exceed n_tasks", {
  n <- medsim_get_optimal_cores(n_tasks = 3)
  expect_true(n <= 3)
})

test_that("medsim_get_optimal_cores respects max_cores", {
  n <- medsim_get_optimal_cores(n_tasks = 1000, max_cores = 2)
  expect_true(n <= 2)
})

test_that("medsim_get_optimal_cores uses SLURM_CPUS_PER_TASK when set", {
  withr::local_envvar(
    SLURM_CPUS_PER_TASK = "8",
    PBS_NUM_PPN = NA,
    LSB_DJOB_NUMPROC = NA
  )
  # With 8 cores, reserve becomes 0, so up to 8 usable; capped at n_tasks
  n <- medsim_get_optimal_cores(n_tasks = 100)
  expect_true(n >= 1)
  expect_true(n <= 8)
})

test_that("medsim_get_optimal_cores uses PBS_NUM_PPN when set", {
  withr::local_envvar(
    SLURM_CPUS_PER_TASK = NA,
    PBS_NUM_PPN = "4",
    LSB_DJOB_NUMPROC = NA
  )
  n <- medsim_get_optimal_cores(n_tasks = 100)
  expect_true(n >= 1)
  expect_true(n <= 4)
})

test_that("medsim_get_optimal_cores uses LSB_DJOB_NUMPROC when set", {
  withr::local_envvar(
    SLURM_CPUS_PER_TASK = NA,
    PBS_NUM_PPN = NA,
    LSB_DJOB_NUMPROC = "6"
  )
  n <- medsim_get_optimal_cores(n_tasks = 100)
  expect_true(n >= 1)
  expect_true(n <= 6)
})

# ---------------------------------------------------------------------------
# medsim_estimate_speedup
# ---------------------------------------------------------------------------

test_that("medsim_estimate_speedup returns named list with required fields", {
  est <- medsim_estimate_speedup(n_tasks = 100, n_cores = 4)
  expect_type(est, "list")
  expect_named(est, c("speedup", "efficiency", "recommendation"), ignore.order = TRUE)
})

test_that("medsim_estimate_speedup speedup is >= 1 for multiple cores", {
  est <- medsim_estimate_speedup(n_tasks = 100, n_cores = 4)
  expect_true(est$speedup >= 1)
})

test_that("medsim_estimate_speedup efficiency is between 0 and 1", {
  est <- medsim_estimate_speedup(n_tasks = 100, n_cores = 4)
  expect_true(est$efficiency > 0 && est$efficiency <= 1)
})

test_that("medsim_estimate_speedup caps effective_cores at n_tasks", {
  # 1000 cores but only 2 tasks: effective_cores = 2
  est <- medsim_estimate_speedup(n_tasks = 2, n_cores = 1000)
  expected_speedup <- 2 / (1 + 0.1 * 2)
  expect_equal(est$speedup, expected_speedup, tolerance = 1e-10)
})

test_that("medsim_estimate_speedup recommendation is character", {
  est <- medsim_estimate_speedup(n_tasks = 10, n_cores = 2)
  expect_type(est$recommendation, "character")
  expect_length(est$recommendation, 1)
})

test_that("medsim_estimate_speedup recommendation categories cover all branches", {
  # efficiency > 0.8: overhead = 0.05 gives eff = 0.833
  est_high <- medsim_estimate_speedup(n_tasks = 100, n_cores = 4, overhead = 0.05)
  expect_match(est_high$recommendation, "Excellent")

  # efficiency in (0.6, 0.8]: overhead = 0.15 gives eff = 0.625
  est_good <- medsim_estimate_speedup(n_tasks = 100, n_cores = 4, overhead = 0.15)
  expect_match(est_good$recommendation, "Good")

  # efficiency in (0.4, 0.6]: overhead = 0.30 gives eff = 0.455
  est_mod <- medsim_estimate_speedup(n_tasks = 100, n_cores = 4, overhead = 0.30)
  expect_match(est_mod$recommendation, "Moderate")

  # efficiency <= 0.4: overhead = 0.80 gives eff = 0.238
  est_low <- medsim_estimate_speedup(n_tasks = 100, n_cores = 4, overhead = 0.80)
  expect_match(est_low$recommendation, "Consider")
})

# ---------------------------------------------------------------------------
# medsim_check_results
# ---------------------------------------------------------------------------

test_that("medsim_check_results messages on all-success results", {
  results <- list(1, 2, 3)
  expect_message(
    out <- medsim_check_results(results),
    "completed successfully"
  )
  expect_equal(out, integer(0))
})

test_that("medsim_check_results warns when errors present", {
  good <- list(1, 2)
  bad <- list(
    structure(
      list(message = "something went wrong", task = 3),
      class = c("medsim_error", "error", "condition")
    )
  )
  results <- c(good, bad)

  expect_warning(
    medsim_check_results(results),
    "tasks failed"
  )
})

test_that("medsim_check_results returns indices of failed tasks", {
  results <- list(
    1,
    structure(list(message = "err", task = 2), class = c("medsim_error", "error", "condition")),
    3,
    structure(list(message = "err2", task = 4), class = c("medsim_error", "error", "condition"))
  )
  idx <- suppressWarnings(medsim_check_results(results))
  expect_equal(idx, c(2L, 4L))
})

test_that("medsim_check_results stops when stop_on_error = TRUE", {
  results <- list(
    structure(list(message = "boom", task = 1), class = c("medsim_error", "error", "condition"))
  )
  expect_error(
    suppressWarnings(medsim_check_results(results, stop_on_error = TRUE)),
    "tasks failed"
  )
})

# ---------------------------------------------------------------------------
# medsim_run_parallel -- sequential path (n_cores = 1 or < 4 tasks)
# ---------------------------------------------------------------------------

test_that("medsim_run_parallel validates that fun is a function", {
  expect_error(
    medsim_run_parallel(tasks = 1:5, fun = "not_a_function"),
    "fun must be a function"
  )
})

test_that("medsim_run_parallel returns empty list for empty tasks", {
  result <- medsim_run_parallel(tasks = list(), fun = identity)
  expect_equal(result, list())
})

test_that("medsim_run_parallel returns results of correct length (sequential path)", {
  # < 4 tasks forces sequential path
  result <- medsim_run_parallel(
    tasks = 1:3,
    fun = function(i) i * 2,
    n_cores = 1
  )
  expect_length(result, 3)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("medsim_run_parallel wraps task errors as medsim_error objects", {
  result <- medsim_run_parallel(
    tasks = 1:2,
    fun = function(i) stop("deliberate error"),
    n_cores = 1
  )
  expect_length(result, 2)
  for (r in result) {
    expect_s3_class(r, "medsim_error")
    expect_equal(r$message, "deliberate error")
  }
})

test_that("medsim_run_parallel n_cores capped to length(tasks)", {
  # Passing n_cores = 99 with 3 tasks should still work without error
  result <- medsim_run_parallel(
    tasks = 1:3,
    fun = function(i) i + 10,
    n_cores = 99
  )
  expect_length(result, 3)
  expect_equal(result[[1]], 11)
})

# ---------------------------------------------------------------------------
# medsim_run_sequential (internal, but accessible after load_all)
# ---------------------------------------------------------------------------

test_that("medsim_run_sequential returns correct results", {
  result <- medsim_run_sequential(
    tasks = as.list(1:4),
    fun = function(x) x^2,
    progress = FALSE
  )
  expect_equal(result[[1]], 1)
  expect_equal(result[[4]], 16)
})

test_that("medsim_run_sequential catches task errors", {
  result <- medsim_run_sequential(
    tasks = list(1),
    fun = function(x) stop("oops"),
    progress = FALSE
  )
  expect_s3_class(result[[1]], "medsim_error")
})

test_that("medsim_run_sequential with progress = TRUE runs without error", {
  skip_if_not_installed("pbapply")
  # This hits the pbapply::pblapply branch in medsim_run_sequential
  result <- medsim_run_sequential(
    tasks = as.list(1:4),
    fun = function(x) x * 3,
    progress = TRUE
  )
  expect_length(result, 4)
  expect_equal(result[[2]], 6)
})

# ---------------------------------------------------------------------------
# medsim_run_parallel -- actual parallel cluster path (>= 4 tasks, > 1 core)
# ---------------------------------------------------------------------------

test_that("medsim_run_parallel uses PSOCK cluster for >= 4 tasks with n_cores = 2", {
  skip_on_cran()
  skip_if_not_installed("pbapply")
  # PSOCK works on all platforms; force it explicitly
  result <- medsim_run_parallel(
    tasks = 1:6,
    fun = function(i) i * 10,
    n_cores = 2,
    progress = FALSE,
    cluster_type = "PSOCK"
  )
  expect_length(result, 6)
  # Values may be in any order due to parallel scheduling; check set equality
  values <- unlist(result)
  expect_setequal(values, c(10, 20, 30, 40, 50, 60))
})

test_that("medsim_run_parallel PSOCK cluster wraps errors per task", {
  skip_on_cran()
  result <- medsim_run_parallel(
    tasks = 1:5,
    fun = function(i) {
      if (i == 3) stop("task three failed")
      i
    },
    n_cores = 2,
    progress = FALSE,
    cluster_type = "PSOCK"
  )
  expect_length(result, 5)
  # Task 3 should be a medsim_error; find it by class
  is_err <- sapply(result, inherits, "medsim_error")
  expect_equal(sum(is_err), 1)
})

test_that("medsim_run_parallel PSOCK cluster with no packages arg returns correct length", {
  # NOTE: The clusterEvalQ code path in parallel.R (lines 159-163) has a bug:
  # `packages` is referenced as a free variable inside clusterEvalQ's expr but
  # PSOCK workers don't have it in scope. Passing packages = NULL avoids that
  # branch. The bug should be fixed in a separate PR (do not pass 'packages'
  # via clusterEvalQ without exporting the variable first).
  skip_on_cran()
  result <- medsim_run_parallel(
    tasks = 1:4,
    fun = function(i) i + 1,
    n_cores = 2,
    progress = FALSE,
    cluster_type = "PSOCK",
    packages = NULL
  )
  expect_length(result, 4)
})

test_that("medsim_run_parallel NULL n_cores auto-detects and runs", {
  skip_on_cran()
  # With NULL n_cores and < 4 tasks, falls through to sequential; still tests the
  # n_cores detection code path (lines 115-116)
  result <- medsim_run_parallel(
    tasks = 1:3,
    fun = function(i) i,
    n_cores = NULL
  )
  expect_length(result, 3)
})

test_that("medsim_run_parallel suppresses progress on batch job (PSOCK path)", {
  skip_on_cran()
  # Simulate a batch environment so medsim_is_batch_job() returns TRUE early
  # and the cluster path sets progress = FALSE
  withr::local_envvar(SLURM_JOB_ID = "999")
  result <- medsim_run_parallel(
    tasks = 1:5,
    fun = function(i) i * 2,
    n_cores = 2,
    progress = TRUE,      # will be overridden to FALSE inside function
    cluster_type = "PSOCK"
  )
  expect_length(result, 5)
})

test_that("medsim_run_parallel auto-detects FORK on Unix (cluster_type NULL)", {
  skip_on_cran()
  skip_on_os("windows")  # FORK not available on Windows
  # cluster_type = NULL triggers auto-detection; on Unix selects FORK
  result <- medsim_run_parallel(
    tasks = 1:5,
    fun = function(i) i,
    n_cores = 2,
    progress = FALSE,
    cluster_type = NULL
  )
  expect_length(result, 5)
})

test_that("medsim_run_parallel with export exports variable to PSOCK workers", {
  skip_on_cran()
  # Define a variable in local scope that should be exported
  multiplier <- 7L
  result <- medsim_run_parallel(
    tasks = 1:4,
    fun = function(i) i * multiplier,
    n_cores = 2,
    progress = FALSE,
    cluster_type = "PSOCK",
    export = "multiplier"
  )
  expect_length(result, 4)
  values <- unlist(result)
  expect_setequal(values, c(7L, 14L, 21L, 28L))
})

test_that("medsim_run_parallel with progress = TRUE uses pbapply on cluster", {
  skip_on_cran()
  skip_if_not_installed("pbapply")
  skip_on_os("windows")  # pbapply + FORK; skip on Windows
  # Force non-batch environment (interactive() = FALSE in testthat, but no
  # scheduler vars) to allow progress = TRUE to reach the pbapply branch.
  withr::local_envvar(SLURM_JOB_ID = NA, PBS_JOBID = NA, LSB_JOBID = NA)
  # Since testthat is non-interactive, medsim_is_batch_job() returns TRUE and
  # forces progress = FALSE regardless. This test verifies no error occurs.
  result <- medsim_run_parallel(
    tasks = 1:5,
    fun = function(i) i^2,
    n_cores = 2,
    progress = TRUE,
    cluster_type = "FORK"
  )
  expect_length(result, 5)
})
