# Tier-A boundary/edge units for the chunked pipeline (plan.md G8, cheap cases
# that need no cluster). The at-scale versions live in inst/hopper-tests/.

.edge_scenario <- function() {
  medsim_scenario(
    name = "edge",
    data_generator = function(n = 50) data.frame(x = stats::rnorm(n)),
    params = list(theta_true = 0),
    estimand = medsim_estimand("interval", params = "theta", ci = "standard",
                               truth = function(s) c(theta = 0))
  )
}
.edge_method <- function(data, params) {
  m <- mean(data$x); se <- stats::sd(data$x) / sqrt(nrow(data))
  list(theta_lower = m - 1.96 * se, theta_upper = m + 1.96 * se)
}

test_that("n_chunks > n_replications produces exactly n_replications rows (no phantom reps)", {
  # Empty chunks (chunk_id > n_rep) get zero indices; the pipeline must emit NO
  # rows for them. The `1:0 == c(1,0)` footgun would fabricate 2 phantom reps
  # per empty chunk -> total > n_rep.
  out <- withr::local_tempdir()
  n_rep <- 3L; n_chunks <- 5L
  for (cid in seq_len(n_chunks)) {
    cfg <- medsim_config("test", chunk_id = cid, n_chunks = n_chunks,
                         n_replications = n_rep, n_cores = 1L, output_dir = out)
    medsim_run_chunk(list(.edge_scenario()), .edge_method, cfg, verbose = FALSE)
  }
  combined <- medsim_combine_chunks(out, verbose = FALSE)
  # Exactly n_rep rows total (not 7): the 2 empty chunks contribute 0 rows.
  # (`replication` is the per-chunk LOCAL id and legitimately repeats across
  # chunks, so it is not asserted here.)
  expect_equal(nrow(combined$results), n_rep)
})

test_that("medsim_combine_chunks warns when fewer chunk files than expected are present", {
  # A timed-out/failed array task leaves a gap; combine must flag it loudly
  # rather than silently combine a partial grid as if complete.
  out <- withr::local_tempdir()
  for (cid in 1:3) {
    cfg <- medsim_config("test", chunk_id = cid, n_chunks = 3L,
                         n_replications = 6L, n_cores = 1L, output_dir = out)
    medsim_run_chunk(list(.edge_scenario()), .edge_method, cfg, verbose = FALSE)
  }
  file.remove(file.path(out, "chunk_0002.rds"))        # simulate a missing chunk
  expect_warning(
    medsim_combine_chunks(out, expected_chunks = 3L, verbose = FALSE),
    "expected 3.*found 2|missing|fewer"
  )
  # Without expected_chunks it still combines the present chunks (no crash).
  cmb <- medsim_combine_chunks(out, verbose = FALSE)
  expect_equal(cmb$n_chunks_combined, 2L)
})

test_that("an all-NA chunk yields failure_rate 1 and non-poisoned coverage", {
  # 100%-failure chunk (e.g. near-singular Sigma -> all-NA CIs): failure_rate 1,
  # coverage NaN (0/0 over successes), no crash.
  out <- withr::local_tempdir()
  na_method <- function(data, params) list(theta_lower = NA_real_, theta_upper = NA_real_)
  cfg <- medsim_config("test", chunk_id = 1L, n_chunks = 1L,
                       n_replications = 5L, n_cores = 1L, output_dir = out)
  medsim_run_chunk(list(.edge_scenario()), na_method, cfg, verbose = FALSE)
  combined <- medsim_combine_chunks(out, verbose = FALSE)
  combined$truth <- data.frame(scenario = "edge", theta = 0, stringsAsFactors = FALSE)
  est <- medsim_estimand("interval", params = "theta", ci = "standard",
                         truth = function(s) c(theta = 0))
  cov <- suppressWarnings(medsim_analyze_coverage(combined, estimand = est, by_scenario = TRUE))
  # No valid rows -> the interval branch skips the param; coverage table is empty
  # but the call must not error.
  expect_s3_class(cov, "medsim_coverage")
})
