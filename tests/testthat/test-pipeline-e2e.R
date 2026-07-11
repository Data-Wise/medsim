# Tier-A correctness guard T2 (plan.md): full-pipeline e2e + truth-attachment.
#
# Guards the chunk->combine->analyze seam where truth is joined to results by
# scenario. The prod3 coverage bug this session (compute_truth not forwarded by
# medsim_run_chunk) lived exactly here. A mis-join -- truth broadcast to the
# wrong scenario, or silently dropped -- would leave per-function unit tests
# green while every downstream coverage number is wrong.
#
# Two scenarios with DELIBERATELY DIFFERENT truths make the seam observable: if
# truth is attached correctly, both land ~0.95; if the truths are swapped
# (planted defect), coverage collapses, because a mean-zero sample's CI never
# contains 3. Self-contained analytic method (plan Decision 3).

.pe_method_nominal <- function(data, params) {
  n  <- nrow(data)
  m  <- mean(data$x)
  se <- stats::sd(data$x) / sqrt(n)
  list(theta_lower = m - 1.96 * se, theta_upper = m + 1.96 * se)
}

.pe_scenarios <- function() {
  mk <- function(name, theta) medsim_scenario(
    name = name,
    data_generator = local({
      th <- theta
      function(n = 200) data.frame(x = stats::rnorm(n, mean = th, sd = 1))
    }),
    params = list(theta_true = theta),
    estimand = medsim_estimand("interval", params = "theta", ci = "standard",
                               truth = function(s) c(theta = s$params$theta_true))
  )
  list(mk("mean_zero", 0.0), mk("mean_three", 3.0))
}

# Run the two-scenario study chunked, combine, attach truth via `truth_map`
# (name -> value), and return the by-scenario coverage as a named vector.
.pe_run <- function(truth_map, n_chunks = 2L, n_replications = 150L, n = 200L) {
  sc  <- .pe_scenarios()
  est <- medsim_estimand("interval", params = "theta", ci = "standard",
                         truth = function(s) c(theta = s$params$theta_true))
  out_dir <- withr::local_tempdir()

  for (cid in seq_len(n_chunks)) {
    cfg <- medsim_config("test", chunk_id = cid, n_chunks = n_chunks,
                         n_replications = n_replications, n_cores = 1L,
                         output_dir = out_dir)
    cfg$n <- n
    medsim_run_chunk(sc, .pe_method_nominal, cfg, verbose = FALSE)
  }

  combined <- medsim_combine_chunks(out_dir, verbose = FALSE)
  combined$truth <- data.frame(scenario = names(truth_map),
                               theta = unname(truth_map),
                               stringsAsFactors = FALSE)

  cov <- medsim_analyze_coverage(combined, estimand = est, by_scenario = TRUE)
  stats::setNames(cov$by_scenario$coverage, cov$by_scenario$scenario)
}

test_that("truth is attached per-scenario: both scenarios cover near-nominal", {
  cov <- .pe_run(c(mean_zero = 0.0, mean_three = 3.0))
  expect_gt(cov[["mean_zero"]],  0.88)
  expect_lt(cov[["mean_zero"]],  0.99)
  expect_gt(cov[["mean_three"]], 0.88)
  expect_lt(cov[["mean_three"]], 0.99)
})

test_that("combine preserves distinct per-scenario truth (both rows present, right names)", {
  cov <- .pe_run(c(mean_zero = 0.0, mean_three = 3.0))
  expect_setequal(names(cov), c("mean_zero", "mean_three"))
  expect_length(cov, 2L)
})

test_that("SWAPPED truth collapses coverage (planted defect proves attachment matters)", {
  # Give mean_zero the truth 3.0 and mean_three the truth 0.0. Each scenario's
  # CI (half-width ~0.14 at n=200) is nowhere near the other's mean, so a correct
  # instrument must report ~0 coverage. If truth were ignored / broadcast, this
  # would still read ~0.95 and the test would fail -- which is the guard.
  cov <- .pe_run(c(mean_zero = 3.0, mean_three = 0.0))
  expect_lt(cov[["mean_zero"]],  0.10)
  expect_lt(cov[["mean_three"]], 0.10)
})
