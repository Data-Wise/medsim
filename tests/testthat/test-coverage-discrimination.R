# Tier-A correctness guard T1 (plan.md): positive-control coverage discrimination.
#
# The bug that motivated the whole test-infrastructure plan (chunked-RNG collapse,
# PR #30) produced spurious coverage = 1.0. The generalisation: a coverage
# *instrument* that silently always "covers" is indistinguishable from a
# correct one unless something deliberately broken is fed through it and shown
# to undercover. This test IS that positive control, run through the full
# chunked path `medsim_run_chunk -> medsim_combine_chunks -> medsim_analyze_coverage`.
#
# Self-contained by design (plan Decision 3): a trivial analytic Wald interval
# for a normal mean -- NOT the expensive prod3 test-inversion kernel -- so it
# runs in well under a second under `R CMD check` and needs no external code.
#
# Must-fail property: the narrowed-CI control is a planted defect. If the
# instrument were broken (always covering), the control would read ~0.95 and
# this test would FAIL. That is the whole point.

# --- self-contained analytic fixtures -------------------------------------

.theta_true <- 0.2

# A scenario whose data_generator draws n iid N(theta_true, 1) observations.
.disc_scenario <- function() {
  medsim_scenario(
    name = "normal_mean",
    description = "iid N(theta_true, 1); estimand = the mean",
    data_generator = function(n = 200) data.frame(x = stats::rnorm(n, mean = .theta_true, sd = 1)),
    params = list(theta_true = .theta_true),
    estimand = medsim_estimand(
      "interval", params = "theta", ci = "standard",
      truth = function(s) c(theta = s$params$theta_true)
    )
  )
}

# Near-nominal method: standard 95% Wald interval for the mean -> ~0.95 coverage.
.method_nominal <- function(data, params) {
  n  <- nrow(data)
  m  <- mean(data$x)
  se <- stats::sd(data$x) / sqrt(n)
  list(theta_lower = m - 1.96 * se, theta_upper = m + 1.96 * se)
}

# Planted defect: same midpoint, interval narrowed to 1/3 width -> must undercover.
.method_narrow <- function(data, params) {
  ci   <- .method_nominal(data, params)
  mid  <- (ci$theta_lower + ci$theta_upper) / 2
  half <- (ci$theta_upper - ci$theta_lower) / 2 / 3
  list(theta_lower = mid - half, theta_upper = mid + half)
}

# Run a small chunked study end-to-end and return the scenario coverage.
# n_cores = 1 forces the sequential path: deterministic, CRAN-safe, and keeps
# this a *chunking* test (parallel FORK realism is Tier-B B4, not here).
.run_chunked_coverage <- function(method, n_chunks = 2L, n_replications = 200L, n = 200L) {
  sc  <- list(.disc_scenario())
  est <- medsim_estimand(
    "interval", params = "theta", ci = "standard",
    truth = function(s) c(theta = s$params$theta_true)
  )
  out_dir <- withr::local_tempdir()

  for (cid in seq_len(n_chunks)) {
    cfg <- medsim_config("test", chunk_id = cid, n_chunks = n_chunks,
                         n_replications = n_replications, n_cores = 1L,
                         output_dir = out_dir)
    cfg$n <- n
    medsim_run_chunk(sc, method, cfg, verbose = FALSE)
  }

  combined <- medsim_combine_chunks(out_dir, verbose = FALSE)
  # medsim_run_chunk() does not forward compute_truth, so attach analytic truth
  # post-combine (the documented pattern for chunked runs).
  combined$truth <- data.frame(scenario = "normal_mean", theta = .theta_true,
                               stringsAsFactors = FALSE)

  cov <- medsim_analyze_coverage(combined, estimand = est, by_scenario = TRUE)
  cov$by_scenario$coverage[cov$by_scenario$scenario == "normal_mean"]
}

# --- the discrimination assertions ----------------------------------------

test_that("coverage instrument reports near-nominal coverage for an honest Wald interval", {
  cov <- .run_chunked_coverage(.method_nominal)
  # ~0.95 with MCSE ~0.015 at 200 reps; a generous band that still excludes the
  # degenerate 1.0 the RNG bug produced and the ~0.49 a broken narrowing gives.
  expect_gt(cov, 0.88)
  expect_lt(cov, 0.99)
})

test_that("coverage instrument CATCHES undercoverage of a narrowed-CI control (must-fail guard)", {
  cov <- .run_chunked_coverage(.method_narrow)
  # A 1/3-width interval covers ~0.49 analytically. If the instrument were broken
  # (always covering), this would read ~0.95 and the assertion would fail --
  # which is exactly the regression this test exists to catch.
  expect_lt(cov, 0.80)
})

test_that("honest and narrowed methods are separated by a wide coverage margin", {
  cov_nominal <- .run_chunked_coverage(.method_nominal)
  cov_narrow  <- .run_chunked_coverage(.method_narrow)
  # Discrimination power: the good method must out-cover the broken one clearly.
  expect_gt(cov_nominal - cov_narrow, 0.20)
})
