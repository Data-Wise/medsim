# Tier-A correctness guard T4 (plan.md, gap G4): truth-cache invalidation.
#
# BUG (latent, this is a test-FIRST fix): medsim_compute_all_truth() keyed the
# truth cache on scenario *index* only (`truth_scenario_%d.rds`), with no content
# check and expiry off by default. Reusing an output_dir after changing a
# scenario's DGM/params silently reloaded STALE truth -> every downstream
# error/coverage number computed against the wrong ground truth.
#
# Fix: store a content fingerprint (serialized name+params+DGM+truth_fn, which
# captures closure values) alongside the cached truth; recompute on mismatch.
# Filename stays truth_scenario_%d.rds (existing tests depend on it).

test_that("changing a scenario's DGM invalidates cached truth (no stale reuse)", {
  out <- withr::local_tempdir()
  truth_fn <- function(data, params) list(mu = mean(data$x))
  # Same index (1), same name -> same cache file. Different DGM -> different truth.
  sc_A <- medsim_scenario(name = "s",
                          data_generator = function(n = 200) data.frame(x = stats::rnorm(n, 0)),
                          params = list())
  sc_B <- medsim_scenario(name = "s",
                          data_generator = function(n = 200) data.frame(x = stats::rnorm(n, 5)),
                          params = list())
  cfg <- medsim_config("test", output_dir = out)  # n_truth default 10000 -> precise

  t_A <- medsim_compute_all_truth(list(sc_A), truth_fn, cfg, verbose = FALSE)
  t_B <- medsim_compute_all_truth(list(sc_B), truth_fn, cfg, verbose = FALSE)

  expect_lt(abs(t_A$mu - 0), 0.3)
  # On the BUGGY code this returns ~0 (stale sc_A truth reloaded) and fails.
  expect_lt(abs(t_B$mu - 5), 0.3)
})

test_that("identical scenario reuses cached truth (fix does not over-invalidate)", {
  out <- withr::local_tempdir()
  truth_fn <- function(data, params) list(mu = mean(data$x))
  sc <- medsim_scenario(name = "s",
                        data_generator = function(n = 200) data.frame(x = stats::rnorm(n, 2)),
                        params = list())
  cfg <- medsim_config("test", output_dir = out)

  t1 <- medsim_compute_all_truth(list(sc), truth_fn, cfg, verbose = FALSE)
  # Second call: identical scenario -> cache hit -> byte-identical truth returned
  # (a recompute would draw a fresh 10000-sample and differ in the ~4th decimal).
  t2 <- medsim_compute_all_truth(list(sc), truth_fn, cfg, verbose = FALSE)
  expect_identical(t1$mu, t2$mu)
})
