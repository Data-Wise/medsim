# Tests for the interval-kind branch of medsim_analyze_coverage()
# and the DM scenario/method machinery.

# ── helpers ──────────────────────────────────────────────────────────────────

# Build a minimal medsim_results object without running medsim_run()
make_interval_medsim_results <- function(n_rep      = 20,
                                         nde_truth  = 0.2,
                                         nie_truth  = 0.3,
                                         hit_nde    = TRUE,
                                         hit_nie    = TRUE,
                                         scenario   = "dm_test") {
  nde_lower <- if (hit_nde) nde_truth - 0.1 else nde_truth + 0.1
  nde_upper <- if (hit_nde) nde_truth + 0.1 else nde_truth + 0.2
  nie_lower <- if (hit_nie) nie_truth - 0.1 else nie_truth + 0.1
  nie_upper <- if (hit_nie) nie_truth + 0.1 else nie_truth + 0.2

  results_df <- data.frame(
    scenario      = scenario,
    rep           = seq_len(n_rep),
    NDE_lower     = nde_lower,
    NDE_upper     = nde_upper,
    NDE_im_lower  = nde_lower - 0.05,
    NDE_im_upper  = nde_upper + 0.05,
    NIE_lower     = nie_lower,
    NIE_upper     = nie_upper,
    NIE_im_lower  = nie_lower - 0.05,
    NIE_im_upper  = nie_upper + 0.05,
    feasible      = TRUE,
    falsified     = FALSE,
    stringsAsFactors = FALSE
  )

  truth_df <- data.frame(
    scenario = scenario,
    NDE      = nde_truth,
    NIE      = nie_truth,
    stringsAsFactors = FALSE
  )

  r <- list(
    results     = results_df,
    truth       = truth_df,
    config      = list(),
    scenarios   = list(),
    method_name = "test_bounds",
    timestamp   = Sys.time()
  )
  class(r) <- c("medsim_results", "list")
  r
}

make_dm_estimand <- function() {
  medsim_estimand(
    "interval",
    params = c("NDE", "NIE"),
    ci     = "imbens_manski",
    extra  = c("feasible", "falsified"),
    truth  = function(s) c(NDE = s$params$NDE, NIE = s$params$NIE)
  )
}

# Extract scalar from coverage long-form: coverage$coverage[coverage$parameter == param]
cov_val <- function(out, param, col = "coverage") {
  out$coverage[[col]][out$coverage$parameter == param]
}

# ── medsim_analyze_coverage() dispatches to interval branch ──────────────────

test_that("medsim_analyze_coverage() with interval estimand returns medsim_coverage", {
  est <- make_dm_estimand()
  res <- make_interval_medsim_results()
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = FALSE)
  expect_s3_class(out, "medsim_coverage")
})

test_that("interval coverage = 1 when truth is inside all bounds", {
  est <- make_dm_estimand()
  res <- make_interval_medsim_results(hit_nde = TRUE, hit_nie = TRUE)
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = FALSE)
  expect_equal(cov_val(out, "NDE"), 1.0)
  expect_equal(cov_val(out, "NIE"), 1.0)
})

test_that("interval coverage = 0 when truth is outside all bounds", {
  est <- make_dm_estimand()
  res <- make_interval_medsim_results(hit_nde = FALSE, hit_nie = FALSE)
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = FALSE)
  expect_equal(cov_val(out, "NDE"), 0.0)
  expect_equal(cov_val(out, "NIE"), 0.0)
})

test_that("interval coverage has mean_width column", {
  est <- make_dm_estimand()
  res <- make_interval_medsim_results()
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = FALSE)
  expect_true("mean_width" %in% names(out$coverage))
  expect_equal(cov_val(out, "NDE", "mean_width"), 0.2)  # 0.1 - (-0.1) = 0.2
})

test_that("interval coverage has im_coverage when im columns present", {
  est <- make_dm_estimand()
  res <- make_interval_medsim_results()
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = FALSE)
  expect_true("im_coverage" %in% names(out$coverage))
  nde_im <- cov_val(out, "NDE", "im_coverage")
  expect_equal(nde_im, 1.0)
})

test_that("interval coverage has feasible_rate and falsified_rate in $extra", {
  est <- make_dm_estimand()
  res <- make_interval_medsim_results()
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = FALSE)
  expect_false(is.null(out$extra))
  expect_true("feasible_rate"  %in% names(out$extra))
  expect_true("falsified_rate" %in% names(out$extra))
  expect_equal(out$extra$feasible_rate,  1.0)
  expect_equal(out$extra$falsified_rate, 0.0)
})

test_that("interval result object carries estimand_kind = 'interval'", {
  est <- make_dm_estimand()
  res <- make_interval_medsim_results()
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = FALSE)
  expect_equal(out$estimand_kind, "interval")
})

test_that("interval coverage $by_scenario groups by scenario", {
  est <- make_dm_estimand()
  r1  <- make_interval_medsim_results(n_rep = 10, scenario = "sc1")
  r2  <- make_interval_medsim_results(n_rep = 10, hit_nde = FALSE, scenario = "sc2")
  # Combine into one medsim_results with two scenarios
  res <- r1
  res$results <- rbind(r1$results, r2$results)
  res$truth   <- rbind(r1$truth, r2$truth)
  out <- medsim_analyze_coverage(res, estimand = est, by_scenario = TRUE)
  expect_false(is.null(out$by_scenario))
  # 2 scenarios x 2 params = 4 rows in by_scenario
  expect_equal(nrow(out$by_scenario), 4L)
  expect_true("scenario" %in% names(out$by_scenario))
})

# ── medsim_analyze_coverage() still works for NULL estimand (point path) ─────

test_that("medsim_analyze_coverage() NULL estimand still uses point branch", {
  results_df <- data.frame(
    scenario          = "base",
    rep               = 1:20,
    indirect          = rnorm(20, 0.3, 0.05),
    indirect_ci_lower = rnorm(20, 0.1, 0.02),
    indirect_ci_upper = rnorm(20, 0.5, 0.02),
    stringsAsFactors  = FALSE
  )
  truth_df <- data.frame(
    scenario = "base",
    indirect = 0.3,
    stringsAsFactors = FALSE
  )
  res <- list(results = results_df, truth = truth_df,
              config = list(), scenarios = list(),
              method_name = "test", timestamp = Sys.time())
  class(res) <- c("medsim_results", "list")
  out <- medsim_analyze_coverage(res, estimand = NULL)
  # Should NOT have estimand_kind = "interval"
  expect_false(isTRUE(out$estimand_kind == "interval"))
})

# ── medsim_scenario_dm() ──────────────────────────────────────────────────────

test_that("medsim_scenario_dm() creates a medsim_scenario with interval estimand", {
  sc <- medsim_scenario_dm(
    name        = "dm_small",
    true_params = list(NDE = 0.2, NIE = 0.3),
    dm_params   = list(delta = 0.1),
    misclass_type = "mediator"
  )
  expect_s3_class(sc, "medsim_scenario")
  expect_s3_class(sc$estimand, "medsim_estimand")
  expect_equal(sc$estimand$kind, "interval")
  expect_equal(sc$estimand$ci, "imbens_manski")
})

test_that("medsim_scenario_dm() rejects missing NDE/NIE in true_params", {
  expect_error(
    medsim_scenario_dm("bad", true_params = list(n = 100)),
    "NDE.*NIE"
  )
})

test_that("medsim_scenario_dm() data_generator(n) uses fallback for NDE/NIE params", {
  sc <- medsim_scenario_dm(
    name        = "dm_gen",
    true_params = list(NDE = 0.2, NIE = 0.3),
    dm_params   = list(delta = 0.1)
  )
  set.seed(42)
  d <- sc$data_generator(50L)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 50L)
  expect_true(all(c("A", "M", "Y", "A_star") %in% names(d)))
})

test_that("medsim_scenario_dm() exposure misclass_type accepted", {
  expect_no_error(
    medsim_scenario_dm(
      name          = "dm_exp",
      true_params   = list(NDE = 0.1, NIE = 0.2),
      dm_params     = list(delta = 0.05),
      misclass_type = "exposure"
    )
  )
})

# ── medsim_method_bounds() ────────────────────────────────────────────────────

test_that("medsim_method_bounds() returns required columns", {
  set.seed(1)
  d <- data.frame(A = rbinom(100, 1L, 0.5), M = rnorm(100), Y = rnorm(100))
  res <- medsim_method_bounds(d, list(NDE = 0.2, NIE = 0.3))
  required <- c("NDE_lower", "NDE_upper", "NDE_im_lower", "NDE_im_upper",
                "NIE_lower", "NIE_upper", "NIE_im_lower", "NIE_im_upper",
                "feasible", "falsified")
  expect_true(all(required %in% names(res)))
})

test_that("medsim_method_bounds(): NDE_lower < NDE_upper", {
  set.seed(2)
  d <- data.frame(A = rbinom(200, 1L, 0.5), M = rnorm(200), Y = rnorm(200))
  res <- medsim_method_bounds(d, list(NDE = 0.2, NIE = 0.3))
  expect_lt(res$NDE_lower, res$NDE_upper)
  expect_lt(res$NIE_lower, res$NIE_upper)
  expect_lt(res$NDE_im_lower, res$NDE_im_upper)
})

test_that("medsim_method_bounds(): feasible = TRUE, falsified = FALSE", {
  set.seed(3)
  d <- data.frame(A = rbinom(100, 1L, 0.5), M = rnorm(100), Y = rnorm(100))
  res <- medsim_method_bounds(d, list(NDE = 0.1, NIE = 0.2))
  expect_true(res$feasible)
  expect_false(res$falsified)
})

test_that("medsim_method_bounds() works with M_star column", {
  set.seed(4)
  d <- data.frame(A = rbinom(100, 1L, 0.5),
                  M_star = rbinom(100, 1L, 0.6), Y = rnorm(100))
  res <- medsim_method_bounds(d, list(NDE = 0.2, NIE = 0.3))
  expect_true(all(c("NDE_lower", "NIE_lower") %in% names(res)))
})
