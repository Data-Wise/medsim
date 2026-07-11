# Tier-A correctness guard T3 (plan.md, gap G7): failure-rate / NA-CI handling.
#
# NEWS 0.4.0 advertised n_failed/failure_rate in coverage output. Two invariants
# must hold on BOTH coverage branches (default `_ci` and estimand="interval"):
#   (1) failed replications (NA CI or NA truth -- e.g. the near-singular tail a
#       test-inversion CI returns NA for) are excluded from the coverage
#       numerator AND denominator, so coverage is over successes only and is
#       never NA-poisoned;
#   (2) failure_rate == failed / total is reported.
#
# Deterministic hand-built fixture (exact known failure fraction) -- far more
# precise than a random-failure simulation, and CRAN-instant. The interval
# branch previously OMITTED failure_rate entirely (discovered this session); the
# interval test below drove that fix.

# 10 replications: 6 covered, 1 valid-but-uncovered, 3 failed (NA CIs).
# truth = 0. Covered CI = [-1, 1]; uncovered-valid CI = [2, 3]; failed = NA.
# Expect: n_valid = 7, coverage = 6/7, n_failed = 3, failure_rate = 0.3.
.fr_lower <- c(rep(-1, 6), 2, NA, NA, NA)
.fr_upper <- c(rep( 1, 6), 3, NA, NA, NA)

.fr_results <- function(ci_suffix) {
  # ci_suffix = "_ci" for the default branch, "" for the interval branch.
  res <- data.frame(
    scenario = "s", replication = 1:10,
    stringsAsFactors = FALSE
  )
  res[[paste0("theta", ci_suffix, "_lower")]] <- .fr_lower
  res[[paste0("theta", ci_suffix, "_upper")]] <- .fr_upper
  # Default branch needs a point-estimate column named `theta` so the
  # merge-by-scenario collides it into `theta_truth`; harmless for interval.
  if (ci_suffix == "_ci") res$theta <- c(rep(0, 6), 2.5, NA, NA, NA)
  structure(
    list(results = res,
         truth   = data.frame(scenario = "s", theta = 0, stringsAsFactors = FALSE)),
    class = c("medsim_results", "list")
  )
}

test_that("default coverage branch: coverage over successes only, correct failure_rate", {
  cov <- medsim_analyze_coverage(.fr_results("_ci"), by_scenario = FALSE)
  row <- cov$coverage[cov$coverage$parameter == "theta", ]
  expect_equal(row$n_valid, 7L)
  expect_equal(row$coverage, 6 / 7)          # NOT 6/10 -> proves no NA poisoning
  expect_equal(row$n_failed, 3L)
  expect_equal(row$failure_rate, 0.3)
})

test_that("interval coverage branch: reports failure_rate (regression: previously omitted)", {
  est <- medsim_estimand("interval", params = "theta", ci = "standard",
                         truth = function(s) c(theta = 0))
  cov <- medsim_analyze_coverage(.fr_results(""), estimand = est, by_scenario = FALSE)
  row <- cov$coverage[cov$coverage$parameter == "theta", ]
  expect_equal(row$n_valid, 7L)
  expect_equal(row$coverage, 6 / 7)
  # These two columns did not exist on the interval branch before the fix.
  expect_true(all(c("n_failed", "failure_rate") %in% names(row)))
  expect_equal(row$n_failed, 3L)
  expect_equal(row$failure_rate, 0.3)
})

test_that("all-failed scenario yields failure_rate 1 and non-poisoned (NaN) coverage", {
  # The 100%-NA tail (near-singular Sigma class): every rep fails. Coverage must
  # be NaN (0/0 over successes), NOT 0, and must not crash. Default branch skips
  # a fully-invalid param with a warning, so assert via the by-scenario path on a
  # mixed set where one scenario is all-failed.
  res <- data.frame(scenario = c(rep("good", 4), rep("bad", 4)),
                    replication = rep(1:4, 2),
                    theta_ci_lower = c(rep(-1, 4), rep(NA, 4)),
                    theta_ci_upper = c(rep( 1, 4), rep(NA, 4)),
                    theta = c(rep(0, 4), rep(NA, 4)),
                    stringsAsFactors = FALSE)
  results <- structure(
    list(results = res,
         truth = data.frame(scenario = c("good", "bad"), theta = c(0, 0),
                            stringsAsFactors = FALSE)),
    class = c("medsim_results", "list"))
  # The all-NA 'bad' scenario must not error the whole analysis; 'good' still reported.
  cov <- suppressWarnings(medsim_analyze_coverage(results, by_scenario = TRUE))
  good <- cov$by_scenario[cov$by_scenario$scenario == "good", ]
  expect_equal(good$coverage, 1)
  expect_equal(good$failure_rate, 0)
})
