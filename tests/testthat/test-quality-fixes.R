# Regression tests for the 2026-07 quality-fix round
# Finding 2 (X2): generic column validation for non-core estimand kinds
# Finding 3 (X5): interval coverage -> coverage_mcse -> non-empty LaTeX table

# ── Finding 2: medsim_validate_scenario generic fallback ──────────────────

test_that("validate_scenario accepts method-defined columns for interval kind", {
  sc <- medsim_scenario(
    name = "interval_ok",
    data_generator = function(n = 10) data.frame(zzz = rnorm(n)),
    estimand = medsim_estimand("interval", params = "theta")
  )
  # Column contract is method-defined: no X/M/Y requirement for interval kind
  expect_true(suppressMessages(medsim_validate_scenario(sc)))
})

test_that("validate_scenario rejects a 0-column data.frame for interval kind", {
  sc <- medsim_scenario(
    name = "interval_empty_cols",
    data_generator = function(n = 10) as.data.frame(matrix(nrow = n, ncol = 0)),
    estimand = medsim_estimand("interval", params = "theta")
  )
  expect_error(medsim_validate_scenario(sc), "non-empty data.frame")
})

test_that("validate_scenario rejects a 0-row data.frame for numeric kind", {
  sc <- medsim_scenario(
    name = "numeric_empty_rows",
    data_generator = function(n = 10) data.frame(zzz = numeric(0)),
    estimand = medsim_estimand("numeric", params = "theta")
  )
  expect_error(medsim_validate_scenario(sc), "non-empty data.frame")
})

# ── Finding 3: interval coverage MCSE + non-empty coverage table ──────────

make_interval_results <- function(n_rep = 20L) {
  set.seed(99L)
  structure(list(
    results = data.frame(
      scenario    = rep("sc1", n_rep),
      theta_lower = rnorm(n_rep, -0.2, 0.01),
      theta_upper = rnorm(n_rep, 0.4, 0.01)
    ),
    # truth near the upper bound so coverage is INTERIOR (0 < p < 1) -- with
    # truth = 0.1 every interval covered, p was exactly 1 and the MCSE
    # assertion degenerated to 0 == 0 (review F2).
    truth = data.frame(scenario = "sc1", theta = 0.39)
  ), class = c("medsim_results", "list"))
}

test_that("interval coverage emits coverage_mcse (overall and by-scenario)", {
  cov <- medsim_analyze_coverage(
    make_interval_results(),
    estimand = medsim_estimand("interval", params = "theta")
  )
  expect_true("coverage_mcse" %in% names(cov$coverage))
  expect_true("coverage_mcse" %in% names(cov$by_scenario))
  p <- cov$coverage$coverage[1]
  n <- cov$coverage$n_valid[1]
  expect_gt(p, 0); expect_lt(p, 1)   # guard: the fixture stays non-degenerate
  expect_equal(cov$coverage$coverage_mcse[1], sqrt(p * (1 - p) / n))
  expect_gt(cov$coverage$coverage_mcse[1], 0)
})

test_that("interval coverage renders a non-empty coverage table with real MCSE", {
  cov <- medsim_analyze_coverage(
    make_interval_results(),
    estimand = medsim_estimand("interval", params = "theta")
  )
  tab <- medsim_table_coverage(cov)
  body <- tab[seq(which(tab == "\\midrule") + 1L, which(tab == "\\bottomrule") - 1L)]
  expect_gte(length(body), 1L)
  expect_true(any(grepl("theta", body, fixed = TRUE)))
  # MCSE cell must be a rendered number, not the "NA" placeholder
  expect_false(any(grepl("& NA &", body, fixed = TRUE)))
})

test_that("medsim_table_coverage is robust to absent optional columns", {
  # Hand-built minimal coverage object missing coverage_mcse/n_valid/n_failed:
  # the body row must still render (with NA placeholders), never drop.
  cov <- structure(list(
    coverage = data.frame(parameter = "theta", coverage = 0.93,
                          stringsAsFactors = FALSE),
    by_scenario = NULL,
    summary = data.frame(n_scenarios = 1L, n_parameters = 1L,
                         overall_coverage = 0.93)
  ), class = c("medsim_coverage", "list"))
  tab <- medsim_table_coverage(cov)
  body <- tab[seq(which(tab == "\\midrule") + 1L, which(tab == "\\bottomrule") - 1L)]
  expect_gte(length(body), 1L)
  expect_true(any(grepl("theta", body, fixed = TRUE)))
})
