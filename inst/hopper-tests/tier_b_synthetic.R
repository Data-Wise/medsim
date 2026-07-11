################################################################################
# Tier-B synthetic study — self-contained (NO product-of-three kernel).
#
# A trivial analytic Wald-CI-for-a-normal-mean study that exercises the WHOLE
# medsim chunked API (build scenarios -> medsim_run_chunk x N -> combine ->
# analyze_coverage) at real cluster scale, with a known truth so coverage is
# checkable. Reused by every Tier-B pilot/full script in this directory.
#
# Why synthetic: medsim is shared infra and cannot depend on an un-packaged
# integrator; the real prod3 test-inversion study stays external as the genuine
# integration test (see tasks/plan.md Decision 3 / B3).
################################################################################

suppressPackageStartupMessages(library(medsim))

# Six scenarios with distinct true means -> a coverage grid. data_generator(n)
# draws n iid N(theta, 1); the estimand is the mean; truth = theta.
TIER_B_THETAS <- c(s1 = -1.0, s2 = -0.4, s3 = 0.0, s4 = 0.3, s5 = 1.0, s6 = 2.5)

# Near-nominal method: standard 95% Wald interval -> ~0.95 coverage.
tier_b_method_nominal <- function(data, params) {
  n  <- nrow(data)
  m  <- mean(data$x)
  se <- stats::sd(data$x) / sqrt(n)
  list(theta_lower = m - 1.96 * se, theta_upper = m + 1.96 * se)
}

# Planted-defect method (for the dogfood can-fail control): 1/3-width interval
# -> must undercover. Proves the at-scale dogfood can actually fail.
tier_b_method_narrow <- function(data, params) {
  ci   <- tier_b_method_nominal(data, params)
  mid  <- (ci$theta_lower + ci$theta_upper) / 2
  half <- (ci$theta_upper - ci$theta_lower) / 2 / 3
  list(theta_lower = mid - half, theta_upper = mid + half)
}

tier_b_estimand <- function() {
  medsim_estimand("interval", params = "theta", ci = "standard",
                  truth = function(s) c(theta = s$params$theta_true))
}

build_tier_b_scenarios <- function() {
  lapply(seq_along(TIER_B_THETAS), function(i) {
    nm <- names(TIER_B_THETAS)[i]
    th <- unname(TIER_B_THETAS[i])
    medsim_scenario(
      name = nm,
      data_generator = local({
        theta <- th
        function(n = 200) data.frame(x = stats::rnorm(n, mean = theta, sd = 1))
      }),
      params   = list(theta_true = th),
      estimand = tier_b_estimand()
    )
  })
}

# Truth table to attach post-combine (medsim_run_chunk does not forward
# compute_truth; this is the documented pattern for chunked runs).
tier_b_truth <- function() {
  data.frame(scenario = names(TIER_B_THETAS),
             theta    = unname(TIER_B_THETAS),
             stringsAsFactors = FALSE)
}
