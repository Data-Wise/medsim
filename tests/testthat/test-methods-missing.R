# WS-D tests — medsim_method_mbco_mi() / medsim_method_mc_ci() / medsim_method_ipw()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row D)
#
# medsim_method_mbco_mi() ports the validated D4-stacked MBCO (mice MI + D4
# pooling + union-null LRT; prototype-d4-mbco.R, exact match vs mitml). With
# mice + missingness it runs D4; with complete data / no mice it degrades to the
# complete-case MBCO LRT with a chi-square reference (never a Sobel test). The
# contract tests run unconditionally; D4 pooling is validated against mitml.

contract_fields <- c(
  "indirect", "indirect_ci_lower", "indirect_ci_upper",
  "indirect_p", "branch_switch", "converged"
)

model <- "
  M ~ a*X
  Y ~ b*M + cp*X
  ab := a*b
"

toy <- function(n = 300, a = 0.4, b = 0.4) {
  X <- rnorm(n)
  M <- a * X + rnorm(n)
  Y <- 0.3 * X + b * M + rnorm(n)
  d <- data.frame(X = X, M = M, Y = Y)
  d$M[sample(n, n * 0.2)] <- NA
  d
}

expect_contract <- function(out) {
  expect_true(all(contract_fields %in% names(out)))
  expect_true(is.numeric(out$indirect))
  expect_true(out$converged %in% c(0, 1))
  # CI bounds are populated (and ordered) only when the estimator converged;
  # a degenerate failure returns the NA-filled contract with converged = 0.
  if (isTRUE(out$converged == 1)) {
    expect_false(is.na(out$indirect_ci_lower))
    expect_true(out$indirect_ci_lower <= out$indirect_ci_upper)
    expect_true(out$indirect_p >= 0 && out$indirect_p <= 1)
  }
}

test_that("adapters are function factories returning a function(data, params)", {
  expect_type(medsim_method_mbco_mi(model), "closure")
  expect_type(medsim_method_mc_ci(model), "closure")
  expect_type(medsim_method_ipw(model), "closure")
})

test_that("MBCO-MI returns the contract and a {0,1} branch on MAR data", {
  set.seed(1)
  out <- medsim_method_mbco_mi(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(out$branch_switch %in% c(0, 1))
  expect_equal(out$converged, 1)
})

test_that("MC-CI returns the contract and sets branch_switch to NA", {
  set.seed(2)
  out <- medsim_method_mc_ci(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(is.na(out$branch_switch))
})

test_that("IPW returns the contract and sets branch_switch to NA", {
  set.seed(3)
  out <- medsim_method_ipw(model)(toy(), list())
  expect_contract(out)
  expect_true(is.na(out$branch_switch))
})

test_that("MBCO-MI degrades to the complete-case MBCO chi-square test (no missingness)", {
  set.seed(7)
  d <- toy()
  d <- d[stats::complete.cases(d), ] # complete data -> K = 1 -> chi-square path
  out <- medsim_method_mbco_mi(model, m = 5)(d, list())
  expect_contract(out)
  expect_equal(out$converged, 1)
})

test_that("adapters auto-detect covariate (C*) columns and keep the contract", {
  set.seed(4)
  d <- toy()
  d$C1 <- rnorm(nrow(d))
  out <- medsim_method_mbco_mi(model, m = 3)(d, list())
  expect_contract(out)
})

test_that("an estimator failure degrades to converged = 0 rather than erroring", {
  out <- medsim_method_ipw(model)(
    data.frame(X = c(1, 2), M = c(NA_real_, NA_real_), Y = c(1, 2)),
    list()
  )
  expect_contract(out)
  expect_equal(out$converged, 0)
})

test_that("MBCO-MI detects a clear indirect effect (high power)", {
  skip_if_not_installed("mice")
  set.seed(11)
  out <- medsim_method_mbco_mi(model, m = 10)(toy(400, a = 0.5, b = 0.5), list())
  expect_lt(out$indirect_p, 0.05)
  expect_gt(out$indirect, 0)
})

test_that("D4 pooling reproduces mitml::testModels(method = 'D4') [acceptance]", {
  skip_if_not_installed("mice")
  skip_if_not_installed("mitml")
  set.seed(20260611)
  n <- 200
  cc <- rnorm(n)
  x <- rbinom(n, 1, plogis(0.3 * cc))
  mm <- 0.39 * x + 0.3 * cc + rnorm(n)
  yy <- 0.2 * x + 0.39 * mm + 0.3 * cc + rnorm(n)
  d <- data.frame(X = x, M = mm, Y = yy, C = cc)
  d$M[runif(n) < plogis(qlogis(0.25) + 0.5 * x + 0.5 * cc)] <- NA
  d$Y[runif(n) < plogis(qlogis(0.25) + 0.5 * x + 0.5 * cc)] <- NA

  il <- mice::complete(
    suppressWarnings(mice::mice(d, m = 20, method = "norm", printFlag = FALSE)), "all"
  )
  lrt_lin <- function(z) {
    2 * (as.numeric(stats::logLik(stats::lm(Y ~ X + M + C, z))) -
      as.numeric(stats::logLik(stats::lm(Y ~ X + C, z))))
  }
  d_k <- vapply(il, lrt_lin, 0)
  d_s <- lrt_lin(do.call(rbind, il)) / length(il)
  mine <- medsim:::.medsim_d4_from_stats(d_k, d_s, k = 1)
  ref <- mitml::testModels(
    lapply(il, function(z) stats::lm(Y ~ X + M + C, z)),
    lapply(il, function(z) stats::lm(Y ~ X + C, z)),
    method = "D4"
  )
  expect_equal(unname(mine[["D4"]]), unname(ref$test[1, "F.value"]), tolerance = 1e-3)
  # p-value tolerance is looser: at p ~ 0.01 the F-tail is steep, so minor
  # stochastic variation in mice imputations causes ~5-10% relative p difference.
  expect_equal(unname(mine[["p"]]), unname(ref$test[1, "P(>F)"]), tolerance = 0.1)
})

# -- fixed-branch ARIV + branch-mixing diagnostics (2026-08-17) -----------------
# Motivation: Missing Effect ms:check 2026-08-16 (Fable KO #2) + the local
# comparator pilot (code/pilot-comparators-2026-08-16.R): the Chan-Meng r4 pools
# per-imputation MBCO statistics each computed on its OWN branch, so branch
# disagreement pulls dbar down and under-estimates the ARIV. The fixed-branch
# variant recomputes d_k on the stacked fit's branch. Both p-values are emitted.

diag_fields <- c("indirect_p_fixed", "branch_mix", "p_branch_a",
                 "stacked_branch", "r4", "r4_fixed")

test_that("MBCO-MI emits the additive branch diagnostics alongside the contract", {
  skip_if_not_installed("mice")
  set.seed(31)
  out <- medsim_method_mbco_mi(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(all(diag_fields %in% names(out)))
  expect_true(out$branch_mix %in% c(0, 1))
  expect_true(out$stacked_branch %in% c(0, 1))
  expect_true(out$p_branch_a >= 0 && out$p_branch_a <= 1)
  expect_true(out$indirect_p_fixed >= 0 && out$indirect_p_fixed <= 1)
  expect_true(out$r4 >= 0 && out$r4_fixed >= 0)
})

test_that("ariv = 'fixed' routes the fixed-branch p-value into indirect_p", {
  skip_if_not_installed("mice")
  set.seed(32)
  d <- toy(200, a = 0.3, b = 0)      # interior null: b = 0 -> branch disagreement likely
  set.seed(1)
  own <- medsim_method_mbco_mi(model, m = 8, ariv = "own")(d, list())
  set.seed(1)
  fix <- medsim_method_mbco_mi(model, m = 8, ariv = "fixed")(d, list())
  expect_equal(fix$indirect_p, fix$indirect_p_fixed)
  expect_equal(own$indirect_p_fixed, fix$indirect_p_fixed)
  expect_equal(own$indirect, fix$indirect)          # point estimate unaffected
})

test_that(".medsim_d4_mbco fixed_branch never lowers r4 below own-branch (same numerator)", {
  skip_if_not_installed("mice")
  set.seed(33)
  d <- toy(150, a = 0.3, b = 0)
  covs <- character(0)
  imp <- mice::mice(d, m = 6, method = "norm", printFlag = FALSE)
  implist <- mice::complete(imp, "all")
  own <- .medsim_d4_mbco(implist, covs)
  fix <- .medsim_d4_mbco(implist, covs, fixed_branch = TRUE)
  # d_k on the stacked branch >= d_k on the own (max-likelihood) branch for every k,
  # so dbar_fixed >= dbar_own and r4_fixed >= r4_own; the numerator d_S is shared.
  expect_gte(fix[["r4"]], own[["r4"]])
  expect_true(fix[["stacked_branch"]] %in% c(0, 1))
  expect_equal(unname(fix[["stacked_branch"]]), unname(own[["stacked_branch"]]))
})

test_that("with a single imputation (complete data) both p-values coincide", {
  set.seed(34)
  d <- toy(); d <- d[stats::complete.cases(d), ]
  out <- medsim_method_mbco_mi(model, m = 5)(d, list())
  expect_equal(out$indirect_p, out$indirect_p_fixed)
  expect_equal(out$branch_mix, 0)
})

test_that(".medsim_d4_mbco fixed_branch changes r4 exactly as designed (hand-built triples)", {
  # Two imputations that DISAGREE on the constrained branch; the stacked fit
  # picks the a = 0 branch. Own-branch: d_k = 2*(0 - (-1)) = 2 for both,
  # d_S = 2*(0 - (-2))/2 = 2 -> dbar == d_S -> r4 = 0.
  # Fixed-branch (key = "la"): d_1 = 2, d_2 = 6 -> dbar = 4 ->
  # r4 = (kk+1)/(k*(kk-1)) * (dbar - d_S) = 3/1 * 2 = 6.
  # A build that ignores fixed_branch (or hard-wires the own p) fails here.
  lls_list <- list(c(full = 0, la = -1, lb = -3), c(full = 0, la = -3, lb = -1))
  lls_S <- c(full = 0, la = -2, lb = -4)
  implist <- list(1, 2)   # only length(implist) is used when lls_* are supplied
  own <- .medsim_d4_mbco(implist, lls_list = lls_list, lls_S = lls_S)
  fix <- .medsim_d4_mbco(implist, lls_list = lls_list, lls_S = lls_S,
                         fixed_branch = TRUE)
  expect_equal(unname(own[["r4"]]), 0)
  expect_equal(unname(fix[["r4"]]), 6)
  expect_equal(unname(own[["stacked_branch"]]), 1)
  expect_equal(unname(fix[["stacked_branch"]]), 1)
  expect_equal(unname(own[["D4"]]), 2)              # d_S / (k * (1 + 0))
  expect_equal(unname(fix[["D4"]]), 2 / 7)          # d_S / (k * (1 + 6))
  expect_gt(unname(fix[["p"]]), unname(own[["p"]]))
})

test_that("Gate A.2 collapse audit accepts the mbco_mi branch diagnostics by default", {
  # A realistic MBCO-MI result frame: continuous estimates plus the six
  # low-cardinality diagnostics (0/1 flags, an m-valued share, ARIVs with
  # mass at 0). Before these names joined collapse_exclude, this frame raised
  # 4 [collapse] violations and medsim_combine_chunks() refused it.
  set.seed(7)
  n <- 40L
  df <- data.frame(
    scenario = "s1", replication = seq_len(n), elapsed = runif(n),
    error = NA_character_,
    indirect = rnorm(n), indirect_ci_lower = rnorm(n), indirect_ci_upper = rnorm(n),
    indirect_p = runif(n), indirect_p_fixed = runif(n),
    branch_switch = rbinom(n, 1, 0.5), converged = 1,
    branch_mix = rbinom(n, 1, 0.3), stacked_branch = rbinom(n, 1, 0.5),
    p_branch_a = sample(0:5, n, replace = TRUE) / 5,
    r4 = pmax(0, rnorm(n, 0, 0.3)), r4_fixed = pmax(0, rnorm(n, 0.2, 0.3)),
    stringsAsFactors = FALSE
  )
  df$r4[1:10] <- 0                                   # point mass at zero
  attr(df, "medsim_schema") <- 2L
  attr(df, "medsim_meta_cols") <- c("scenario", "replication", "elapsed", "error")
  expect_silent(medsim_audit_results(df))
  # positive control: with the pre-0.5.1 exclude list the same frame IS flagged
  v0 <- suppressWarnings(medsim_audit_results(
    df, on_violation = "warn", collapse_exclude = c("converged", "branch_switch")))
  expect_true("collapse" %in% vapply(v0, `[[`, "", "type"))
  # the default is name-based: an unknown 0/1 field is still flagged
  df$my_flag <- rep_len(c(0, 1), n)
  v <- suppressWarnings(medsim_audit_results(df, on_violation = "warn"))
  expect_true("collapse" %in% vapply(v, `[[`, "", "type"))
})
