# Source this file with medsim installed or loaded from the candidate checkout.
.calibration_arms <- c("own_li", "own_chan_meng", "fixed_li", "fixed_chan_meng")
.calibration_df <- c("li", "chan_meng")
.calibration_k <- 20L
.calibration_missingmed_sha <- "4272b367b483018fccb1a0aadc7f618e94ee6901"
.calibration_fixture_blob <- "e9956e7ea9127f4b7138b3073c7880b88508e583"

calibration_grid <- function() {
  grid <- expand.grid(
    effect = c("origin", "a_null", "b_null", "power"),
    n = c(100L, 200L, 500L), mar = c(10L, 30L, 50L),
    errors = c("gaussian", "skewed"), stringsAsFactors = FALSE
  )
  grid$a <- ifelse(grid$effect %in% c("b_null", "power"), .39, 0)
  grid$b <- ifelse(grid$effect %in% c("a_null", "power"), .39, 0)
  grid$is_null <- grid$effect != "power"
  grid$attempts <- ifelse(grid$is_null, 100000L, 10000L)
  grid$group <- paste(grid$n, ifelse(grid$is_null, "null", "power"), sep = "-")
  grid$scenario <- with(grid, paste("mbco62", group, effect, mar, errors, sep = "_"))
  grid$first_id <- 1L
  grid$last_id <- grid$attempts
  grid
}

calibration_scenarios <- function(grid) {
  lapply(seq_len(nrow(grid)), function(i) {
    cell <- grid[i, ]
    nonnormal <- if (cell$errors == "skewed") list(skew = 1.5, kurtosis = 4)
    medsim::medsim_scenario_missing(
      cell$scenario,
      true_params = list(
        a = cell$a, b = cell$b, cp = .14,
        sd_M = 1, sd_Y = 1
      ),
      mechanism = "MAR", target = "M", prop = cell$mar / 100,
      nonnormal = nonnormal
    )
  })
}

calibration_check_seeds <- function(grid) {
  seeds <- lapply(seq_len(nrow(grid)), function(i) {
    cell <- grid[i, ]
    medsim:::.medsim_det_seed(cell$scenario, seq.int(cell$first_id, cell$last_id))
  })
  if (anyDuplicated(unlist(seeds, use.names = FALSE))) {
    stop("The full calibration manifest contains a seed collision.")
  }
  invisible(TRUE)
}

calibration_fit <- function(data, params) {
  no_missing <- !anyNA(data)
  result <- list(
    indirect = NA_real_, indirect_ci_lower = NA_real_,
    indirect_ci_upper = NA_real_, indirect_p = NA_real_,
    branch_switch = NA_integer_, converged = 0L,
    indirect_p_fixed = NA_real_, branch_mix = NA_integer_,
    p_branch_a = NA_real_, stacked_branch = NA_real_,
    r4 = NA_real_, r4_fixed = NA_real_,
    own_li = NA_real_, own_chan_meng = NA_real_,
    fixed_li = NA_real_, fixed_chan_meng = NA_real_,
    effective_k = NA_integer_, complete_by_design = no_missing,
    realized_missing = mean(is.na(data$M)), failure_reason = NA_character_
  )
  implist <- tryCatch(
    medsim:::.medsim_md_implist(data, m = .calibration_k, use_mi = !no_missing),
    error = identity
  )
  if (inherits(implist, "error")) {
    result$failure_reason <- paste("imputation:", conditionMessage(implist))
    return(result)
  }
  result$effective_k <- length(implist)
  expected_k <- if (no_missing) 1L else .calibration_k
  if (length(implist) != expected_k || any(vapply(implist, anyNA, logical(1)))) {
    result$failure_reason <- if (length(implist) == 1L && !no_missing) {
      "imputation_fallback"
    } else {
      "incomplete_or_wrong_imputation_count"
    }
    return(result)
  }

  fit <- tryCatch(
    medsim:::.medsim_mbco_mi_fit(
      implist, medsim:::.medsim_md_covariates(data), .calibration_df
    ),
    error = identity
  )
  if (inherits(fit, "error")) {
    result$failure_reason <- paste("fit:", conditionMessage(fit))
    return(result)
  }

  for (df in .calibration_df) {
    for (ariv in c("own", "fixed")) {
      result[[paste(ariv, df, sep = "_")]] <- fit$tests[[df]][[ariv]][["p"]]
    }
  }
  result[names(fit$effect)] <- fit$effect
  for (field in c("branch_switch", "branch_mix", "p_branch_a", "stacked_branch")) {
    result[[field]] <- fit[[field]]
  }
  result$r4 <- fit$tests$chan_meng$own[["r4"]]
  result$r4_fixed <- fit$tests$chan_meng$fixed[["r4"]]
  result$indirect_p <- result$indirect_p_fixed <- result$fixed_chan_meng
  result$converged <- as.integer(calibration_valid_p(result$indirect_p))
  result
}

calibration_valid_p <- function(p) is.finite(p) & p >= 0 & p <= 1

calibration_pass <- function(u_paired, u_size) u_paired < .005 && u_size < .065

calibration_limits <- function(new, new_only, legacy_only, attempts) {
  tail <- .05 / (3 * 54)
  upper <- function(n) {
    if (n == attempts) {
      1
    } else {
      stats::qbeta(tail, n + 1, attempts - n,
        lower.tail = FALSE
      )
    }
  }
  lower <- if (legacy_only == 0) {
    0
  } else {
    stats::qbeta(tail, legacy_only, attempts - legacy_only + 1)
  }
  u_paired <- upper(new_only) - lower
  u_size <- upper(new)
  c(
    u_paired = u_paired, u_size = u_size,
    passed = calibration_pass(u_paired, u_size)
  )
}

calibration_summarize <- function(rows, cell) {
  n <- nrow(rows)
  if (n != cell$attempts || !identical(sort(rows$replication), seq_len(n)) ||
    !all(rows$scenario == cell$scenario)) {
    stop("Wrong replicate keys for ", cell$scenario)
  }
  p <- rows[.calibration_arms]
  valid_path <- with(rows, (complete_by_design & effective_k == 1L) |
    (!complete_by_design & effective_k == .calibration_k))
  valid_path[is.na(valid_path)] <- FALSE
  usable <- lapply(p, function(x) calibration_valid_p(x) & valid_path)
  reject <- Map(function(x, ok) ok & x < .05, p, usable)
  new <- reject$fixed_chan_meng | !usable$fixed_chan_meng
  legacy <- reject$own_li
  paired <- usable$fixed_chan_meng & usable$own_li
  dif <- as.integer(new) - as.integer(legacy)
  mi <- valid_path & !rows$complete_by_design & !is.na(rows$branch_mix) &
    Reduce(`|`, usable)
  mix <- sum(rows$branch_mix[mi] == 1L)
  decision <- c(
    attempts = n, new = sum(new), new_only = sum(dif == 1L),
    legacy_only = sum(dif == -1L),
    paired_difference = mean(dif), paired_mcse = stats::sd(dif) / sqrt(n),
    complete_pairs = sum(paired), complete_pair_difference = mean(dif[paired]),
    complete_pair_mcse = stats::sd(dif[paired]) / sqrt(sum(paired)),
    complete_by_design = sum(rows$complete_by_design %in% TRUE),
    branch_mix = mix, branch_mix_denominator = sum(mi),
    branch_mix_rate = mix / sum(mi),
    mean_realized_missing = mean(rows$realized_missing, na.rm = TRUE)
  )
  if (cell$is_null) {
    decision <- c(decision, calibration_limits(
      decision[["new"]], decision[["new_only"]], decision[["legacy_only"]], n
    ))
  }
  arms <- do.call(rbind, lapply(.calibration_arms, function(arm) {
    ok <- usable[[arm]]
    r <- reject[[arm]]
    rate <- mean(r)
    rate_usable <- mean(r[ok])
    data.frame(
      arm = arm, attempts = n, usable = sum(ok), unusable = sum(!ok),
      rejections = sum(r), rate = rate, mcse = sqrt(rate * (1 - rate) / n),
      rate_usable = rate_usable,
      mcse_usable = sqrt(rate_usable * (1 - rate_usable) / sum(ok))
    )
  }))
  reasons <- do.call(rbind, lapply(.calibration_arms, function(arm) {
    bad <- !usable[[arm]]
    reason <- rows$failure_reason[bad]
    reason[is.na(reason) & !valid_path[bad]] <- "missing_or_invalid_method_path"
    reason[is.na(reason)] <- "invalid_arm_p_value"
    data.frame(arm = rep(arm, length(reason)), reason = reason)
  }))
  reasons <- as.data.frame(table(reasons), stringsAsFactors = FALSE)
  list(
    decision = decision, arms = arms,
    unusable_reasons = reasons[reasons$Freq > 0L, ],
    effective_k = as.data.frame(table(rows$effective_k, useNA = "ifany"))
  )
}
