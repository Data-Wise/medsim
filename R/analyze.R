# Result Analysis Functions
#
# Functions for analyzing simulation results: accuracy, bias, coverage, power

#' Analyze Simulation Results
#'
#' @description
#' Computes comprehensive accuracy metrics for simulation results including
#' bias, mean absolute error (MAE), root mean squared error (RMSE), and
#' relative efficiency.
#'
#' @param results A medsim_results object from [medsim_run()]
#' @param metrics Character vector: which metrics to compute. Options:
#'   - "bias": Mean estimation error
#'   - "mae": Mean absolute error
#'   - "rmse": Root mean squared error
#'   - "median_ae": Median absolute error
#'   - "max_ae": Maximum absolute error
#'   - "relative_bias": Bias as percentage of truth
#'   - "all": Compute all metrics (default)
#' @param by_scenario Logical: Compute metrics separately for each scenario
#'   (default TRUE)
#'
#' @return A list with class "medsim_analysis" containing:
#'   - `accuracy`: data.frame with accuracy metrics
#'   - `by_scenario`: data.frame with metrics by scenario (if by_scenario=TRUE)
#'   - `summary`: Overall summary statistics
#'
#' @details
#' ## Metrics Computed
#'
#' For each estimated parameter with known ground truth:
#'
#' - **Bias**: Mean estimation error (estimate - truth)
#' - **MAE**: Mean absolute error, robust to outliers
#' - **RMSE**: Root mean squared error, penalizes large errors
#' - **Median AE**: Median absolute error, very robust
#' - **Max AE**: Maximum absolute error, identifies worst case
#' - **Relative Bias**: Bias as percentage of true value
#'
#' ## Interpretation
#'
#' - Bias close to 0: unbiased estimator
#' - MAE/RMSE small: accurate estimator
#' - RMSE > MAE: presence of outliers/large errors
#' - Relative bias: standardized bias metric
#'
#' @examples
#' \dontrun{
#' # Run simulation
#' results <- medsim_run(method, scenarios, config)
#'
#' # Analyze accuracy
#' analysis <- medsim_analyze(results)
#'
#' # View overall metrics
#' print(analysis$accuracy)
#'
#' # View by-scenario metrics
#' print(analysis$by_scenario)
#'
#' # Custom metrics
#' analysis <- medsim_analyze(
#'   results,
#'   metrics = c("bias", "mae", "rmse"),
#'   by_scenario = FALSE
#' )
#' }
#'
#' @seealso [medsim_analyze_coverage()], [medsim_compare_methods()]
#'
#' @export
medsim_analyze <- function(results,
                            metrics = "all",
                            by_scenario = TRUE) {

  # --- Input Validation ---
  if (!inherits(results, "medsim_results")) {
    stop("results must be a medsim_results object from medsim_run()")
  }

  if (is.null(results$truth) || nrow(results$truth) == 0) {
    stop("Cannot compute accuracy metrics: no ground truth available. ",
         "Provide compute_truth function to medsim_run()")
  }

  # Validate metrics
  valid_metrics <- c("bias", "mae", "rmse", "median_ae", "max_ae", "relative_bias")
  if (length(metrics) == 1 && metrics == "all") {
    metrics <- valid_metrics
  } else {
    invalid <- setdiff(metrics, valid_metrics)
    if (length(invalid) > 0) {
      stop("Invalid metrics: ", paste(invalid, collapse = ", "),
           ". Valid options: ", paste(valid_metrics, collapse = ", "))
    }
  }

  # --- Merge Results with Ground Truth ---
  # Match on scenario name
  merged <- merge(
    results$results,
    results$truth,
    by = "scenario",
    suffixes = c("_estimate", "_truth")
  )

  # Identify estimate columns (exclude metadata)
  metadata_cols <- c("scenario", "replication", "elapsed")
  estimate_cols <- setdiff(names(results$results), metadata_cols)

  # --- Compute Metrics ---
  accuracy_list <- list()

  for (param in estimate_cols) {
    estimate_col <- paste0(param, "_estimate")
    truth_col <- paste0(param, "_truth")

    # Skip if truth not available
    if (!truth_col %in% names(merged)) {
      warning(sprintf("No ground truth for parameter '%s', skipping", param))
      next
    }

    estimates <- merged[[estimate_col]]
    truth <- merged[[truth_col]]

    # Remove missing values
    valid <- !is.na(estimates) & !is.na(truth)
    estimates <- estimates[valid]
    truth <- truth[valid]

    if (length(estimates) == 0) {
      warning(sprintf("No valid estimates for parameter '%s', skipping", param))
      next
    }

    # Compute error
    error <- estimates - truth
    abs_error <- abs(error)

    # Compute requested metrics
    param_metrics <- list(parameter = param)

    if ("bias" %in% metrics) {
      param_metrics$bias <- mean(error)
    }
    if ("mae" %in% metrics) {
      param_metrics$mae <- mean(abs_error)
    }
    if ("rmse" %in% metrics) {
      param_metrics$rmse <- sqrt(mean(error^2))
    }
    if ("median_ae" %in% metrics) {
      param_metrics$median_ae <- median(abs_error)
    }
    if ("max_ae" %in% metrics) {
      param_metrics$max_ae <- max(abs_error)
    }
    if ("relative_bias" %in% metrics) {
      # Relative bias as percentage
      # Use mean truth (should be constant within parameter)
      mean_truth <- mean(truth)
      if (abs(mean_truth) > 1e-10) {
        param_metrics$relative_bias <- (mean(error) / mean_truth) * 100
      } else {
        param_metrics$relative_bias <- NA_real_
      }
    }

    accuracy_list[[param]] <- as.data.frame(param_metrics)
  }

  # Combine into data.frame
  accuracy <- do.call(rbind, accuracy_list)
  rownames(accuracy) <- NULL

  # --- By-Scenario Analysis ---
  by_scenario_df <- NULL

  if (by_scenario) {
    scenario_list <- list()

    for (scenario_name in unique(merged$scenario)) {
      scenario_data <- merged[merged$scenario == scenario_name, ]

      for (param in estimate_cols) {
        estimate_col <- paste0(param, "_estimate")
        truth_col <- paste0(param, "_truth")

        if (!truth_col %in% names(scenario_data)) next

        estimates <- scenario_data[[estimate_col]]
        truth <- scenario_data[[truth_col]]

        valid <- !is.na(estimates) & !is.na(truth)
        estimates <- estimates[valid]
        truth <- truth[valid]

        if (length(estimates) == 0) next

        error <- estimates - truth
        abs_error <- abs(error)

        scenario_metrics <- list(
          scenario = scenario_name,
          parameter = param
        )

        if ("bias" %in% metrics) scenario_metrics$bias <- mean(error)
        if ("mae" %in% metrics) scenario_metrics$mae <- mean(abs_error)
        if ("rmse" %in% metrics) scenario_metrics$rmse <- sqrt(mean(error^2))
        if ("median_ae" %in% metrics) scenario_metrics$median_ae <- median(abs_error)
        if ("max_ae" %in% metrics) scenario_metrics$max_ae <- max(abs_error)
        if ("relative_bias" %in% metrics) {
          mean_truth <- mean(truth)
          if (abs(mean_truth) > 1e-10) {
            scenario_metrics$relative_bias <- (mean(error) / mean_truth) * 100
          } else {
            scenario_metrics$relative_bias <- NA_real_
          }
        }

        key <- paste(scenario_name, param, sep = "_")
        scenario_list[[key]] <- as.data.frame(scenario_metrics)
      }
    }

    by_scenario_df <- do.call(rbind, scenario_list)
    rownames(by_scenario_df) <- NULL
  }

  # --- Overall Summary ---
  summary_stats <- data.frame(
    n_scenarios = length(unique(results$results$scenario)),
    n_replications = max(results$results$replication),
    n_parameters = nrow(accuracy),
    stringsAsFactors = FALSE
  )

  # --- Return ---
  analysis <- list(
    accuracy = accuracy,
    by_scenario = by_scenario_df,
    summary = summary_stats,
    metrics_computed = metrics
  )

  class(analysis) <- c("medsim_analysis", "list")

  return(analysis)
}

#' Analyze Coverage Rates
#'
#' @description
#' Computes coverage rates for confidence intervals from simulation results.
#' Requires that the simulation method returns confidence interval bounds
#' (e.g., `ci_lower`, `ci_upper`).
#'
#' @param results A medsim_results object from [medsim_run()]
#' @param ci_levels Numeric vector: nominal confidence levels to check
#'   (default: c(0.90, 0.95, 0.99))
#' @param ci_suffix Character: suffix for CI columns (default: "_ci")
#'   E.g., if parameter is "indirect", looks for "indirect_ci_lower" and
#'   "indirect_ci_upper"
#' @param by_scenario Logical: Compute coverage separately for each scenario
#'   (default TRUE)
#'
#' @return A list with class "medsim_coverage" containing:
#'   - `coverage`: data.frame with coverage rates
#'   - `by_scenario`: data.frame with coverage by scenario (if by_scenario=TRUE)
#'   - `summary`: Overall summary statistics
#'
#' @details
#' ## Coverage Interpretation
#'
#' Coverage rate is the proportion of confidence intervals that contain the
#' true parameter value. For a 95% CI, expect ~95% coverage in large samples.
#'
#' - Coverage < nominal: CI too narrow (anti-conservative)
#' - Coverage ≈ nominal: CI has correct width
#' - Coverage > nominal: CI too wide (conservative)
#'
#' ## Column Naming Conventions
#'
#' The function looks for columns named:
#' - `{parameter}_ci_lower` and `{parameter}_ci_upper` (default)
#' - Or custom suffix: `{parameter}{ci_suffix}_lower` and `{parameter}{ci_suffix}_upper`
#'
#' @examples
#' \dontrun{
#' # Method that returns CIs
#' my_method <- function(data, params) {
#'   # ... estimation
#'   list(
#'     indirect = estimate,
#'     indirect_ci_lower = ci[1],
#'     indirect_ci_upper = ci[2]
#'   )
#' }
#'
#' results <- medsim_run(my_method, scenarios, config)
#'
#' # Analyze coverage
#' coverage <- medsim_analyze_coverage(results)
#'
#' print(coverage$coverage)
#' #>   parameter  coverage_90  coverage_95  coverage_99
#' #>   indirect   0.902        0.951        0.991
#' }
#'
#' @seealso [medsim_analyze()], [medsim_analyze_power()]
#'
#' @export
medsim_analyze_coverage <- function(results,
                                    ci_levels = c(0.90, 0.95, 0.99),
                                    ci_suffix = "_ci",
                                    by_scenario = TRUE) {

  # --- Input Validation ---
  if (!inherits(results, "medsim_results")) {
    stop("results must be a medsim_results object from medsim_run()")
  }

  if (is.null(results$truth) || nrow(results$truth) == 0) {
    stop("Cannot compute coverage: no ground truth available. ",
         "Provide compute_truth function to medsim_run()")
  }

  if (!is.numeric(ci_levels) || any(ci_levels <= 0) || any(ci_levels >= 1)) {
    stop("ci_levels must be numeric values between 0 and 1")
  }

  # --- Merge Results with Ground Truth ---
  merged <- merge(
    results$results,
    results$truth,
    by = "scenario",
    suffixes = c("_estimate", "_truth")
  )

  # --- Identify CI Columns ---
  # Look for columns with pattern {param}_ci_lower and {param}_ci_upper
  ci_lower_cols <- grep(paste0(ci_suffix, "_lower$"), names(merged), value = TRUE)
  ci_upper_cols <- grep(paste0(ci_suffix, "_upper$"), names(merged), value = TRUE)

  # Extract parameter names
  param_names <- gsub(paste0(ci_suffix, "_lower$"), "", ci_lower_cols)

  if (length(param_names) == 0) {
    stop("No confidence interval columns found. ",
         "Expected columns named '{parameter}", ci_suffix, "_lower' and ",
         "'{parameter}", ci_suffix, "_upper'")
  }

  # --- Compute Coverage ---
  coverage_list <- list()

  for (param in param_names) {
    lower_col <- paste0(param, ci_suffix, "_lower")
    upper_col <- paste0(param, ci_suffix, "_upper")
    truth_col <- paste0(param, "_truth")

    # Skip if truth not available
    if (!truth_col %in% names(merged)) {
      warning(sprintf("No ground truth for parameter '%s', skipping", param))
      next
    }

    ci_lower <- merged[[lower_col]]
    ci_upper <- merged[[upper_col]]
    truth <- merged[[truth_col]]

    # Remove missing values
    valid <- !is.na(ci_lower) & !is.na(ci_upper) & !is.na(truth)
    ci_lower <- ci_lower[valid]
    ci_upper <- ci_upper[valid]
    truth <- truth[valid]

    if (length(ci_lower) == 0) {
      warning(sprintf("No valid CIs for parameter '%s', skipping", param))
      next
    }

    # Check if truth is in CI
    in_ci <- (truth >= ci_lower) & (truth <= ci_upper)

    # Compute coverage rate
    coverage_rate <- mean(in_ci)

    # Store result
    coverage_list[[param]] <- data.frame(
      parameter = param,
      coverage = coverage_rate,
      n_valid = length(in_ci),
      stringsAsFactors = FALSE
    )
  }

  # Combine into data.frame
  coverage_df <- do.call(rbind, coverage_list)
  rownames(coverage_df) <- NULL

  # --- By-Scenario Coverage ---
  by_scenario_df <- NULL

  if (by_scenario) {
    scenario_list <- list()

    for (scenario_name in unique(merged$scenario)) {
      scenario_data <- merged[merged$scenario == scenario_name, ]

      for (param in param_names) {
        lower_col <- paste0(param, ci_suffix, "_lower")
        upper_col <- paste0(param, ci_suffix, "_upper")
        truth_col <- paste0(param, "_truth")

        if (!truth_col %in% names(scenario_data)) next

        ci_lower <- scenario_data[[lower_col]]
        ci_upper <- scenario_data[[upper_col]]
        truth <- scenario_data[[truth_col]]

        valid <- !is.na(ci_lower) & !is.na(ci_upper) & !is.na(truth)
        ci_lower <- ci_lower[valid]
        ci_upper <- ci_upper[valid]
        truth <- truth[valid]

        if (length(ci_lower) == 0) next

        in_ci <- (truth >= ci_lower) & (truth <= ci_upper)

        scenario_list[[paste(scenario_name, param, sep = "_")]] <- data.frame(
          scenario = scenario_name,
          parameter = param,
          coverage = mean(in_ci),
          n_valid = length(in_ci),
          stringsAsFactors = FALSE
        )
      }
    }

    by_scenario_df <- do.call(rbind, scenario_list)
    rownames(by_scenario_df) <- NULL
  }

  # --- Summary ---
  summary_stats <- data.frame(
    n_scenarios = length(unique(merged$scenario)),
    n_parameters = nrow(coverage_df),
    overall_coverage = mean(coverage_df$coverage),
    stringsAsFactors = FALSE
  )

  # --- Return ---
  coverage <- list(
    coverage = coverage_df,
    by_scenario = by_scenario_df,
    summary = summary_stats,
    ci_levels_expected = ci_levels
  )

  class(coverage) <- c("medsim_coverage", "list")

  return(coverage)
}

#' Analyze Statistical Power
#'
#' @description
#' Computes empirical power (rejection rate under alternative hypothesis) from
#' simulation results. Requires that the simulation method returns p-values.
#'
#' @param results A medsim_results object from [medsim_run()]
#' @param alpha Numeric: significance level (default: 0.05)
#' @param p_suffix Character: suffix for p-value columns (default: "_p")
#'   E.g., if parameter is "indirect", looks for "indirect_p"
#' @param null_value Numeric: null hypothesis value (default: 0)
#' @param by_scenario Logical: Compute power separately for each scenario
#'   (default TRUE)
#'
#' @return A list with class "medsim_power" containing:
#'   - `power`: data.frame with power rates
#'   - `by_scenario`: data.frame with power by scenario (if by_scenario=TRUE)
#'   - `summary`: Overall summary statistics
#'
#' @details
#' ## Power Interpretation
#'
#' Power is the probability of correctly rejecting the null hypothesis when
#' it is false (i.e., detecting a true effect).
#'
#' - Power = proportion of replications where p < alpha
#' - Higher power = better ability to detect effects
#' - Power depends on: effect size, sample size, alpha level
#'
#' ## Notes
#'
#' This function computes **empirical power** from simulations, not theoretical
#' power. For scenarios where the null is true (e.g., indirect effect = 0),
#' the rejection rate represents Type I error rate, not power.
#'
#' @examples
#' \dontrun{
#' # Method that returns p-values
#' my_method <- function(data, params) {
#'   # ... estimation and testing
#'   list(
#'     indirect = estimate,
#'     indirect_p = p_value
#'   )
#' }
#'
#' results <- medsim_run(my_method, scenarios, config)
#'
#' # Analyze power
#' power <- medsim_analyze_power(results, alpha = 0.05)
#'
#' print(power$power)
#' #>   parameter  power    n_valid
#' #>   indirect   0.847    1000
#' }
#'
#' @seealso [medsim_analyze()], [medsim_analyze_coverage()]
#'
#' @export
medsim_analyze_power <- function(results,
                                  alpha = 0.05,
                                  p_suffix = "_p",
                                  null_value = 0,
                                  by_scenario = TRUE) {

  # --- Input Validation ---
  if (!inherits(results, "medsim_results")) {
    stop("results must be a medsim_results object from medsim_run()")
  }

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a numeric value between 0 and 1")
  }

  # --- Identify P-value Columns ---
  p_value_cols <- grep(paste0(p_suffix, "$"), names(results$results), value = TRUE)
  param_names <- gsub(paste0(p_suffix, "$"), "", p_value_cols)

  if (length(param_names) == 0) {
    stop("No p-value columns found. ",
         "Expected columns named '{parameter}", p_suffix, "'")
  }

  # --- Compute Power ---
  power_list <- list()

  for (param in param_names) {
    p_col <- paste0(param, p_suffix)
    p_values <- results$results[[p_col]]

    # Remove missing values
    valid <- !is.na(p_values)
    p_values <- p_values[valid]

    if (length(p_values) == 0) {
      warning(sprintf("No valid p-values for parameter '%s', skipping", param))
      next
    }

    # Compute rejection rate (power under alternative)
    rejected <- p_values < alpha
    power_rate <- mean(rejected)

    power_list[[param]] <- data.frame(
      parameter = param,
      power = power_rate,
      n_valid = length(p_values),
      alpha = alpha,
      stringsAsFactors = FALSE
    )
  }

  # Combine into data.frame
  power_df <- do.call(rbind, power_list)
  rownames(power_df) <- NULL

  # --- By-Scenario Power ---
  by_scenario_df <- NULL

  if (by_scenario) {
    scenario_list <- list()

    for (scenario_name in unique(results$results$scenario)) {
      scenario_data <- results$results[results$results$scenario == scenario_name, ]

      for (param in param_names) {
        p_col <- paste0(param, p_suffix)
        p_values <- scenario_data[[p_col]]

        valid <- !is.na(p_values)
        p_values <- p_values[valid]

        if (length(p_values) == 0) next

        rejected <- p_values < alpha

        scenario_list[[paste(scenario_name, param, sep = "_")]] <- data.frame(
          scenario = scenario_name,
          parameter = param,
          power = mean(rejected),
          n_valid = length(p_values),
          alpha = alpha,
          stringsAsFactors = FALSE
        )
      }
    }

    by_scenario_df <- do.call(rbind, scenario_list)
    rownames(by_scenario_df) <- NULL
  }

  # --- Summary ---
  summary_stats <- data.frame(
    n_scenarios = length(unique(results$results$scenario)),
    n_parameters = nrow(power_df),
    overall_power = mean(power_df$power),
    alpha = alpha,
    stringsAsFactors = FALSE
  )

  # --- Return ---
  power <- list(
    power = power_df,
    by_scenario = by_scenario_df,
    summary = summary_stats,
    alpha = alpha
  )

  class(power) <- c("medsim_power", "list")

  return(power)
}

#' Compare Multiple Methods
#'
#' @description
#' Compares performance of multiple estimation methods across scenarios.
#' Combines results from multiple simulation runs and computes relative
#' performance metrics.
#'
#' @param ... Named medsim_results objects (e.g., method1 = results1,
#'   method2 = results2)
#' @param metrics Character vector: which comparison metrics to compute.
#'   Options: "accuracy", "timing", "coverage", "all" (default)
#'
#' @return A list with class "medsim_comparison" containing:
#'   - `accuracy_comparison`: data.frame comparing accuracy metrics
#'   - `timing_comparison`: data.frame comparing timing
#'   - `coverage_comparison`: data.frame comparing coverage (if applicable)
#'   - `summary`: Overall comparison summary
#'
#' @details
#' ## Comparison Metrics
#'
#' - **Accuracy**: MAE, RMSE, bias across methods
#' - **Timing**: Mean/median execution time
#' - **Coverage**: CI coverage rates (if CIs available)
#' - **Relative Efficiency**: Ratio of MSE (lower = better)
#'
#' ## Notes
#'
#' All results must:
#' - Use the same scenarios (matched by name)
#' - Use the same configuration (n_replications, seed)
#' - Have ground truth available (for accuracy comparison)
#'
#' @examples
#' \dontrun{
#' # Run multiple methods
#' results_proposed <- medsim_run(method_proposed, scenarios, config)
#' results_delta <- medsim_run(method_delta, scenarios, config)
#' results_boot <- medsim_run(method_boot, scenarios, config)
#'
#' # Compare
#' comparison <- medsim_compare_methods(
#'   proposed = results_proposed,
#'   delta = results_delta,
#'   bootstrap = results_boot
#' )
#'
#' # View accuracy comparison
#' print(comparison$accuracy_comparison)
#'
#' # View timing comparison
#' print(comparison$timing_comparison)
#' }
#'
#' @seealso [medsim_analyze()], [medsim_analyze_coverage()]
#'
#' @export
medsim_compare_methods <- function(..., metrics = "all") {

  # --- Input Validation ---
  results_list <- list(...)

  if (length(results_list) < 2) {
    stop("At least two medsim_results objects required for comparison")
  }

  # Check that all are medsim_results
  is_results <- sapply(results_list, inherits, "medsim_results")
  if (!all(is_results)) {
    stop("All arguments must be medsim_results objects")
  }

  # Get method names
  method_names <- names(results_list)
  if (is.null(method_names) || any(method_names == "")) {
    stop("All results must be named (e.g., method1 = results1, method2 = results2)")
  }

  # Validate metrics
  valid_metrics <- c("accuracy", "timing", "coverage")
  if (length(metrics) == 1 && metrics == "all") {
    metrics <- valid_metrics
  } else {
    invalid <- setdiff(metrics, valid_metrics)
    if (length(invalid) > 0) {
      stop("Invalid metrics: ", paste(invalid, collapse = ", "))
    }
  }

  # --- Check Compatibility ---
  # All should have same scenarios
  scenario_names <- lapply(results_list, function(r) unique(r$results$scenario))
  if (length(unique(lapply(scenario_names, sort))) > 1) {
    warning("Methods were run on different scenarios. Comparison may be incomplete.")
  }

  # --- Accuracy Comparison ---
  accuracy_comparison <- NULL

  if ("accuracy" %in% metrics) {
    acc_list <- list()

    for (i in seq_along(results_list)) {
      method <- method_names[i]
      results <- results_list[[i]]

      # Skip if no truth
      if (is.null(results$truth) || nrow(results$truth) == 0) {
        warning(sprintf("Method '%s' has no ground truth, skipping accuracy comparison", method))
        next
      }

      # Compute accuracy metrics
      analysis <- medsim_analyze(results, metrics = c("mae", "rmse", "bias"), by_scenario = FALSE)

      acc_df <- analysis$accuracy
      acc_df$method <- method

      acc_list[[method]] <- acc_df
    }

    if (length(acc_list) > 0) {
      accuracy_comparison <- do.call(rbind, acc_list)
      rownames(accuracy_comparison) <- NULL

      # Reorder columns
      accuracy_comparison <- accuracy_comparison[, c("method", "parameter", "mae", "rmse", "bias")]
    }
  }

  # --- Timing Comparison ---
  timing_comparison <- NULL

  if ("timing" %in% metrics) {
    timing_list <- list()

    for (i in seq_along(results_list)) {
      method <- method_names[i]
      results <- results_list[[i]]

      # Check if elapsed time available
      if (!"elapsed" %in% names(results$results)) {
        warning(sprintf("Method '%s' has no timing data, skipping timing comparison", method))
        next
      }

      elapsed <- results$results$elapsed
      valid <- !is.na(elapsed)
      elapsed <- elapsed[valid]

      if (length(elapsed) == 0) next

      timing_list[[method]] <- data.frame(
        method = method,
        mean_time = mean(elapsed),
        median_time = median(elapsed),
        min_time = min(elapsed),
        max_time = max(elapsed),
        total_time = sum(elapsed),
        n_runs = length(elapsed),
        stringsAsFactors = FALSE
      )
    }

    if (length(timing_list) > 0) {
      timing_comparison <- do.call(rbind, timing_list)
      rownames(timing_comparison) <- NULL
    }
  }

  # --- Coverage Comparison ---
  coverage_comparison <- NULL

  if ("coverage" %in% metrics) {
    cov_list <- list()

    for (i in seq_along(results_list)) {
      method <- method_names[i]
      results <- results_list[[i]]

      # Try to compute coverage
      coverage <- tryCatch(
        medsim_analyze_coverage(results, by_scenario = FALSE),
        error = function(e) NULL
      )

      if (is.null(coverage)) {
        warning(sprintf("Method '%s' has no coverage data, skipping coverage comparison", method))
        next
      }

      cov_df <- coverage$coverage
      cov_df$method <- method

      cov_list[[method]] <- cov_df
    }

    if (length(cov_list) > 0) {
      coverage_comparison <- do.call(rbind, cov_list)
      rownames(coverage_comparison) <- NULL

      # Reorder columns
      coverage_comparison <- coverage_comparison[, c("method", "parameter", "coverage", "n_valid")]
    }
  }

  # --- Summary ---
  summary_stats <- data.frame(
    n_methods = length(results_list),
    methods = paste(method_names, collapse = ", "),
    stringsAsFactors = FALSE
  )

  # --- Return ---
  comparison <- list(
    accuracy_comparison = accuracy_comparison,
    timing_comparison = timing_comparison,
    coverage_comparison = coverage_comparison,
    summary = summary_stats,
    metrics_compared = metrics
  )

  class(comparison) <- c("medsim_comparison", "list")

  return(comparison)
}

#' Print Analysis Results
#'
#' @param x A medsim_analysis object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_analysis <- function(x, ...) {
  cat("\n")
  cat("Simulation Analysis\n")
  cat("===================\n\n")

  cat("Summary:\n")
  cat(sprintf("  Scenarios:       %d\n", x$summary$n_scenarios))
  cat(sprintf("  Replications:    %d\n", x$summary$n_replications))
  cat(sprintf("  Parameters:      %d\n", x$summary$n_parameters))
  cat("\n")

  cat("Accuracy Metrics:\n")
  print(x$accuracy, row.names = FALSE)
  cat("\n")

  if (!is.null(x$by_scenario)) {
    cat("By-Scenario Metrics Available: Use $by_scenario to view\n\n")
  }

  invisible(x)
}

#' Print Coverage Results
#'
#' @param x A medsim_coverage object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_coverage <- function(x, ...) {
  cat("\n")
  cat("Coverage Analysis\n")
  cat("=================\n\n")

  cat("Summary:\n")
  cat(sprintf("  Scenarios:         %d\n", x$summary$n_scenarios))
  cat(sprintf("  Parameters:        %d\n", x$summary$n_parameters))
  cat(sprintf("  Overall Coverage:  %.3f\n", x$summary$overall_coverage))
  cat("\n")

  cat("Coverage Rates:\n")
  print(x$coverage, row.names = FALSE)
  cat("\n")

  if (!is.null(x$by_scenario)) {
    cat("By-Scenario Coverage Available: Use $by_scenario to view\n\n")
  }

  invisible(x)
}

#' Print Power Results
#'
#' @param x A medsim_power object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_power <- function(x, ...) {
  cat("\n")
  cat("Power Analysis\n")
  cat("==============\n\n")

  cat("Summary:\n")
  cat(sprintf("  Scenarios:      %d\n", x$summary$n_scenarios))
  cat(sprintf("  Parameters:     %d\n", x$summary$n_parameters))
  cat(sprintf("  Overall Power:  %.3f\n", x$summary$overall_power))
  cat(sprintf("  Alpha Level:    %.3f\n", x$summary$alpha))
  cat("\n")

  cat("Power Rates:\n")
  print(x$power, row.names = FALSE)
  cat("\n")

  if (!is.null(x$by_scenario)) {
    cat("By-Scenario Power Available: Use $by_scenario to view\n\n")
  }

  invisible(x)
}

#' Print Method Comparison
#'
#' @param x A medsim_comparison object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_comparison <- function(x, ...) {
  cat("\n")
  cat("Method Comparison\n")
  cat("=================\n\n")

  cat("Summary:\n")
  cat(sprintf("  Methods:  %s\n", x$summary$methods))
  cat("\n")

  if (!is.null(x$accuracy_comparison)) {
    cat("Accuracy Comparison:\n")
    print(x$accuracy_comparison, row.names = FALSE)
    cat("\n")
  }

  if (!is.null(x$timing_comparison)) {
    cat("Timing Comparison:\n")
    print(x$timing_comparison, row.names = FALSE)
    cat("\n")
  }

  if (!is.null(x$coverage_comparison)) {
    cat("Coverage Comparison:\n")
    print(x$coverage_comparison, row.names = FALSE)
    cat("\n")
  }

  invisible(x)
}
