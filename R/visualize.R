# Visualization Functions for Simulation Results
#
# Publication-ready plotting functions adapted from product-of-three patterns

#' Plot Error Distribution Boxplots
#'
#' @description
#' Creates boxplots showing the distribution of estimation errors across
#' scenarios. Useful for comparing accuracy of different methods.
#'
#' @param results A medsim_results object from [medsim_run()], or a list of
#'   named results objects for method comparison
#' @param parameter Character: which parameter to plot (default: "indirect").
#'   If NULL, plots all parameters with available ground truth.
#' @param log_scale Logical: use log10 scale for y-axis (default: FALSE)
#' @param color_palette Character: color palette name from RColorBrewer or
#'   "viridis" (default: NULL uses ggplot2 defaults)
#' @param title Character: plot title (default: auto-generated)
#' @param ... Additional arguments passed to ggplot2::theme()
#'
#' @return A ggplot2 object
#'
#' @details
#' ## Interpreting Boxplots
#'
#' - Box: IQR (25th to 75th percentile)
#' - Line in box: Median error
#' - Whiskers: 1.5 × IQR
#' - Points: Outliers beyond whiskers
#'
#' Tighter boxes = more consistent estimates
#'
#' ## Usage Patterns
#'
#' ```r
#' # Single method
#' p <- medsim_plot_error_boxplot(results)
#'
#' # Compare methods
#' p <- medsim_plot_error_boxplot(list(
#'   Proposed = results1,
#'   Delta = results2,
#'   Bootstrap = results3
#' ))
#'
#' # Customize
#' p <- medsim_plot_error_boxplot(
#'   results,
#'   log_scale = TRUE,
#'   color_palette = "Set2"
#' )
#' p + ggplot2::theme_minimal()
#' ```
#'
#' @examples
#' \dontrun{
#' results <- medsim_run(method, scenarios, config)
#' p <- medsim_plot_error_boxplot(results)
#' print(p)
#'
#' # Save to file
#' ggplot2::ggsave("error_boxplot.pdf", p, width = 10, height = 6)
#' }
#'
#' @export
medsim_plot_error_boxplot <- function(results,
                                       parameter = "indirect",
                                       log_scale = FALSE,
                                       color_palette = NULL,
                                       title = NULL,
                                       ...) {

  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualization. Install with: install.packages('ggplot2')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for visualization. Install with: install.packages('dplyr')")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required for visualization. Install with: install.packages('tidyr')")
  }

  # --- Handle Multiple Methods ---
  if (is.list(results) && !inherits(results, "medsim_results")) {
    # List of results - compare methods
    method_names <- names(results)
    if (is.null(method_names) || any(method_names == "")) {
      stop("When providing multiple results, they must be named (e.g., list(method1 = res1, method2 = res2))")
    }

    plot_data_list <- list()
    for (i in seq_along(results)) {
      res <- results[[i]]
      method <- method_names[i]

      if (!inherits(res, "medsim_results")) {
        stop(sprintf("Element '%s' is not a medsim_results object", method))
      }

      # Extract error data
      merged <- merge(res$results, res$truth, by = "scenario", suffixes = c("_estimate", "_truth"))

      for (param in if (is.null(parameter)) setdiff(names(res$results), c("scenario", "replication", "elapsed")) else parameter) {
        estimate_col <- paste0(param, "_estimate")
        truth_col <- paste0(param, "_truth")

        if (!truth_col %in% names(merged)) next

        plot_data_list[[paste(method, param, sep = "_")]] <- data.frame(
          method = method,
          scenario = merged$scenario,
          parameter = param,
          error = abs(merged[[estimate_col]] - merged[[truth_col]]),
          stringsAsFactors = FALSE
        )
      }
    }

    plot_data <- do.call(rbind, plot_data_list)
    fill_var <- "method"

  } else {
    # Single method
    if (!inherits(results, "medsim_results")) {
      stop("results must be a medsim_results object or a named list of medsim_results objects")
    }

    if (is.null(results$truth) || nrow(results$truth) == 0) {
      stop("Cannot plot errors: no ground truth available")
    }

    # Merge with truth
    merged <- merge(results$results, results$truth, by = "scenario", suffixes = c("_estimate", "_truth"))

    plot_data_list <- list()
    for (param in if (is.null(parameter)) setdiff(names(results$results), c("scenario", "replication", "elapsed")) else parameter) {
      estimate_col <- paste0(param, "_estimate")
      truth_col <- paste0(param, "_truth")

      if (!truth_col %in% names(merged)) {
        warning(sprintf("No ground truth for parameter '%s', skipping", param))
        next
      }

      plot_data_list[[param]] <- data.frame(
        scenario = merged$scenario,
        parameter = param,
        error = abs(merged[[estimate_col]] - merged[[truth_col]]),
        stringsAsFactors = FALSE
      )
    }

    plot_data <- do.call(rbind, plot_data_list)
    fill_var <- "scenario"
  }

  # --- Create Plot ---
  if (nrow(plot_data) == 0) {
    stop("No plottable data available")
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = scenario, y = error, fill = .data[[fill_var]])) +
    ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
    ggplot2::labs(
      title = title %||% "Error Distribution by Scenario",
      x = "Scenario",
      y = if (log_scale) "Absolute Error (log scale)" else "Absolute Error",
      fill = if (fill_var == "method") "Method" else "Scenario"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      ...
    )

  # Apply log scale if requested
  if (log_scale) {
    p <- p + ggplot2::scale_y_log10()
  }

  # Apply color palette if requested
  if (!is.null(color_palette)) {
    if (color_palette == "viridis") {
      p <- p + ggplot2::scale_fill_viridis_d()
    } else if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      p <- p + ggplot2::scale_fill_brewer(palette = color_palette)
    }
  }

  # Facet by parameter if multiple parameters
  if ("parameter" %in% names(plot_data) && length(unique(plot_data$parameter)) > 1) {
    p <- p + ggplot2::facet_wrap(~ parameter, scales = "free_y")
  }

  return(p)
}

#' Plot Timing Comparison
#'
#' @description
#' Creates bar plots comparing computational time across methods and scenarios.
#'
#' @param results A named list of medsim_results objects (required for comparison)
#' @param metric Character: which timing metric to plot. Options: "mean", "median",
#'   "total" (default: "mean")
#' @param log_scale Logical: use log10 scale for y-axis (default: FALSE)
#' @param title Character: plot title (default: auto-generated)
#' @param ... Additional arguments passed to ggplot2::theme()
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' p <- medsim_plot_timing(list(
#'   Proposed = results1,
#'   Delta = results2,
#'   Bootstrap = results3
#' ))
#' }
#'
#' @export
medsim_plot_timing <- function(results,
                                metric = "mean",
                                log_scale = FALSE,
                                title = NULL,
                                ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  # Must be a list for comparison
  if (!is.list(results) || inherits(results, "medsim_results")) {
    stop("results must be a named list of medsim_results objects for timing comparison")
  }

  method_names <- names(results)
  if (is.null(method_names) || any(method_names == "")) {
    stop("All results must be named")
  }

  # Extract timing data
  timing_list <- list()
  for (i in seq_along(results)) {
    res <- results[[i]]
    method <- method_names[i]

    if (!"elapsed" %in% names(res$results)) {
      warning(sprintf("Method '%s' has no timing data, skipping", method))
      next
    }

    timing_by_scenario <- res$results %>%
      dplyr::group_by(scenario) %>%
      dplyr::summarise(
        mean_time = mean(elapsed, na.rm = TRUE),
        median_time = median(elapsed, na.rm = TRUE),
        total_time = sum(elapsed, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(method = method)

    timing_list[[method]] <- timing_by_scenario
  }

  timing_data <- do.call(rbind, timing_list)

  # Select metric
  time_col <- switch(metric,
    "mean" = "mean_time",
    "median" = "median_time",
    "total" = "total_time",
    stop("metric must be 'mean', 'median', or 'total'")
  )

  timing_data$time <- timing_data[[time_col]]

  # Create plot
  p <- ggplot2::ggplot(timing_data, ggplot2::aes(x = scenario, y = time, fill = method)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      title = title %||% sprintf("%s Execution Time by Method", tools::toTitleCase(metric)),
      x = "Scenario",
      y = sprintf("%s Time (seconds)", tools::toTitleCase(metric)),
      fill = "Method"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      ...
    )

  if (log_scale) {
    p <- p + ggplot2::scale_y_log10()
  }

  return(p)
}

#' Plot Coverage Rates
#'
#' @description
#' Creates a plot showing confidence interval coverage rates across scenarios
#' and methods.
#'
#' @param coverage A medsim_coverage object from [medsim_analyze_coverage()],
#'   or a named list of coverage objects for method comparison
#' @param expected_coverage Numeric: expected coverage rate to show as reference
#'   line (default: 0.95)
#' @param tolerance Numeric: tolerance band around expected coverage (default: 0.02)
#' @param title Character: plot title (default: auto-generated)
#' @param ... Additional arguments passed to ggplot2::theme()
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' coverage <- medsim_analyze_coverage(results)
#' p <- medsim_plot_coverage(coverage, expected_coverage = 0.95)
#' }
#'
#' @export
medsim_plot_coverage <- function(coverage,
                                  expected_coverage = 0.95,
                                  tolerance = 0.02,
                                  title = NULL,
                                  ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  # Handle multiple methods
  if (is.list(coverage) && !inherits(coverage, "medsim_coverage")) {
    method_names <- names(coverage)
    if (is.null(method_names) || any(method_names == "")) {
      stop("All coverage objects must be named")
    }

    plot_data_list <- list()
    for (i in seq_along(coverage)) {
      cov <- coverage[[i]]
      method <- method_names[i]

      if (!inherits(cov, "medsim_coverage")) {
        stop(sprintf("Element '%s' is not a medsim_coverage object", method))
      }

      if (!is.null(cov$by_scenario)) {
        cov_data <- cov$by_scenario
        cov_data$method <- method
        plot_data_list[[method]] <- cov_data
      }
    }

    plot_data <- do.call(rbind, plot_data_list)
    color_var <- "method"

  } else {
    if (!inherits(coverage, "medsim_coverage")) {
      stop("coverage must be a medsim_coverage object or named list of coverage objects")
    }

    if (is.null(coverage$by_scenario)) {
      # Use overall coverage
      plot_data <- coverage$coverage
      plot_data$scenario <- "Overall"
      color_var <- "parameter"
    } else {
      plot_data <- coverage$by_scenario
      color_var <- "parameter"
    }
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = scenario, y = coverage, color = .data[[color_var]])) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_hline(yintercept = expected_coverage, linetype = "dashed", color = "black") +
    ggplot2::geom_hline(yintercept = expected_coverage - tolerance, linetype = "dotted", color = "gray50") +
    ggplot2::geom_hline(yintercept = expected_coverage + tolerance, linetype = "dotted", color = "gray50") +
    ggplot2::ylim(max(0, expected_coverage - 0.1), min(1, expected_coverage + 0.1)) +
    ggplot2::labs(
      title = title %||% sprintf("Coverage Rates (Expected: %.1f%%)", expected_coverage * 100),
      x = "Scenario",
      y = "Coverage Rate",
      color = if (color_var == "method") "Method" else "Parameter"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      ...
    )

  return(p)
}

#' Create Combined Multi-Panel Figure
#'
#' @description
#' Creates a combined figure with multiple panels for manuscript submission.
#' Useful for showing multiple aspects of simulation results in one figure.
#'
#' @param results A medsim_results object or named list of results objects
#' @param panels Character vector: which panels to include. Options:
#'   "error", "timing", "coverage" (default: c("error", "timing"))
#' @param layout Character: layout specification like "2x1", "1x2", "2x2"
#'   (default: auto-determined from number of panels)
#' @param ... Additional arguments passed to individual plotting functions
#'
#' @return A combined plot object (patchwork or gridExtra)
#'
#' @details
#' Requires either `patchwork` or `gridExtra` package for combining plots.
#'
#' @examples
#' \dontrun{
#' # Two-panel figure
#' p <- medsim_plot_combined_panel(
#'   results,
#'   panels = c("error", "timing"),
#'   layout = "1x2"
#' )
#'
#' # Four-panel figure for manuscript
#' p <- medsim_plot_combined_panel(
#'   list(Proposed = res1, Delta = res2),
#'   panels = c("error", "timing", "coverage"),
#'   layout = "2x2"
#' )
#' }
#'
#' @export
medsim_plot_combined_panel <- function(results,
                                        panels = c("error", "timing"),
                                        layout = NULL,
                                        ...) {

  # Check for combining package
  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
  has_gridExtra <- requireNamespace("gridExtra", quietly = TRUE)

  if (!has_patchwork && !has_gridExtra) {
    stop("Either 'patchwork' or 'gridExtra' package required for combined panels. ",
         "Install with: install.packages('patchwork')")
  }

  # Generate individual plots
  plot_list <- list()

  if ("error" %in% panels) {
    plot_list$error <- medsim_plot_error_boxplot(results, ...) +
      ggplot2::ggtitle("(A) Error Distribution")
  }

  if ("timing" %in% panels) {
    if (is.list(results) && !inherits(results, "medsim_results")) {
      plot_list$timing <- medsim_plot_timing(results, ...) +
        ggplot2::ggtitle("(B) Computational Time")
    } else {
      warning("Timing comparison requires multiple methods, skipping timing panel")
    }
  }

  if ("coverage" %in% panels) {
    # Try to compute coverage
    if (is.list(results) && !inherits(results, "medsim_results")) {
      coverage_list <- lapply(results, function(r) {
        tryCatch(medsim_analyze_coverage(r), error = function(e) NULL)
      })
      coverage_list <- Filter(Negate(is.null), coverage_list)

      if (length(coverage_list) > 0) {
        plot_list$coverage <- medsim_plot_coverage(coverage_list, ...) +
          ggplot2::ggtitle("(C) Coverage Rates")
      } else {
        warning("No coverage data available, skipping coverage panel")
      }
    } else {
      cov <- tryCatch(medsim_analyze_coverage(results), error = function(e) NULL)
      if (!is.null(cov)) {
        plot_list$coverage <- medsim_plot_coverage(cov, ...) +
          ggplot2::ggtitle("(C) Coverage Rates")
      } else {
        warning("No coverage data available, skipping coverage panel")
      }
    }
  }

  if (length(plot_list) == 0) {
    stop("No plots could be generated")
  }

  # Combine plots
  if (has_patchwork) {
    # Use patchwork for better control
    if (is.null(layout)) {
      # Auto-layout
      combined <- patchwork::wrap_plots(plot_list, ncol = ceiling(sqrt(length(plot_list))))
    } else {
      # Parse layout (e.g., "2x2" -> 2 rows, 2 cols)
      dims <- as.numeric(strsplit(layout, "x")[[1]])
      combined <- patchwork::wrap_plots(plot_list, nrow = dims[1], ncol = dims[2])
    }
  } else {
    # Use gridExtra
    if (is.null(layout)) {
      combined <- gridExtra::grid.arrange(grobs = plot_list, ncol = ceiling(sqrt(length(plot_list))))
    } else {
      dims <- as.numeric(strsplit(layout, "x")[[1]])
      combined <- gridExtra::grid.arrange(grobs = plot_list, nrow = dims[1], ncol = dims[2])
    }
  }

  return(combined)
}

# Helper for NULL-coalescing (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
