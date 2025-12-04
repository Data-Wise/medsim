# LaTeX Table Generation Functions
#
# Publication-ready table generation for simulation results
# Based on product-of-three patterns with booktabs formatting

# ==============================================================================
# INTERNAL FORMATTING HELPERS
# ==============================================================================

#' Format Number for LaTeX
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Character string with LaTeX math formatting
#' @keywords internal
.format_number_latex <- function(x, digits = 3) {
  if (is.na(x) || !is.finite(x)) {
    return("---")
  }
  sprintf("$%.*f$", digits, x)
}

#' Format Number in Scientific Notation for LaTeX
#'
#' @param x Numeric value
#' @param digits Number of significant digits in mantissa
#' @param threshold Use scientific notation if abs(x) < threshold
#' @return Character string with LaTeX scientific notation
#' @keywords internal
.format_scientific_latex <- function(x, digits = 1, threshold = 0.001) {
  if (is.na(x) || !is.finite(x)) {
    return("---")
  }

  if (abs(x) < threshold && x != 0) {
    exponent <- floor(log10(abs(x)))
    mantissa <- x / 10^exponent
    sprintf("$%.*f \\times 10^{%d}$", digits, mantissa, exponent)
  } else {
    sprintf("$%.*f$", digits + 2, x)
  }
}

#' Format Time for LaTeX
#'
#' Auto-scales to ms, s, or min based on magnitude
#'
#' @param seconds Time in seconds
#' @return Character string with appropriate time unit
#' @keywords internal
.format_time_latex <- function(seconds) {
  if (is.na(seconds) || !is.finite(seconds)) {
    return("---")
  }

  if (seconds < 0.001) {
    # Microseconds
    sprintf("$%.1f$ $\\mu$s", seconds * 1e6)
  } else if (seconds < 1) {
    # Milliseconds
    sprintf("$%.1f$ ms", seconds * 1000)
  } else if (seconds < 60) {
    # Seconds
    sprintf("$%.2f$ s", seconds)
  } else if (seconds < 3600) {
    # Minutes
    sprintf("$%.1f$ min", seconds / 60)
  } else {
    # Hours
    sprintf("$%.1f$ h", seconds / 3600)
  }
}

#' Format Percentage for LaTeX
#'
#' @param x Numeric value (proportion 0-1 or percentage 0-100)
#' @param digits Decimal places
#' @param as_proportion If TRUE, x is 0-1; if FALSE, x is 0-100
#' @return Character string with percentage
#' @keywords internal
.format_percentage_latex <- function(x, digits = 1, as_proportion = TRUE) {
  if (is.na(x) || !is.finite(x)) {
    return("---")
  }

  if (as_proportion) {
    x <- x * 100
  }

  sprintf("$%.*f\\%%$", digits, x)
}

#' Format P-value for LaTeX
#'
#' @param p P-value
#' @param digits Decimal places for exact values
#' @return Character string with p-value
#' @keywords internal
.format_pvalue_latex <- function(p, digits = 3) {
  if (is.na(p) || !is.finite(p)) {
    return("---")
  }

  if (p < 0.001) {
    "$< 0.001$"
  } else if (p < 0.01) {
    sprintf("$%.3f$", p)
  } else {
    sprintf("$%.*f$", digits, p)
  }
}

#' Format Speedup for LaTeX
#'
#' @param x Speedup factor
#' @param digits Decimal places
#' @return Character string with times symbol
#' @keywords internal
.format_speedup_latex <- function(x, digits = 1) {
  if (is.na(x) || !is.finite(x)) {
    return("---")
  }

  sprintf("$%.*f\\times$", digits, x)
}

# ==============================================================================
# MAIN TABLE GENERATION FUNCTIONS
# ==============================================================================

#' Generate Accuracy Table
#'
#' @description
#' Creates a publication-ready LaTeX table showing accuracy metrics
#' (bias, MAE, RMSE) from simulation analysis results.
#'
#' @param analysis A medsim_analysis object from [medsim_analyze()]
#' @param digits Integer: number of decimal places (default: 3)
#' @param metrics Character vector: which metrics to include. Options:
#'   "bias", "mae", "rmse", "median_ae", "max_ae", "relative_bias"
#'   (default: c("bias", "mae", "rmse"))
#' @param by_scenario Logical: include scenario breakdown (default: TRUE)
#' @param caption Character: table caption (default: auto-generated)
#' @param label Character: LaTeX label for cross-referencing (default: "tab:accuracy")
#'
#' @return A medsim_table object (character vector with LaTeX code)
#'
#' @examples
#' \dontrun{
#' results <- medsim_run(method, scenarios, config)
#' analysis <- medsim_analyze(results)
#'
#' # Generate table
#' table <- medsim_table_accuracy(analysis)
#' print(table)  # Preview
#'
#' # Write to file
#' medsim_write_table(table, "tables/accuracy.tex")
#' }
#'
#' @export
medsim_table_accuracy <- function(analysis,
                                   digits = 3,
                                   metrics = c("bias", "mae", "rmse"),
                                   by_scenario = TRUE,
                                   caption = NULL,
                                   label = "tab:accuracy") {

  # --- Input Validation ---
  if (!inherits(analysis, "medsim_analysis")) {
    stop("analysis must be a medsim_analysis object from medsim_analyze()")
  }

  valid_metrics <- c("bias", "mae", "rmse", "median_ae", "max_ae", "relative_bias")
  invalid <- setdiff(metrics, valid_metrics)
  if (length(invalid) > 0) {
    stop("Invalid metrics: ", paste(invalid, collapse = ", "))
  }

  # --- Prepare Data ---
  if (by_scenario && !is.null(analysis$by_scenario)) {
    data <- analysis$by_scenario
  } else {
    data <- analysis$accuracy
  }

  # --- Build Table ---
  lines <- character()

  # Header
  lines <- c(lines, "\\begin{table}[ht]")

  lines <- c(lines, "\\centering")

  if (is.null(caption)) {
    caption <- "Accuracy Metrics from Simulation Study"
  }
  lines <- c(lines, sprintf("\\caption{%s}", caption))
  lines <- c(lines, sprintf("\\label{%s}", label))

  # Column specification
  n_metrics <- length(metrics)
  col_spec <- paste0("l", paste(rep("c", n_metrics), collapse = ""))
  if (by_scenario && "scenario" %in% names(data)) {
    col_spec <- paste0("l", col_spec)
  }
  lines <- c(lines, sprintf("\\begin{tabular}{%s}", col_spec))
  lines <- c(lines, "\\toprule")

  # Header row
  metric_labels <- c(
    bias = "Bias",
    mae = "MAE",
    rmse = "RMSE",
    median_ae = "Median AE",
    max_ae = "Max AE",
    relative_bias = "Rel. Bias (\\%)"
  )

  header_parts <- c()
  if (by_scenario && "scenario" %in% names(data)) {
    header_parts <- c(header_parts, "Scenario")
  }
  header_parts <- c(header_parts, "Parameter")
  header_parts <- c(header_parts, metric_labels[metrics])

  lines <- c(lines, paste0(paste(header_parts, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  # Data rows
  for (i in seq_len(nrow(data))) {
    row_parts <- c()

    if (by_scenario && "scenario" %in% names(data)) {
      row_parts <- c(row_parts, data$scenario[i])
    }

    row_parts <- c(row_parts, data$parameter[i])

    for (metric in metrics) {
      if (metric %in% names(data)) {
        value <- data[[metric]][i]
        if (is.na(value) || !is.finite(value)) {
          row_parts <- c(row_parts, "---")
        } else if (metric == "relative_bias") {
          # Already in percentage
          row_parts <- c(row_parts, .format_number_latex(value, digits = 1))
        } else if (abs(value) < 0.001 && value != 0) {
          row_parts <- c(row_parts, .format_scientific_latex(value, digits = 1))
        } else {
          row_parts <- c(row_parts, .format_number_latex(value, digits = digits))
        }
      } else {
        row_parts <- c(row_parts, "---")
      }
    }

    lines <- c(lines, paste0(paste(row_parts, collapse = " & "), " \\\\"))
  }

  # Footer
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  # Create object
  table <- structure(
    lines,
    class = c("medsim_table", "character"),
    type = "accuracy",
    caption = caption,
    label = label
  )

  return(table)
}

#' Generate Timing Comparison Table
#'
#' @description
#' Creates a publication-ready LaTeX table comparing computational time
#' across methods.
#'
#' @param results_list Named list of medsim_results objects, one per method
#' @param metric Character: timing metric to use ("mean", "median", "total")
#' @param include_speedup Logical: include speedup column relative to slowest method
#' @param reference_method Character: method name to use as reference for speedup
#'   (default: NULL uses slowest method)
#' @param by_scenario Logical: show timing by scenario (default: FALSE)
#' @param caption Character: table caption
#' @param label Character: LaTeX label
#'
#' @return A medsim_table object
#'
#' @examples
#' \dontrun{
#' table <- medsim_table_timing(list(
#'   Proposed = results1,
#'   Delta = results2,
#'   Bootstrap = results3
#' ))
#' }
#'
#' @export
medsim_table_timing <- function(results_list,
                                 metric = "mean",
                                 include_speedup = TRUE,
                                 reference_method = NULL,
                                 by_scenario = FALSE,
                                 caption = NULL,
                                 label = "tab:timing") {

  # --- Input Validation ---
  if (!is.list(results_list) || length(results_list) < 1) {
    stop("results_list must be a named list of medsim_results objects")
  }

  method_names <- names(results_list)
  if (is.null(method_names) || any(method_names == "")) {
    stop("All results must be named")
  }

  metric <- match.arg(metric, c("mean", "median", "total"))

  # --- Compute Timing ---
  timing_data <- list()

  for (method in method_names) {
    res <- results_list[[method]]

    if (!inherits(res, "medsim_results")) {
      stop(sprintf("'%s' is not a medsim_results object", method))
    }

    if (!"elapsed" %in% names(res$results)) {
      warning(sprintf("'%s' has no timing data, using NA", method))
      timing_data[[method]] <- NA
      next
    }

    elapsed <- res$results$elapsed
    elapsed <- elapsed[!is.na(elapsed)]

    if (length(elapsed) == 0) {
      timing_data[[method]] <- NA
    } else if (metric == "mean") {
      timing_data[[method]] <- mean(elapsed)
    } else if (metric == "median") {
      timing_data[[method]] <- median(elapsed)
    } else {
      timing_data[[method]] <- sum(elapsed)
    }
  }

  # Convert to vector
  times <- unlist(timing_data)

  # Compute speedup
  if (include_speedup) {
    if (is.null(reference_method)) {
      # Use slowest method
      reference_time <- max(times, na.rm = TRUE)
    } else {
      if (!reference_method %in% method_names) {
        stop(sprintf("Reference method '%s' not found", reference_method))
      }
      reference_time <- times[reference_method]
    }
    speedup <- reference_time / times
  }

  # --- Build Table ---
  lines <- character()

  lines <- c(lines, "\\begin{table}[ht]")
  lines <- c(lines, "\\centering")

  if (is.null(caption)) {
    caption <- sprintf("Computational Time Comparison (%s)", tools::toTitleCase(metric))
  }
  lines <- c(lines, sprintf("\\caption{%s}", caption))
  lines <- c(lines, sprintf("\\label{%s}", label))

  # Column specification
  col_spec <- if (include_speedup) "lcc" else "lc"
  lines <- c(lines, sprintf("\\begin{tabular}{%s}", col_spec))
  lines <- c(lines, "\\toprule")

  # Header
  header <- if (include_speedup) {
    sprintf("Method & %s Time & Speedup \\\\", tools::toTitleCase(metric))
  } else {
    sprintf("Method & %s Time \\\\", tools::toTitleCase(metric))
  }
  lines <- c(lines, header)
  lines <- c(lines, "\\midrule")

  # Data rows
  for (method in method_names) {
    time_str <- .format_time_latex(times[method])

    if (include_speedup) {
      speedup_str <- .format_speedup_latex(speedup[method], digits = 1)
      lines <- c(lines, sprintf("%s & %s & %s \\\\", method, time_str, speedup_str))
    } else {
      lines <- c(lines, sprintf("%s & %s \\\\", method, time_str))
    }
  }

  # Footer
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  table <- structure(
    lines,
    class = c("medsim_table", "character"),
    type = "timing",
    caption = caption,
    label = label
  )

  return(table)
}

#' Generate Coverage Table
#'
#' @description
#' Creates a publication-ready LaTeX table showing confidence interval
#' coverage rates.
#'
#' @param coverage A medsim_coverage object from [medsim_analyze_coverage()]
#' @param expected Numeric: expected/nominal coverage rate (default: 0.95)
#' @param by_scenario Logical: show coverage by scenario (default: TRUE)
#' @param caption Character: table caption
#' @param label Character: LaTeX label
#'
#' @return A medsim_table object
#'
#' @export
medsim_table_coverage <- function(coverage,
                                   expected = 0.95,
                                   by_scenario = TRUE,
                                   caption = NULL,
                                   label = "tab:coverage") {

  # --- Input Validation ---
  if (!inherits(coverage, "medsim_coverage")) {
    stop("coverage must be a medsim_coverage object from medsim_analyze_coverage()")
  }

  # --- Prepare Data ---
  if (by_scenario && !is.null(coverage$by_scenario)) {
    data <- coverage$by_scenario
  } else {
    data <- coverage$coverage
  }

  # --- Build Table ---
  lines <- character()

  lines <- c(lines, "\\begin{table}[ht]")
  lines <- c(lines, "\\centering")

  if (is.null(caption)) {
    caption <- sprintf("Confidence Interval Coverage Rates (Nominal: %.0f\\%%)", expected * 100)
  }
  lines <- c(lines, sprintf("\\caption{%s}", caption))
  lines <- c(lines, sprintf("\\label{%s}", label))

  # Column specification
  col_spec <- if (by_scenario && "scenario" %in% names(data)) "llcc" else "lcc"
  lines <- c(lines, sprintf("\\begin{tabular}{%s}", col_spec))
  lines <- c(lines, "\\toprule")

  # Header
  header_parts <- c()
  if (by_scenario && "scenario" %in% names(data)) {
    header_parts <- c(header_parts, "Scenario")
  }
  header_parts <- c(header_parts, "Parameter", "Coverage", "$n$")
  lines <- c(lines, paste0(paste(header_parts, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  # Data rows
  for (i in seq_len(nrow(data))) {
    row_parts <- c()

    if (by_scenario && "scenario" %in% names(data)) {
      row_parts <- c(row_parts, data$scenario[i])
    }

    row_parts <- c(row_parts, data$parameter[i])
    row_parts <- c(row_parts, .format_percentage_latex(data$coverage[i], digits = 1))
    row_parts <- c(row_parts, sprintf("$%d$", data$n_valid[i]))

    lines <- c(lines, paste0(paste(row_parts, collapse = " & "), " \\\\"))
  }

  # Footer
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  table <- structure(
    lines,
    class = c("medsim_table", "character"),
    type = "coverage",
    caption = caption,
    label = label
  )

  return(table)
}

#' Generate Power Table
#'
#' @description
#' Creates a publication-ready LaTeX table showing empirical power
#' (rejection rates) from simulation analysis.
#'
#' @param power A medsim_power object from [medsim_analyze_power()]
#' @param by_scenario Logical: show power by scenario (default: TRUE)
#' @param caption Character: table caption
#' @param label Character: LaTeX label
#'
#' @return A medsim_table object
#'
#' @export
medsim_table_power <- function(power,
                                by_scenario = TRUE,
                                caption = NULL,
                                label = "tab:power") {

  # --- Input Validation ---
  if (!inherits(power, "medsim_power")) {
    stop("power must be a medsim_power object from medsim_analyze_power()")
  }

  # --- Prepare Data ---
  if (by_scenario && !is.null(power$by_scenario)) {
    data <- power$by_scenario
  } else {
    data <- power$power
  }

  alpha <- power$alpha

  # --- Build Table ---
  lines <- character()

  lines <- c(lines, "\\begin{table}[ht]")
  lines <- c(lines, "\\centering")

  if (is.null(caption)) {
    caption <- sprintf("Empirical Power ($\\alpha = %.2f$)", alpha)
  }
  lines <- c(lines, sprintf("\\caption{%s}", caption))
  lines <- c(lines, sprintf("\\label{%s}", label))

  # Column specification
  col_spec <- if (by_scenario && "scenario" %in% names(data)) "llcc" else "lcc"
  lines <- c(lines, sprintf("\\begin{tabular}{%s}", col_spec))
  lines <- c(lines, "\\toprule")

  # Header
  header_parts <- c()
  if (by_scenario && "scenario" %in% names(data)) {
    header_parts <- c(header_parts, "Scenario")
  }
  header_parts <- c(header_parts, "Parameter", "Power", "$n$")
  lines <- c(lines, paste0(paste(header_parts, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  # Data rows
  for (i in seq_len(nrow(data))) {
    row_parts <- c()

    if (by_scenario && "scenario" %in% names(data)) {
      row_parts <- c(row_parts, data$scenario[i])
    }

    row_parts <- c(row_parts, data$parameter[i])
    row_parts <- c(row_parts, .format_percentage_latex(data$power[i], digits = 1))
    row_parts <- c(row_parts, sprintf("$%d$", data$n_valid[i]))

    lines <- c(lines, paste0(paste(row_parts, collapse = " & "), " \\\\"))
  }

  # Footer
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  table <- structure(
    lines,
    class = c("medsim_table", "character"),
    type = "power",
    caption = caption,
    label = label
  )

  return(table)
}

#' Generate Method Comparison Table
#'
#' @description
#' Creates a publication-ready LaTeX table comparing multiple methods
#' across accuracy, timing, and coverage metrics.
#'
#' @param comparison A medsim_comparison object from [medsim_compare_methods()]
#' @param metrics Character vector: which metrics to include
#'   ("accuracy", "timing", "coverage")
#' @param caption Character: table caption
#' @param label Character: LaTeX label
#'
#' @return A medsim_table object
#'
#' @export
medsim_table_comparison <- function(comparison,
                                     metrics = c("accuracy", "timing"),
                                     caption = NULL,
                                     label = "tab:comparison") {

  # --- Input Validation ---
  if (!inherits(comparison, "medsim_comparison")) {
    stop("comparison must be a medsim_comparison object from medsim_compare_methods()")
  }

  # --- Build Table ---
  lines <- character()

  lines <- c(lines, "\\begin{table}[ht]")
  lines <- c(lines, "\\centering")

  if (is.null(caption)) {
    caption <- "Method Comparison Summary"
  }
  lines <- c(lines, sprintf("\\caption{%s}", caption))
  lines <- c(lines, sprintf("\\label{%s}", label))

  # Determine columns based on available metrics
  col_parts <- c("Method")

  if ("accuracy" %in% metrics && !is.null(comparison$accuracy_comparison)) {
    col_parts <- c(col_parts, "MAE", "RMSE", "Bias")
  }
  if ("timing" %in% metrics && !is.null(comparison$timing_comparison)) {
    col_parts <- c(col_parts, "Mean Time")
  }
  if ("coverage" %in% metrics && !is.null(comparison$coverage_comparison)) {
    col_parts <- c(col_parts, "Coverage")
  }

  col_spec <- paste0("l", paste(rep("c", length(col_parts) - 1), collapse = ""))
  lines <- c(lines, sprintf("\\begin{tabular}{%s}", col_spec))
  lines <- c(lines, "\\toprule")

  # Header
  lines <- c(lines, paste0(paste(col_parts, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\midrule")

  # Get unique methods
  methods <- unique(c(
    if (!is.null(comparison$accuracy_comparison)) comparison$accuracy_comparison$method,
    if (!is.null(comparison$timing_comparison)) comparison$timing_comparison$method,
    if (!is.null(comparison$coverage_comparison)) comparison$coverage_comparison$method
  ))

  # Data rows
  for (method in methods) {
    row_parts <- c(method)

    # Accuracy columns
    if ("accuracy" %in% metrics && !is.null(comparison$accuracy_comparison)) {
      acc_data <- comparison$accuracy_comparison
      method_acc <- acc_data[acc_data$method == method, ]

      if (nrow(method_acc) > 0) {
        # Average across parameters if multiple
        row_parts <- c(row_parts, .format_scientific_latex(mean(method_acc$mae, na.rm = TRUE)))
        row_parts <- c(row_parts, .format_scientific_latex(mean(method_acc$rmse, na.rm = TRUE)))
        row_parts <- c(row_parts, .format_scientific_latex(mean(method_acc$bias, na.rm = TRUE)))
      } else {
        row_parts <- c(row_parts, "---", "---", "---")
      }
    }

    # Timing column
    if ("timing" %in% metrics && !is.null(comparison$timing_comparison)) {
      timing_data <- comparison$timing_comparison
      method_time <- timing_data[timing_data$method == method, ]

      if (nrow(method_time) > 0) {
        row_parts <- c(row_parts, .format_time_latex(method_time$mean_time[1]))
      } else {
        row_parts <- c(row_parts, "---")
      }
    }

    # Coverage column
    if ("coverage" %in% metrics && !is.null(comparison$coverage_comparison)) {
      cov_data <- comparison$coverage_comparison
      method_cov <- cov_data[cov_data$method == method, ]

      if (nrow(method_cov) > 0) {
        row_parts <- c(row_parts, .format_percentage_latex(mean(method_cov$coverage, na.rm = TRUE)))
      } else {
        row_parts <- c(row_parts, "---")
      }
    }

    lines <- c(lines, paste0(paste(row_parts, collapse = " & "), " \\\\"))
  }

  # Footer
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  table <- structure(
    lines,
    class = c("medsim_table", "character"),
    type = "comparison",
    caption = caption,
    label = label
  )

  return(table)
}

# ==============================================================================
# OUTPUT FUNCTIONS
# ==============================================================================

#' Write Table to File
#'
#' @description
#' Writes a medsim_table object to a file in LaTeX, CSV, or Markdown format.
#'
#' @param table A medsim_table object
#' @param file Character: output file path
#' @param format Character: output format ("latex", "csv", "markdown")
#'   If NULL, inferred from file extension.
#'
#' @return Invisibly returns the file path
#'
#' @examples
#' \dontrun{
#' table <- medsim_table_accuracy(analysis)
#'
#' # Write LaTeX
#' medsim_write_table(table, "tables/accuracy.tex")
#'
#' # Write CSV
#' medsim_write_table(table, "tables/accuracy.csv", format = "csv")
#' }
#'
#' @export
medsim_write_table <- function(table, file, format = NULL) {

  if (!inherits(table, "medsim_table")) {
    stop("table must be a medsim_table object")
  }

  # Infer format from extension
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext,
      "tex" = "latex",
      "csv" = "csv",
      "md" = "markdown",
      "latex"  # default
    )
  }

  format <- match.arg(format, c("latex", "csv", "markdown"))

  # Create directory if needed
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)

  if (format == "latex") {
    # Write LaTeX directly
    writeLines(table, file)
  } else if (format == "csv") {
    # Convert to CSV (strip LaTeX formatting)
    warning("CSV export strips LaTeX formatting - some information may be lost")
    # Simple conversion: remove LaTeX commands and math mode
    csv_lines <- gsub("\\$|\\\\times|\\\\%|\\\\toprule|\\\\midrule|\\\\bottomrule|\\\\\\\\", "", table)
    csv_lines <- csv_lines[!grepl("^\\\\|^$", csv_lines)]  # Remove LaTeX commands
    csv_lines <- gsub(" & ", ",", csv_lines)  # Convert separators
    writeLines(csv_lines, file)
  } else {
    # Markdown
    warning("Markdown export is basic - consider using knitr::kable for better output")
    md_lines <- gsub("\\$|\\\\times|\\\\%", "", table)
    md_lines <- md_lines[!grepl("^\\\\begin|^\\\\end|^\\\\centering|^\\\\caption|^\\\\label|^\\\\toprule|^\\\\midrule|^\\\\bottomrule", md_lines)]
    md_lines <- gsub(" & ", " | ", md_lines)
    md_lines <- gsub("\\\\\\\\$", "", md_lines)
    writeLines(md_lines, file)
  }

  message(sprintf("Table written to: %s", file))
  invisible(file)
}

#' Generate All Tables
#'
#' @description
#' Convenience function to generate all tables from simulation results
#' and save them to an output directory.
#'
#' @param results A medsim_results object from [medsim_run()]
#' @param output_dir Character: output directory for tables
#' @param tables Character vector: which tables to generate
#'   ("accuracy", "coverage", "power", "all")
#' @param format Character: output format ("latex", "csv", "markdown")
#' @param prefix Character: prefix for file names (default: "table_")
#'
#' @return Invisibly returns a list of generated file paths
#'
#' @examples
#' \dontrun{
#' results <- medsim_run(method, scenarios, config)
#'
#' # Generate all tables
#' files <- medsim_tables_workflow(
#'   results,
#'   output_dir = "manuscript/tables",
#'   format = "latex"
#' )
#' }
#'
#' @export
medsim_tables_workflow <- function(results,
                                    output_dir,
                                    tables = "all",
                                    format = "latex",
                                    prefix = "table_") {

  # --- Input Validation ---
  if (!inherits(results, "medsim_results")) {
    stop("results must be a medsim_results object from medsim_run()")
  }

  if (length(tables) == 1 && tables == "all") {
    tables <- c("accuracy", "coverage", "power")
  }

  format <- match.arg(format, c("latex", "csv", "markdown"))

  ext <- switch(format, latex = "tex", csv = "csv", markdown = "md")

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  files_generated <- list()

  # --- Accuracy Table ---
  if ("accuracy" %in% tables) {
    tryCatch({
      analysis <- medsim_analyze(results)
      table <- medsim_table_accuracy(analysis)
      file <- file.path(output_dir, paste0(prefix, "accuracy.", ext))
      medsim_write_table(table, file, format = format)
      files_generated$accuracy <- file
    }, error = function(e) {
      warning(sprintf("Could not generate accuracy table: %s", e$message))
    })
  }

  # --- Coverage Table ---
  if ("coverage" %in% tables) {
    tryCatch({
      coverage <- medsim_analyze_coverage(results)
      table <- medsim_table_coverage(coverage)
      file <- file.path(output_dir, paste0(prefix, "coverage.", ext))
      medsim_write_table(table, file, format = format)
      files_generated$coverage <- file
    }, error = function(e) {
      warning(sprintf("Could not generate coverage table: %s", e$message))
    })
  }

  # --- Power Table ---
  if ("power" %in% tables) {
    tryCatch({
      power <- medsim_analyze_power(results)
      table <- medsim_table_power(power)
      file <- file.path(output_dir, paste0(prefix, "power.", ext))
      medsim_write_table(table, file, format = format)
      files_generated$power <- file
    }, error = function(e) {
      warning(sprintf("Could not generate power table: %s", e$message))
    })
  }

  message(sprintf("\nGenerated %d tables in: %s", length(files_generated), output_dir))

  invisible(files_generated)
}

# ==============================================================================
# PRINT METHOD
# ==============================================================================

#' Print medsim_table
#'
#' @param x A medsim_table object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_table <- function(x, ...) {
  cat("\n")
  cat("medsim LaTeX Table\n")
  cat("==================\n")
  cat(sprintf("Type:    %s\n", attr(x, "type")))
  cat(sprintf("Label:   %s\n", attr(x, "label")))
  cat("\n")
  cat("LaTeX Code:\n")
  cat("----------\n")
  cat(x, sep = "\n")
  cat("\n")

  invisible(x)
}

