#' Generate all standard figures from simulation results
#'
#' Convenience wrapper that produces the standard set of medsim plots
#' (coverage, error boxplot, timing) and writes them to `output_dir`.
#'
#' @param results A `medsim_results` object from [medsim_run()].
#' @param output_dir Directory to write figures to. Created if it doesn't exist.
#' @param format File extension passed to [ggplot2::ggsave()] (e.g. `"pdf"`, `"png"`).
#' @param width,height Figure dimensions in inches.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return Invisibly, a named character vector of file paths written.
#' @export
medsim_figures <- function(results,
                           output_dir = "figures",
                           format = "pdf",
                           width = 8,
                           height = 6,
                           ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  paths <- character()
  save_one <- function(name, plot) {
    path <- file.path(output_dir, paste0(name, ".", format))
    ggplot2::ggsave(path, plot, width = width, height = height, ...)
    paths[[name]] <<- path
  }

  coverage <- tryCatch(
    medsim_analyze_coverage(results),
    error = function(e) NULL
  )
  if (!is.null(coverage)) {
    save_one("coverage", medsim_plot_coverage(coverage))
  }

  save_one("error_boxplot", medsim_plot_error_boxplot(results))
  save_one("timing", medsim_plot_timing(results))

  invisible(paths)
}

#' Generate all standard LaTeX tables from simulation results
#'
#' Convenience wrapper around [medsim_tables_workflow()] with sensible defaults.
#'
#' @param results A `medsim_results` object from [medsim_run()].
#' @param output_dir Directory to write tables to. Created if it doesn't exist.
#' @param format Output format passed to [medsim_tables_workflow()] (e.g. `"latex"`, `"markdown"`).
#' @param ... Additional arguments passed to [medsim_tables_workflow()].
#'
#' @return Invisibly, the result of [medsim_tables_workflow()].
#' @export
medsim_tables <- function(results,
                          output_dir = "tables",
                          format = "latex",
                          ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  invisible(medsim_tables_workflow(results, output_dir = output_dir, format = format, ...))
}

#' Generate analysis, figures, and tables from simulation results
#'
#' End-to-end output pipeline: runs [medsim_analyze()], [medsim_figures()],
#' and [medsim_tables()] and writes everything to `output_dir`.
#'
#' @param results A `medsim_results` object from [medsim_run()].
#' @param output_dir Top-level directory for all outputs. Subdirectories
#'   `figures/` and `tables/` are created underneath. Created if it doesn't exist.
#' @param figures_format File format for figures (e.g. `"pdf"`, `"png"`).
#' @param tables_format Output format for tables (e.g. `"latex"`, `"markdown"`).
#' @param ... Additional arguments passed to [medsim_figures()].
#'
#' @return Invisibly, a list with elements `analysis`, `figures` (file paths),
#'   and `tables` (return value of [medsim_tables_workflow()]).
#' @export
#'
#' @examples
#' \dontrun{
#' config <- medsim_config("test")
#' scenarios <- medsim_scenarios_mediation()
#' results <- medsim_run(my_method, scenarios, config)
#' medsim_workflow(results, output_dir = "results")
#' }
medsim_workflow <- function(results,
                            output_dir = "results",
                            figures_format = "pdf",
                            tables_format = "latex",
                            ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  analysis <- medsim_analyze(results)
  figures <- medsim_figures(
    results,
    output_dir = file.path(output_dir, "figures"),
    format = figures_format,
    ...
  )
  tables <- medsim_tables(
    results,
    output_dir = file.path(output_dir, "tables"),
    format = tables_format
  )
  invisible(list(analysis = analysis, figures = figures, tables = tables))
}
