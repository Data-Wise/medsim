# Standard Scenario Definitions for Mediation Analysis

#' Create Standard Mediation Scenarios
#'
#' @description
#' Creates a list of standard mediation scenarios for simulation studies. These
#' scenarios cover common patterns in mediation analysis including independent
#' paths, various correlation structures, suppression effects, and non-standard
#' conditions.
#'
#' @return A list of scenario objects, each with name, description, and
#'   data_generator function
#'
#' @details
#' ## Standard Scenarios
#'
#' 1. **Independent Paths**: No correlation between X, M, Y
#' 2. **Moderate Correlation**: rho = 0.3 between all pairs
#' 3. **High Correlation**: rho = 0.7 between all pairs
#' 4. **Suppression**: Mixed positive and negative correlations
#' 5. **Non-zero Effects**: Small to moderate true effects
#' 6. **Unequal Variances**: Different residual variances
#'
#' Each scenario generates data with:
#' - Sample size n (default: 200)
#' - Treatment X, Mediator M, Outcome Y
#' - Known population parameters for validation
#'
#' @examples
#' # Get all standard scenarios
#' scenarios <- medsim_scenarios_mediation()
#'
#' # See scenario names
#' sapply(scenarios, function(s) s$name)
#'
#' # Generate data from first scenario
#' data <- scenarios[[1]]$data_generator(n = 100)
#' head(data)
#'
#' # Access scenario parameters
#' scenarios[[1]]$params
#'
#' @seealso [medsim_scenario()] for creating custom scenarios
#'
#' @export
medsim_scenarios_mediation <- function() {
  scenarios <- list(

    # Scenario 1: Independent paths
    medsim_scenario(
      name = "Independent",
      description = "No correlation between X, M, Y",
      data_generator = function(n = 200) {
        X <- rnorm(n)
        M <- 0.3 * X + rnorm(n)
        Y <- 0.3 * M + rnorm(n)
        data.frame(X = X, M = M, Y = Y)
      },
      params = list(
        a = 0.3,
        b = 0.3,
        c_prime = 0,
        indirect = 0.09,
        rho_xm = 0,
        rho_my = 0,
        rho_xy = 0
      )
    ),

    # Scenario 2: Moderate correlation
    medsim_scenario(
      name = "Moderate Correlation",
      description = "Moderate positive correlation (rho = 0.3)",
      data_generator = function(n = 200) {
        # Generate correlated residuals
        Sigma <- matrix(c(1, 0.3, 0.3,
                          0.3, 1, 0.3,
                          0.3, 0.3, 1), nrow = 3)
        errors <- MASS::mvrnorm(n, mu = c(0, 0, 0), Sigma = Sigma)

        X <- rnorm(n) + errors[, 1]
        M <- 0.3 * X + errors[, 2]
        Y <- 0.3 * M + errors[, 3]
        data.frame(X = X, M = M, Y = Y)
      },
      params = list(
        a = 0.3,
        b = 0.3,
        c_prime = 0,
        indirect = 0.09,
        rho = 0.3
      )
    ),

    # Scenario 3: High correlation
    medsim_scenario(
      name = "High Correlation",
      description = "High positive correlation (rho = 0.7)",
      data_generator = function(n = 200) {
        Sigma <- matrix(c(1, 0.7, 0.7,
                          0.7, 1, 0.7,
                          0.7, 0.7, 1), nrow = 3)
        errors <- MASS::mvrnorm(n, mu = c(0, 0, 0), Sigma = Sigma)

        X <- rnorm(n) + errors[, 1]
        M <- 0.3 * X + errors[, 2]
        Y <- 0.3 * M + errors[, 3]
        data.frame(X = X, M = M, Y = Y)
      },
      params = list(
        a = 0.3,
        b = 0.3,
        c_prime = 0,
        indirect = 0.09,
        rho = 0.7
      )
    ),

    # Scenario 4: Suppression
    medsim_scenario(
      name = "Suppression",
      description = "Mixed positive and negative correlations",
      data_generator = function(n = 200) {
        Sigma <- matrix(c(1.0, 0.5, -0.3,
                          0.5, 1.0, 0.2,
                          -0.3, 0.2, 1.0), nrow = 3)
        errors <- MASS::mvrnorm(n, mu = c(0, 0, 0), Sigma = Sigma)

        X <- rnorm(n) + errors[, 1]
        M <- 0.5 * X + errors[, 2]
        Y <- 0.4 * M + errors[, 3]
        data.frame(X = X, M = M, Y = Y)
      },
      params = list(
        a = 0.5,
        b = 0.4,
        c_prime = 0,
        indirect = 0.20,
        rho_xm = 0.5,
        rho_my = 0.2,
        rho_xy = -0.3
      )
    ),

    # Scenario 5: Non-zero effects
    medsim_scenario(
      name = "Non-zero Effects",
      description = "Small to moderate true effects with direct path",
      data_generator = function(n = 200) {
        X <- rnorm(n)
        M <- 0.4 * X + rnorm(n)
        Y <- 0.2 * X + 0.5 * M + rnorm(n)  # Direct path c' = 0.2
        data.frame(X = X, M = M, Y = Y)
      },
      params = list(
        a = 0.4,
        b = 0.5,
        c_prime = 0.2,
        indirect = 0.20,
        total = 0.40
      )
    ),

    # Scenario 6: Unequal variances
    medsim_scenario(
      name = "Unequal Variances",
      description = "Different residual variances across equations",
      data_generator = function(n = 200) {
        X <- rnorm(n, sd = sqrt(2))
        M <- 0.3 * X + rnorm(n, sd = 1)
        Y <- 0.3 * M + rnorm(n, sd = sqrt(0.5))
        data.frame(X = X, M = M, Y = Y)
      },
      params = list(
        a = 0.3,
        b = 0.3,
        c_prime = 0,
        indirect = 0.09,
        var_x = 2.0,
        var_m = 1.0,
        var_y = 0.5
      )
    )
  )

  return(scenarios)
}

#' Create Custom Simulation Scenario
#'
#' @description
#' Define a custom scenario for simulation studies. A scenario consists of a
#' name, description, data generation function, and population parameters.
#'
#' @param name Character: Descriptive name for the scenario
#' @param description Character: Detailed description (optional)
#' @param data_generator Function: Takes n (sample size) and returns data.frame
#' @param params List: Known population parameters for validation
#'
#' @return A scenario object (list with class "medsim_scenario")
#'
#' @details
#' ## Data Generator Function
#'
#' The data_generator function must:
#' - Accept `n` (sample size) as first argument
#' - Return a data.frame with at least: X (treatment), M (mediator), Y (outcome)
#' - Can include additional variables or covariates
#'
#' ## Parameters List
#'
#' The params list should include known population values:
#' - `a`: X -> M path coefficient
#' - `b`: M -> Y path coefficient
#' - `c_prime`: X -> Y direct effect
#' - `indirect`: True indirect effect (a * b)
#' - Additional parameters as needed
#'
#' @examples
#' # Simple custom scenario
#' my_scenario <- medsim_scenario(
#'   name = "Large Effects",
#'   description = "Both paths have large effects",
#'   data_generator = function(n = 200) {
#'     X <- rnorm(n)
#'     M <- 0.7 * X + rnorm(n)
#'     Y <- 0.7 * M + rnorm(n)
#'     data.frame(X = X, M = M, Y = Y)
#'   },
#'   params = list(
#'     a = 0.7,
#'     b = 0.7,
#'     indirect = 0.49
#'   )
#' )
#'
#' # Generate data
#' data <- my_scenario$data_generator(n = 100)
#'
#' # Scenario with covariates
#' covariate_scenario <- medsim_scenario(
#'   name = "With Covariates",
#'   data_generator = function(n = 200) {
#'     C1 <- rnorm(n)
#'     C2 <- rbinom(n, 1, 0.5)
#'     X <- rnorm(n)
#'     M <- 0.3 * X + 0.2 * C1 + rnorm(n)
#'     Y <- 0.3 * M + 0.2 * C2 + rnorm(n)
#'     data.frame(X = X, M = M, Y = Y, C1 = C1, C2 = C2)
#'   },
#'   params = list(
#'     a = 0.3,
#'     b = 0.3,
#'     indirect = 0.09,
#'     gamma_m = 0.2,
#'     gamma_y = 0.2
#'   )
#' )
#'
#' @seealso [medsim_scenarios_mediation()] for standard scenarios
#'
#' @export
medsim_scenario <- function(name,
                            description = "",
                            data_generator,
                            params = list()) {

  # Validate inputs
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }

  if (!is.function(data_generator)) {
    stop("data_generator must be a function")
  }

  # Check that data_generator accepts n argument
  if (!"n" %in% names(formals(data_generator))) {
    warning("data_generator should accept 'n' (sample size) as an argument")
  }

  if (!is.list(params)) {
    stop("params must be a list")
  }

  # Create scenario object
  scenario <- list(
    name = name,
    description = description,
    data_generator = data_generator,
    params = params
  )

  class(scenario) <- c("medsim_scenario", "list")

  return(scenario)
}

#' Print Scenario Summary
#'
#' @param x A medsim_scenario object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.medsim_scenario <- function(x, ...) {
  cat("\n")
  cat("Scenario:", x$name, "\n")

  if (nchar(x$description) > 0) {
    cat("Description:", x$description, "\n")
  }

  cat("\nPopulation Parameters:\n")
  for (param_name in names(x$params)) {
    param_value <- x$params[[param_name]]
    if (is.numeric(param_value)) {
      cat(sprintf("  %-15s: %.4f\n", param_name, param_value))
    } else {
      cat(sprintf("  %-15s: %s\n", param_name, as.character(param_value)))
    }
  }

  cat("\nData Generator: function(n)\n")
  cat("\n")

  invisible(x)
}

#' Validate Scenario
#'
#' @description
#' Checks that a scenario object is valid and can generate data correctly.
#'
#' @param scenario A medsim_scenario object
#' @param n Sample size to test (default: 10)
#'
#' @return TRUE if valid, throws error otherwise
#'
#' @examples
#' scenarios <- medsim_scenarios_mediation()
#' medsim_validate_scenario(scenarios[[1]])
#'
#' @export
medsim_validate_scenario <- function(scenario, n = 10) {
  if (!inherits(scenario, "medsim_scenario")) {
    stop("scenario must be a medsim_scenario object")
  }

  # Try to generate data
  data <- tryCatch(
    scenario$data_generator(n = n),
    error = function(e) {
      stop(sprintf("data_generator failed: %s", e$message))
    }
  )

  # Check that data is a data.frame
  if (!is.data.frame(data)) {
    stop("data_generator must return a data.frame")
  }

  # Check for required columns
  required_cols <- c("X", "M", "Y")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(sprintf("data_generator must return columns: %s (missing: %s)",
                 paste(required_cols, collapse = ", "),
                 paste(missing_cols, collapse = ", ")))
  }

  # Check that data has correct number of rows
  if (nrow(data) != n) {
    stop(sprintf("data_generator returned %d rows, expected %d", nrow(data), n))
  }

  message(sprintf("* Scenario '%s' is valid", scenario$name))
  return(TRUE)
}
