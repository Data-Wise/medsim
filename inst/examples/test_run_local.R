#!/usr/bin/env Rscript
################################################################################
# Test Run: Local Environment
#
# Purpose: Verify medsim works end-to-end on local machine
# Usage: Rscript test_run_local.R
################################################################################

# Load medsim
library(medsim)

cat("\n")
cat("========================================================\n")
cat("  MEDSIM TEST RUN - LOCAL ENVIRONMENT\n")
cat("========================================================\n\n")

# --- Step 1: Create Configuration ---
cat("[STEP 1] Creating test configuration...\n")

config <- medsim_config(
  mode = "test",  # Fast test mode: 20 reps, 4 cores
  seed = 12345
)

print(config)

# --- Step 2: Define Scenarios ---
cat("[STEP 2] Loading standard mediation scenarios...\n")

scenarios <- medsim_scenarios_mediation()
cat(sprintf("  ✓ Loaded %d scenarios\n\n", length(scenarios)))

# Use only first 2 scenarios for quick test
scenarios_test <- scenarios[1:2]

# --- Step 3: Define Simulation Method ---
cat("[STEP 3] Defining simulation method...\n")

# Simple mediation analysis using lm()
my_method <- function(data, params) {
  # Fit mediator model: M ~ X
  fit_m <- lm(M ~ X, data = data)

  # Fit outcome model: Y ~ X + M
  fit_y <- lm(Y ~ X + M, data = data)

  # Extract coefficients
  a <- coef(fit_m)["X"]
  b <- coef(fit_y)["M"]
  c_prime <- coef(fit_y)["X"]

  # Compute indirect effect
  indirect <- a * b

  # Simple bootstrap CI (small n_boot for speed)
  boot_results <- replicate(100, {
    # Resample data
    boot_idx <- sample(nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, ]

    # Refit models
    fit_m_boot <- lm(M ~ X, data = boot_data)
    fit_y_boot <- lm(Y ~ X + M, data = boot_data)

    # Compute indirect
    coef(fit_m_boot)["X"] * coef(fit_y_boot)["M"]
  })

  # Get bootstrap CI
  ci <- quantile(boot_results, c(0.025, 0.975))

  # Test indirect effect
  # Simple Sobel test approximation
  se_a <- summary(fit_m)$coefficients["X", "Std. Error"]
  se_b <- summary(fit_y)$coefficients["M", "Std. Error"]
  se_indirect <- sqrt(b^2 * se_a^2 + a^2 * se_b^2)
  z_stat <- indirect / se_indirect
  p_value <- 2 * (1 - pnorm(abs(z_stat)))

  # Return results
  list(
    indirect = indirect,
    a_path = a,
    b_path = b,
    c_prime = c_prime,
    indirect_ci_lower = ci[1],
    indirect_ci_upper = ci[2],
    indirect_p = p_value
  )
}

cat("  ✓ Method defined\n\n")

# --- Step 4: Define Ground Truth Function ---
cat("[STEP 4] Defining ground truth function...\n")

compute_truth <- function(data, params) {
  # Return known population parameters
  list(
    indirect = params$indirect,
    a_path = params$a,
    b_path = params$b,
    c_prime = params$c_prime %||% 0
  )
}

cat("  ✓ Ground truth function defined\n\n")

# --- Step 5: Run Simulation ---
cat("[STEP 5] Running simulation...\n\n")

results <- medsim_run(
  method = my_method,
  scenarios = scenarios_test,
  config = config,
  compute_truth = compute_truth,
  parallel = TRUE,
  verbose = TRUE
)

cat("\n")

# --- Step 6: Analyze Results ---
cat("[STEP 6] Analyzing results...\n")

# Accuracy analysis
analysis <- medsim_analyze(results, metrics = "all")
print(analysis)

# Coverage analysis
coverage <- medsim_analyze_coverage(results)
print(coverage)

# Power analysis
power <- medsim_analyze_power(results, alpha = 0.05)
print(power)

# --- Step 7: Summary ---
cat("\n")
cat("========================================================\n")
cat("  TEST RUN COMPLETE\n")
cat("========================================================\n\n")

cat("Summary:\n")
cat(sprintf("  Scenarios:       %d\n", length(unique(results$results$scenario))))
cat(sprintf("  Replications:    %d\n", max(results$results$replication)))
cat(sprintf("  Total runs:      %d\n", nrow(results$results)))
cat(sprintf("  Parameters:      %d\n", length(setdiff(names(results$results), c("scenario", "replication", "elapsed")))))
cat("\n")

cat("Accuracy:\n")
for (i in 1:nrow(analysis$accuracy)) {
  cat(sprintf("  %s:\n", analysis$accuracy$parameter[i]))
  cat(sprintf("    Bias:        %.6f\n", analysis$accuracy$bias[i]))
  cat(sprintf("    MAE:         %.6f\n", analysis$accuracy$mae[i]))
  cat(sprintf("    RMSE:        %.6f\n", analysis$accuracy$rmse[i]))
}
cat("\n")

cat("Coverage:\n")
for (i in 1:nrow(coverage$coverage)) {
  cat(sprintf("  %s: %.1f%% (n=%d)\n",
              coverage$coverage$parameter[i],
              coverage$coverage$coverage[i] * 100,
              coverage$coverage$n_valid[i]))
}
cat("\n")

cat("Power:\n")
for (i in 1:nrow(power$power)) {
  cat(sprintf("  %s: %.1f%% (alpha=%.2f, n=%d)\n",
              power$power$parameter[i],
              power$power$power[i] * 100,
              power$power$alpha[i],
              power$power$n_valid[i]))
}
cat("\n")

cat("✓ Test run completed successfully!\n\n")

# Helper for NULL-coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
