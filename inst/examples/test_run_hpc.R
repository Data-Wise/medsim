#!/usr/bin/env Rscript
################################################################################
# Test Run: HPC/Cluster Environment
#
# Purpose: Verify medsim works end-to-end on HPC cluster
# Usage: sbatch test_run_hpc.sh  (or directly: Rscript test_run_hpc.R)
#
# This script will:
# - Auto-detect cluster environment
# - Use cluster-appropriate settings
# - Run larger simulation (1000 reps)
# - Save results to files
################################################################################

# Load medsim
library(medsim)

cat("\n")
cat("========================================================\n")
cat("  MEDSIM TEST RUN - HPC ENVIRONMENT\n")
cat("========================================================\n\n")

# --- Step 1: Auto-Detect Environment ---
cat("[STEP 1] Detecting environment...\n")

config <- medsim_config(
  mode = "auto",  # Auto-detect cluster vs local
  seed = 12345,
  output_dir = "simulation_results"
)

print(config)

# --- Step 2: Define Scenarios ---
cat("[STEP 2] Loading standard mediation scenarios...\n")

scenarios <- medsim_scenarios_mediation()
cat(sprintf("  ✓ Loaded %d scenarios\n\n", length(scenarios)))

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

  # Bootstrap CI (moderate n_boot for HPC)
  boot_results <- replicate(500, {
    boot_idx <- sample(nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, ]

    fit_m_boot <- lm(M ~ X, data = boot_data)
    fit_y_boot <- lm(Y ~ X + M, data = boot_data)

    coef(fit_m_boot)["X"] * coef(fit_y_boot)["M"]
  })

  ci <- quantile(boot_results, c(0.025, 0.975))

  # Sobel test
  se_a <- summary(fit_m)$coefficients["X", "Std. Error"]
  se_b <- summary(fit_y)$coefficients["M", "Std. Error"]
  se_indirect <- sqrt(b^2 * se_a^2 + a^2 * se_b^2)
  z_stat <- indirect / se_indirect
  p_value <- 2 * (1 - pnorm(abs(z_stat)))

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
  list(
    indirect = params$indirect,
    a_path = params$a,
    b_path = params$b,
    c_prime = params$c_prime %||% 0
  )
}

cat("  ✓ Ground truth function defined\n\n")

# --- Step 5: Run Simulation ---
cat("[STEP 5] Running simulation...\n")
cat(sprintf("  Mode: %s\n", config$mode))
cat(sprintf("  Replications: %d\n", config$n_replications))
cat(sprintf("  Cores: %d\n", config$n_cores))
cat(sprintf("  Scenarios: %d\n", length(scenarios)))
cat("\n")

start_time <- Sys.time()

results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = config,
  compute_truth = compute_truth,
  parallel = TRUE,
  verbose = TRUE
)

end_time <- Sys.time()
elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n")
cat(sprintf("  ✓ Simulation completed in %.1f seconds (%.1f minutes)\n\n",
            elapsed, elapsed / 60))

# --- Step 6: Analyze Results ---
cat("[STEP 6] Analyzing results...\n")

# Accuracy analysis
analysis <- medsim_analyze(results, metrics = "all", by_scenario = TRUE)

# Coverage analysis
coverage <- medsim_analyze_coverage(results, by_scenario = TRUE)

# Power analysis
power <- medsim_analyze_power(results, alpha = 0.05, by_scenario = TRUE)

cat("  ✓ Analysis complete\n\n")

# --- Step 7: Save Results ---
cat("[STEP 7] Saving results...\n")

# Create output directory
output_dir <- config$output_dir
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save RDS files
saveRDS(results, file.path(output_dir, "simulation_results.rds"))
saveRDS(analysis, file.path(output_dir, "analysis_results.rds"))
saveRDS(coverage, file.path(output_dir, "coverage_results.rds"))
saveRDS(power, file.path(output_dir, "power_results.rds"))

# Save CSV files
write.csv(results$results, file.path(output_dir, "all_results.csv"), row.names = FALSE)
write.csv(results$summary, file.path(output_dir, "summary_stats.csv"), row.names = FALSE)
write.csv(results$truth, file.path(output_dir, "ground_truth.csv"), row.names = FALSE)

write.csv(analysis$accuracy, file.path(output_dir, "accuracy_overall.csv"), row.names = FALSE)
write.csv(analysis$by_scenario, file.path(output_dir, "accuracy_by_scenario.csv"), row.names = FALSE)

write.csv(coverage$coverage, file.path(output_dir, "coverage_overall.csv"), row.names = FALSE)
write.csv(coverage$by_scenario, file.path(output_dir, "coverage_by_scenario.csv"), row.names = FALSE)

write.csv(power$power, file.path(output_dir, "power_overall.csv"), row.names = FALSE)
write.csv(power$by_scenario, file.path(output_dir, "power_by_scenario.csv"), row.names = FALSE)

cat(sprintf("  ✓ Results saved to: %s/\n\n", output_dir))

# --- Step 8: Summary Report ---
cat("[STEP 8] Generating summary report...\n")

report_file <- file.path(output_dir, "SUMMARY.txt")

sink(report_file)
cat("========================================================\n")
cat("  MEDSIM TEST RUN SUMMARY\n")
cat("========================================================\n\n")

cat("Execution Details:\n")
cat(sprintf("  Date:            %s\n", Sys.time()))
cat(sprintf("  Mode:            %s\n", config$mode))
cat(sprintf("  Environment:     %s\n", medsim_detect_environment()))
cat(sprintf("  Elapsed Time:    %.1f min\n", elapsed / 60))
cat("\n")

cat("Simulation Configuration:\n")
cat(sprintf("  Scenarios:       %d\n", length(unique(results$results$scenario))))
cat(sprintf("  Replications:    %d\n", max(results$results$replication)))
cat(sprintf("  Total runs:      %d\n", nrow(results$results)))
cat(sprintf("  Cores used:      %d\n", config$n_cores))
cat(sprintf("  Random seed:     %d\n", config$seed))
cat("\n")

cat("Overall Accuracy:\n")
for (i in 1:nrow(analysis$accuracy)) {
  cat(sprintf("  %s:\n", analysis$accuracy$parameter[i]))
  cat(sprintf("    Bias:          %.6f\n", analysis$accuracy$bias[i]))
  cat(sprintf("    MAE:           %.6f\n", analysis$accuracy$mae[i]))
  cat(sprintf("    RMSE:          %.6f\n", analysis$accuracy$rmse[i]))
  cat(sprintf("    Relative Bias: %.2f%%\n", analysis$accuracy$relative_bias[i]))
}
cat("\n")

cat("Overall Coverage:\n")
for (i in 1:nrow(coverage$coverage)) {
  cat(sprintf("  %s: %.1f%% (n=%d)\n",
              coverage$coverage$parameter[i],
              coverage$coverage$coverage[i] * 100,
              coverage$coverage$n_valid[i]))
}
cat("\n")

cat("Overall Power:\n")
for (i in 1:nrow(power$power)) {
  cat(sprintf("  %s: %.1f%% (alpha=%.2f, n=%d)\n",
              power$power$parameter[i],
              power$power$power[i] * 100,
              power$power$alpha[i],
              power$power$n_valid[i]))
}
cat("\n")

cat("Files Saved:\n")
cat(sprintf("  %s\n", list.files(output_dir, pattern = "\\.(rds|csv|txt)$")))
cat("\n")

cat("========================================================\n")
cat("  TEST RUN COMPLETE\n")
cat("========================================================\n")

sink()

# Print summary to console
cat(readLines(report_file), sep = "\n")
cat("\n")

cat(sprintf("✓ Summary report saved to: %s\n\n", report_file))

# Helper for NULL-coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
