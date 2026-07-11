################################################################################
# Combine all chunk_*.rds in an output dir, attach the analytic truth, and print
# by-scenario coverage. Args: <outdir> [expected_label]
# Usage: Rscript combine_analyze.R tier_b_results nominal
################################################################################

source("tier_b_synthetic.R")

args   <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "tier_b_results"
label  <- if (length(args) >= 2) args[[2]] else "nominal"

combined <- medsim_combine_chunks(outdir, verbose = TRUE)
combined$truth <- tier_b_truth()

cov <- medsim_analyze_coverage(combined, estimand = tier_b_estimand(),
                               by_scenario = TRUE)

cat(sprintf("\n=== Tier-B coverage (%s), outdir=%s ===\n", label, outdir))
print(cov$by_scenario)
cat(sprintf("\noverall mean coverage: %.3f  (n reps total: %d)\n",
            mean(cov$by_scenario$coverage), nrow(combined$results)))

# Sanity signal for the pilot examiner: nominal should be ~0.95, narrow << 0.95.
mc <- mean(cov$by_scenario$coverage)
verdict <- if (label == "nominal") {
  if (mc > 0.88 && mc < 0.99) "PASS (near-nominal)" else "CHECK (out of band)"
} else {
  if (mc < 0.80) "PASS (control undercovers as designed)" else "CHECK (control did not undercover)"
}
cat("verdict:", verdict, "\n")
