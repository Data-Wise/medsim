################################################################################
# B4 — FORK-worker RNG realism (the SOLE guard for FORK RNG; impossible in
# Tier A, where _R_CHECK_LIMIT_CORES_ forces the sequential path).
#
# Runs one chunk of many replications through a REAL multi-core FORK cluster and
# asserts that no two replications share identical draws — the signature of the
# original chunked-RNG bug (workers inheriting identical RNG state at fork time).
# Also asserts reproducibility: the same run twice gives identical draws.
# Run on a compute node with several cores allocated.
################################################################################

source("tier_b_synthetic.R")

n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "4"))
n_rep   <- as.integer(Sys.getenv("TIER_B_N_REPLICATIONS", unset = "64"))

run_once <- function() {
  outdir <- tempfile("fork_"); dir.create(outdir)
  cfg <- medsim_config(mode = "cluster", n_chunks = 1L, chunk_id = 1L,
                       output_dir = outdir)
  cfg$n <- 200; cfg$n_replications <- n_rep; cfg$n_cores <- n_cores
  # One scenario, many reps, real FORK cluster (n_cores > 1, unix -> FORK).
  path <- medsim_run_chunk(build_tier_b_scenarios()[1], tier_b_method_nominal,
                           cfg, verbose = FALSE)
  readRDS(path)$results
}

cat(sprintf("FORK-RNG realism: n_cores=%d, n_rep=%d\n", n_cores, n_rep))
r1 <- run_once()
ci <- r1[, c("theta_lower", "theta_upper")]
n_distinct <- nrow(unique(ci))
cat(sprintf("distinct CI pairs: %d / %d\n", n_distinct, nrow(ci)))
cat("all draws distinct (no FORK RNG collision):", n_distinct == nrow(ci), "\n")

r2 <- run_once()
cat("reproducible (run1 == run2):",
    identical(r1$theta_lower, r2$theta_lower), "\n")

ok <- n_distinct == nrow(ci) && identical(r1$theta_lower, r2$theta_lower)
cat("verdict:", if (ok) "PASS" else "FAIL", "\n")
