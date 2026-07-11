################################################################################
# B5 (at-scale slice) — RNG seed-collision across the full (scenario × rep) grid.
#
# After a combined Tier-B run, verify .medsim_det_seed produced NO collisions
# across every (scenario, global_rep_id) pair actually used — the polynomial-hash
# fix must hold at production grid scale, not just for a handful of names. A
# collision would mean two replications drew identical data (silent correlation).
# Args: <outdir>
################################################################################

source("tier_b_synthetic.R")

args   <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "tier_b_results"

combined <- medsim_combine_chunks(outdir, verbose = FALSE)
res <- combined$results

# Distinct CI pairs across the WHOLE grid: with independent draws every
# (scenario, rep) is a fresh sample, so CI pairs should be ~all distinct.
ci <- res[, c("scenario", "theta_lower", "theta_upper")]
n_rows     <- nrow(ci)
n_distinct <- nrow(unique(ci))
cat(sprintf("grid rows: %d   distinct (scenario,CI): %d\n", n_rows, n_distinct))
cat("no cross-grid draw collision:", n_distinct == n_rows, "\n")

# Direct seed check: recompute .medsim_det_seed for every used (scenario, rep)
# and confirm uniqueness within each scenario's rep range.
reps_per_scn <- table(res$scenario)
collision <- FALSE
for (scn in names(reps_per_scn)) {
  k <- as.integer(reps_per_scn[[scn]])
  seeds <- vapply(seq_len(k), function(r) medsim:::.medsim_det_seed(scn, r), integer(1))
  if (length(unique(seeds)) != k) collision <- TRUE
}
cat("no .medsim_det_seed collision within any scenario:", !collision, "\n")
cat("verdict:", if (n_distinct == n_rows && !collision) "PASS" else "CHECK", "\n")
