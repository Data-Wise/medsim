#!/bin/bash -l
################################################################################
# Tier-B cluster test suite — one-command entry point (Hopper).
#
# NEVER run by R CMD check (inst/ is not executed). Every run here is
# PILOT-GATED: this script submits SMALL pilots (<=4 array tasks) and prints
# how to examine + combine them. Scaling to a full run is a DELIBERATE manual
# step (see FULL RUN note at the bottom) requiring explicit approval.
#
# Usage (on Hopper, from this directory after `R CMD INSTALL` of current medsim):
#   ./run-all.sh
################################################################################
set -euo pipefail
mkdir -p logs

echo "== Tier-B pilots (<=4 tasks each) =="

# --- B2 e2e + B5 grid-collision: nominal method, 4-task array ---------------
echo "[B2] submitting nominal 4-task e2e pilot ..."
TIER_B_N=200 TIER_B_N_CHUNKS=4 TIER_B_N_REPLICATIONS=200 \
  TIER_B_METHOD=nominal TIER_B_OUTDIR=tier_b_nominal \
  sbatch --export=ALL --array=1-4 submit_chunk.sh

# --- B3 dogfood can-fail control: narrow method, 4-task array ---------------
echo "[B3] submitting narrow-CI (planted-defect) 4-task pilot ..."
TIER_B_N=200 TIER_B_N_CHUNKS=4 TIER_B_N_REPLICATIONS=200 \
  TIER_B_METHOD=narrow TIER_B_OUTDIR=tier_b_narrow \
  sbatch --export=ALL --array=1-4 submit_chunk.sh

# --- B4 FORK-RNG realism: single job, multiple cores ------------------------
echo "[B4] submitting FORK-RNG realism single job ..."
sbatch --export=ALL --job-name=medsim_fork --partition=general --time=00:10:00 \
  --mem-per-cpu=512M --cpus-per-task=4 \
  --output=logs/fork_%j.out --error=logs/fork_%j.err \
  --wrap='module load r/4.4.0-ytj2; TIER_B_N_REPLICATIONS=64 Rscript fork_rng_realism.R'

cat <<'EOF'

== after all jobs COMPLETED (check: squeue -u $USER ; sacct -j <id>) ==
  Rscript combine_analyze.R tier_b_nominal nominal   # expect coverage ~0.95
  Rscript combine_analyze.R tier_b_narrow  narrow    # expect coverage < 0.80
  Rscript grid_collision_check.R tier_b_nominal      # expect no collisions
  # B4 verdict is printed in logs/fork_<id>.out

== FULL RUN (do NOT run without explicit approval) ==
  Scale --array=1-4 up to the real chunk count and raise TIER_B_N_REPLICATIONS
  only after the pilots above pass and MaxRSS is within the 512M/cpu envelope
  (sacct -j <id> --format=JobID,State,MaxRSS,Elapsed). Respect the
  pilot-before-scale rule (tasks/plan.md Second principle).
EOF
