#!/bin/bash -l
# One SLURM array task for the Tier-B synthetic study.
# `#!/bin/bash -l` (LOGIN shell) is required: on Hopper `module` is a function
# sourced only by login-shell init, so a plain `#!/bin/bash` fails with
# "module: command not found". TIER_B_* env vars are passed via `sbatch
# --export=ALL` from run-all.sh.
#
# Hardened per Gate B (SPEC-medsim-chunked-run-gates-2026-07-31 / #37):
# pipefail, hard-fail module load (never `|| true` -- that plus a trailing
# command resetting $? was the historical COMPLETED/0:0/no-output mode), and
# Rscript as the last command so its exit code IS the task's exit code.
# Completeness checking lives in medsim_combine_chunks()'s audit, not here.
#SBATCH --job-name=medsim_tierb
#SBATCH --partition=general
#SBATCH --time=00:15:00
#SBATCH --mem-per-cpu=512M
#SBATCH --cpus-per-task=4
#SBATCH --requeue
#SBATCH --output=logs/tierb_%A_%a.out
#SBATCH --error=logs/tierb_%A_%a.err

set -eo pipefail

module load r/4.4.0-ytj2 || { echo "FATAL: module load r/4.4.0-ytj2 failed" >&2; exit 1; }

set -u
command -v Rscript >/dev/null || { echo "FATAL: Rscript not on PATH" >&2; exit 1; }

Rscript run_chunk.R
