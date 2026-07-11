#!/bin/bash -l
# One SLURM array task for the Tier-B synthetic study.
# `#!/bin/bash -l` (LOGIN shell) is required: on Hopper `module` is a function
# sourced only by login-shell init, so a plain `#!/bin/bash` fails with
# "module: command not found". TIER_B_* env vars are passed via `sbatch
# --export=ALL` from run-all.sh.
#SBATCH --job-name=medsim_tierb
#SBATCH --partition=general
#SBATCH --time=00:15:00
#SBATCH --mem-per-cpu=512M
#SBATCH --cpus-per-task=4
#SBATCH --output=logs/tierb_%A_%a.out
#SBATCH --error=logs/tierb_%A_%a.err

module load r/4.4.0-ytj2
Rscript run_chunk.R
