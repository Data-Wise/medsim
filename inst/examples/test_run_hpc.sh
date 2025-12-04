#!/bin/bash
#SBATCH --job-name=medsim-test
#SBATCH --output=medsim-test-%j.out
#SBATCH --error=medsim-test-%j.err
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=32G
#SBATCH --partition=standard

################################################################################
# SLURM Batch Script for medsim HPC Test
#
# Purpose: Run medsim test on HPC cluster with SLURM scheduler
#
# Usage:
#   sbatch test_run_hpc.sh
#
# Or with custom resources:
#   sbatch --cpus-per-task=32 --mem=64G test_run_hpc.sh
################################################################################

# Print job information
echo "========================================================"
echo "  MEDSIM HPC TEST - SLURM JOB $SLURM_JOB_ID"
echo "========================================================"
echo ""
echo "Job Details:"
echo "  Job ID:          $SLURM_JOB_ID"
echo "  Job Name:        $SLURM_JOB_NAME"
echo "  Node:            $SLURM_NODELIST"
echo "  CPUs:            $SLURM_CPUS_PER_TASK"
echo "  Memory:          $SLURM_MEM_PER_NODE MB"
echo "  Partition:       $SLURM_JOB_PARTITION"
echo "  Start Time:      $(date)"
echo ""

# Load R module (adjust for your cluster)
# Uncomment and modify as needed:
# module load R/4.3.0
# module load gcc/11.2.0

# Print R version
echo "R Configuration:"
echo "  R Version:       $(Rscript --version 2>&1)"
echo "  R_HOME:          $R_HOME"
echo ""

# Set working directory to script location
cd "$(dirname "$0")"

# Print environment variables (for debugging)
echo "Environment Variables:"
echo "  SLURM_JOB_ID:          $SLURM_JOB_ID"
echo "  SLURM_CPUS_PER_TASK:   $SLURM_CPUS_PER_TASK"
echo "  SLURM_MEM_PER_NODE:    $SLURM_MEM_PER_NODE"
echo ""

# Run R script
echo "Running medsim test..."
echo ""

Rscript test_run_hpc.R

# Capture exit code
EXIT_CODE=$?

# Print completion information
echo ""
echo "========================================================"
echo "  JOB COMPLETE"
echo "========================================================"
echo ""
echo "Exit code:       $EXIT_CODE"
echo "End time:        $(date)"
echo ""

if [ $EXIT_CODE -eq 0 ]; then
    echo "✓ Test completed successfully!"
    echo ""
    echo "Results saved to: simulation_results/"
    echo ""
    echo "To view summary:"
    echo "  cat simulation_results/SUMMARY.txt"
else
    echo "✗ Test failed with exit code $EXIT_CODE"
    echo "Check error log: medsim-test-$SLURM_JOB_ID.err"
fi

echo ""

exit $EXIT_CODE
