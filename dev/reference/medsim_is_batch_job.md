# Check if Running as Batch Job

Detects if code is running as a batch job on an HPC cluster. Used to
suppress progress bars that clutter log files.

## Usage

``` r
medsim_is_batch_job()
```

## Value

Logical: TRUE if running as batch job, FALSE if interactive

## Details

Checks for:

- Interactive session:
  [`interactive()`](https://rdrr.io/r/base/interactive.html)

- HPC scheduler environment variables (SLURM_JOB_ID, PBS_JOBID,
  LSB_JOBID)
