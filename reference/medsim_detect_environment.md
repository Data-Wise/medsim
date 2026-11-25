# Detect Computing Environment

Detects whether code is running on local machine or HPC cluster by
checking for scheduler environment variables.

## Usage

``` r
medsim_detect_environment()
```

## Value

Character: "local" or "cluster"

## Details

Checks for the following environment variables:

- SLURM_JOB_ID (SLURM scheduler)

- PBS_JOBID (PBS/Torque scheduler)

- LSB_JOBID (LSF scheduler)

If any are found, returns "cluster". Otherwise, returns "local".

## Examples

``` r
env <- medsim_detect_environment()
if (env == "cluster") {
  message("Running on HPC cluster")
} else {
  message("Running on local machine")
}
#> Running on local machine
```
