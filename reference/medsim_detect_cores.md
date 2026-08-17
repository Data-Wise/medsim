# Detect Number of Available Cores

Detects the number of CPU cores available for parallel processing. On
HPC clusters, uses scheduler-allocated cores. On local machines, uses
all cores minus 2 to avoid overwhelming the system.

## Usage

``` r
medsim_detect_cores()
```

## Value

Integer: Number of cores to use

## Details

On clusters, checks for:

- SLURM_CPUS_PER_TASK (SLURM)

- PBS_NUM_PPN (PBS/Torque)

- LSB_DJOB_NUMPROC (LSF)

On local machines, uses `parallel::detectCores() - 2`.

## Examples

``` r
n_cores <- medsim_detect_cores()
message(sprintf("Using %d cores", n_cores))
#> Using 2 cores
```
