# Create Simulation Configuration

Creates a configuration object for simulation studies. Automatically
detects whether running on local machine or HPC cluster and adjusts
parameters accordingly.

## Usage

``` r
medsim_config(
  mode = "auto",
  n_replications = NULL,
  n_cores = NULL,
  scenarios = NULL,
  output_dir = NULL,
  seed = 12345,
  ...
)
```

## Arguments

- mode:

  Character: "auto", "test", "local", or "cluster"

  - "auto": Detect based on environment variables (SLURM, PBS, LSF)

  - "test": Quick validation (~30 seconds)

  - "local": Development on local machine (~15 minutes)

  - "cluster": Production on HPC cluster (hours)

- n_replications:

  Integer: Number of Monte Carlo replications. If NULL, uses mode
  defaults (test=20, local=100, cluster=1000)

- n_cores:

  Integer: Number of CPU cores for parallel processing. If NULL,
  auto-detects (all cores - 2 on local, SLURM_CPUS_PER_TASK on cluster)

- scenarios:

  Character: "all" or "test". Use "test" for single challenging scenario
  during development

- output_dir:

  Character: Directory for saving results

- seed:

  Integer: Random seed for reproducibility

- ...:

  Additional custom parameters

## Value

A list with simulation configuration parameters

## Details

### Execution Modes

|         |              |       |         |                  |
|---------|--------------|-------|---------|------------------|
| Mode    | Replications | Cores | Runtime | Use Case         |
| test    | 20           | 4     | ~30s    | Quick validation |
| local   | 100          | auto  | ~15m    | Development      |
| cluster | 1000         | SLURM | hours   | Production       |

### Environment Detection

When mode = "auto", checks for:

- SLURM_JOB_ID (SLURM scheduler)

- PBS_JOBID (PBS/Torque scheduler)

- LSB_JOBID (LSF scheduler)

If any are found, uses "cluster" mode. Otherwise, uses "local" mode.

### Custom Parameters

You can add custom parameters via `...`:

    config <- medsim_config(
      mode = "local",
      n_bootstrap = 5000,
      alpha = 0.05,
      custom_param = "value"
    )

## See also

[`medsim_detect_environment()`](https://data-wise.github.io/medsim/reference/medsim_detect_environment.md),
[`print.medsim_config()`](https://data-wise.github.io/medsim/reference/print.medsim_config.md)

## Examples

``` r
# Auto-detect environment
config <- medsim_config(mode = "auto")
#> Auto-detected: LOCAL environment

# Explicit test mode for quick validation
config_test <- medsim_config(mode = "test")

# Local mode with custom replications
config_local <- medsim_config(
  mode = "local",
  n_replications = 500
)

# Cluster mode (auto-detects SLURM cores)
config_cluster <- medsim_config(mode = "cluster")

# Custom parameters
config_custom <- medsim_config(
  mode = "local",
  n_bootstrap = 1000,
  ci_level = 0.95
)

# Print configuration
print(config)
#> 
#> ========================================================
#>   LOCAL MODE
#> ========================================================
#> 
#> Simulation Parameters:
#>   Replications:        100
#>   Scenarios:           all
#> 
#> Computing Resources:
#>   Cores:               2
#>   Random Seed:         12345
#> 
#> Output:
#>   Directory:           simulation_results
#> 
#> ========================================================
#> 
```
