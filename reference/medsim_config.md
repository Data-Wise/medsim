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
  chunk_id = NULL,
  n_chunks = NULL,
  array_size = NULL,
  seed_stream = NULL,
  partition = NULL,
  walltime = NULL,
  mem_per_cpu = NULL,
  r_module = NULL,
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

  Integer: Base seed, stored on the config and printed for provenance.
  **Does not itself determine replication draws** – since the
  [`.medsim_det_seed()`](https://data-wise.github.io/medsim/reference/dot-medsim_det_seed.md)
  fix,
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  seeds each replication deterministically from
  `(scenario_name, global_rep_id)`, independent of
  `seed`/chunk/worker/cluster-type. `seed` is only consulted if you call
  [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)
  directly (its own `seed=` argument, documented there). See
  `seed_stream` below for its deprecation.

- chunk_id:

  Integer: SLURM array task index (1-based) for this chunk.
  Auto-detected from `SLURM_ARRAY_TASK_ID` when running inside a SLURM
  array job and not supplied explicitly. `NULL` = no chunking.

- n_chunks:

  Integer: Total number of chunks (SLURM array size). Used by
  [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  to slice the replication index.

- array_size:

  Integer: Alias for `n_chunks` (matches SLURM terminology). When both
  are supplied, `n_chunks` wins.

- seed_stream:

  Integer: **Deprecated.** This knob is stored on the config but has
  never been consumed by any medsim function – passing a non-`NULL`
  value now emits a deprecation warning and has no effect on any RNG
  stream. For reproducible per-worker L'Ecuyer-CMRG streams, pass
  `seed=` directly to
  [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md);
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)/
  [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  seed each replication deterministically (see the `seed` param above)
  and need no stream seed.

- partition:

  Character: SLURM partition (queue) name. Defaults to `"general"` in
  cluster mode; `NULL` otherwise.

- walltime:

  Character: SLURM wall-time limit (HH:MM:SS). Defaults to `"08:00:00"`
  in cluster mode.

- mem_per_cpu:

  Character: SLURM memory per CPU. Defaults to `"4G"` in cluster mode.

- r_module:

  Character: Environment module string for `module load`. Defaults to
  `"r/4.4.0-ytj2"` (UNM CARC Hopper) in cluster mode.

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
#>   Base Seed:           12345 (provenance only -- replications are seeded per-rep, see ?medsim_run)
#> 
#> Output:
#>   Directory:           simulation_results
#> 
#> Custom Parameters:
#> 
#> ========================================================
#> 
```
