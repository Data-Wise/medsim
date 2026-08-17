# Write a SLURM array submit script for chunked simulation jobs

Generates a `submit_array.sh` bash script targeting the UNM CARC Hopper
cluster (and compatible SLURM systems). The script submits one SLURM
array job per scenario chunk; each task runs an R script that calls
[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md).

The emitted script is fail-loud (Gate B of the chunked-run integrity
layer): a login shell (`#!/bin/bash -l` – on Hopper `module` is only
defined in login shells), `set -eo pipefail`, a hard-failing
`module load` (never `|| true`), a `command -v Rscript` pre-check,
`#SBATCH --requeue`, and the `Rscript` call as the final command so its
exit code is the task's exit code. There is deliberately NO output-file
gate in the script: the writer's config and the run script's runtime
config are independent, so a baked-in path could fail successful tasks;
completeness is audited at combine time by
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
instead. Set `config$array_throttle = K` to cap concurrently-running
array tasks (`--array=1-N%K`).

## Usage

``` r
medsim_write_submit_script(
  config,
  run_script = "run_simulation_chunk.R",
  output_file = "submit_array.sh",
  account = NULL
)
```

## Arguments

- config:

  A `medsim_config` object (from
  [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)).
  Must have `n_chunks` \> 0 and `mode == "cluster"`. Optional:
  `array_throttle`.

- run_script:

  Character: path (on the cluster) to the per-chunk R script (the one
  that calls
  [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)).
  Default `"run_simulation_chunk.R"`.

- output_file:

  Character: where to write the bash script. Default `"submit_array.sh"`
  in the current directory.

- account:

  Character: SLURM account/PI account string (e.g. `"pi-dtofighi"`). If
  `NULL`, no `--account` line is emitted.

## Value

Invisibly, the path to the written script.

## See also

[`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md),
[`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)

## Examples

``` r
cfg <- medsim_config(
  mode = "cluster", n_replications = 1000, n_chunks = 10,
  walltime = "04:00:00", mem_per_cpu = "8G"
)
tmp <- tempfile(fileext = ".sh")
medsim_write_submit_script(cfg, output_file = tmp)
# cat(readLines(tmp), sep = "\n")
```
