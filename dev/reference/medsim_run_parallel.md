# Run Tasks in Parallel

Executes a list of tasks in parallel using multiple CPU cores. Provides
progress bars, proper error handling, and automatic cluster management.

## Usage

``` r
medsim_run_parallel(
  tasks,
  fun,
  n_cores = NULL,
  progress = TRUE,
  export = NULL,
  packages = NULL,
  cluster_type = NULL
)
```

## Arguments

- tasks:

  Vector or list: Tasks to process (e.g., 1:100, list of params)

- fun:

  Function: Function to apply to each task. Should accept one argument
  (the task) and return a result.

- n_cores:

  Integer: Number of CPU cores to use. If NULL, uses all available cores
  minus 2.

- progress:

  Logical: Show progress bar (default TRUE). Automatically suppressed
  when running on cluster (batch jobs).

- export:

  Character vector: Names of objects to export to workers. These objects
  must be available in the calling environment.

- packages:

  Character vector: Names of packages to load on each worker. Use this
  if `fun` requires specific packages.

- cluster_type:

  Character: Type of cluster ("PSOCK" or "FORK"). FORK is more efficient
  but only works on Unix. Auto-detected by default.

## Value

List: Results from applying `fun` to each task (same length as tasks)

## Details

### Cluster Types

- **PSOCK** (default on Windows): Creates separate R sessions. Requires
  explicit export of objects and packages. Works on all platforms.

- **FORK** (default on Unix): Copies current R session. More efficient,
  automatic object sharing, but Unix-only.

### Progress Bars

Progress bars are shown in interactive sessions and local execution, but
suppressed on HPC clusters to avoid cluttering log files.

### Error Handling

If a task fails:

- Error is caught and returned as an error object

- Other tasks continue processing

- Check results with `sapply(results, inherits, "error")`

### Memory Considerations

Each worker process requires memory. For large tasks:

- Reduce `n_cores` if running out of memory

- Process tasks in chunks

- Use FORK cluster (Unix) which shares memory

## See also

[`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html),
[`parallel::parLapply()`](https://rdrr.io/r/parallel/clusterApply.html),
[`pbapply::pblapply()`](https://peter.solymos.org/pbapply/reference/pbapply.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple parallel computation
results <- medsim_run_parallel(
  tasks = 1:100,
  fun = function(i) {
    Sys.sleep(0.1)  # Simulate work
    i^2
  },
  n_cores = 4
)

# With exports
my_data <- data.frame(x = 1:10, y = 11:20)

results <- medsim_run_parallel(
  tasks = 1:10,
  fun = function(i) {
    mean(my_data$x[1:i])
  },
  export = "my_data"
)

# With packages
results <- medsim_run_parallel(
  tasks = 1:10,
  fun = function(i) {
    dplyr::tibble(x = i, y = i^2)
  },
  packages = "dplyr"
)

# Check for errors
errors <- sapply(results, inherits, "error")
if (any(errors)) {
  cat("Errors occurred in:", which(errors), "\n")
}
} # }
```
