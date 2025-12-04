# Save Object to Cache

Saves an R object to a cache file for later retrieval. Uses RDS format
for efficient storage and compression.

## Usage

``` r
medsim_cache_save(object, file, compress = TRUE, overwrite = TRUE)
```

## Arguments

- object:

  Any R object to cache

- file:

  Character: Path to cache file. Parent directory will be created if it
  doesn't exist.

- compress:

  Logical: Compress the cached object (default TRUE). Can significantly
  reduce file size for large objects.

- overwrite:

  Logical: Overwrite existing cache file (default TRUE)

## Value

Invisibly returns the file path

## Details

### Cache File Format

Cache files are stored in RDS format with metadata:

- Original object

- Timestamp of creation

- R version used

- Package version (if available)

### File Organization

Recommended cache directory structure:

    cache/
    |-- truth_scenario_1.rds
    |-- truth_scenario_2.rds
    +-- ...

## See also

[`medsim_cache_load()`](https://data-wise.github.io/medsim/reference/medsim_cache_load.md),
[`medsim_cache_clear()`](https://data-wise.github.io/medsim/reference/medsim_cache_clear.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Cache simulation truth
truth <- compute_expensive_truth()
medsim_cache_save(truth, "cache/truth_scenario1.rds")

# Cache with metadata
result <- list(value = 42, timestamp = Sys.time())
medsim_cache_save(result, "cache/my_result.rds")
} # }
```
