# Load Object from Cache

Loads a previously cached R object. Returns NULL if cache file doesn't
exist.

## Usage

``` r
medsim_cache_load(file, check_expiry = FALSE, max_age = 30, verbose = TRUE)
```

## Arguments

- file:

  Character: Path to cache file

- check_expiry:

  Logical: Check if cache has expired (default FALSE)

- max_age:

  Numeric: Maximum age of cache in days (default 30). Only used if
  `check_expiry = TRUE`.

- verbose:

  Logical: Print messages about cache status (default TRUE)

## Value

Cached object if file exists and is valid, NULL otherwise

## Details

### Cache Validation

When loading, the cache is checked for:

- File exists

- File is readable

- Age is within max_age (if check_expiry = TRUE)

### Handling Missing Cache

If cache doesn't exist:

- Returns NULL (no error)

- Calling code should compute and cache the result

Example pattern:

    result <- medsim_cache_load("cache/truth.rds")
    if (is.null(result)) {
      result <- expensive_computation()
      medsim_cache_save(result, "cache/truth.rds")
    }

## See also

[`medsim_cache_save()`](https://data-wise.github.io/medsim/reference/medsim_cache_save.md),
[`medsim_cache_exists()`](https://data-wise.github.io/medsim/reference/medsim_cache_exists.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load from cache
truth <- medsim_cache_load("cache/truth_scenario1.rds")

if (is.null(truth)) {
  # Cache miss - compute and cache
  truth <- compute_expensive_truth()
  medsim_cache_save(truth, "cache/truth_scenario1.rds")
}

# Load with expiry check
result <- medsim_cache_load(
  "cache/old_result.rds",
  check_expiry = TRUE,
  max_age = 7  # 7 days
)
} # }
```
