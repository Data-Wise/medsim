# Check if Cache Exists

Checks if a cache file exists and is readable.

## Usage

``` r
medsim_cache_exists(file)
```

## Arguments

- file:

  Character: Path to cache file

## Value

Logical: TRUE if cache exists and is readable, FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
if (medsim_cache_exists("cache/truth.rds")) {
  truth <- medsim_cache_load("cache/truth.rds")
} else {
  truth <- list(indirect = 0.09)  # Compute your truth values
  medsim_cache_save(truth, "cache/truth.rds")
}
} # }
```
