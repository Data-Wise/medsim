# Clear Cache

Deletes cache files from a directory. Can delete all files or files
matching a pattern.

## Usage

``` r
medsim_cache_clear(
  cache_dir,
  pattern = "*.rds",
  max_age = NULL,
  confirm = TRUE
)
```

## Arguments

- cache_dir:

  Character: Directory containing cache files

- pattern:

  Character: Optional pattern to match files (e.g., "truth\_\*.rds")

- max_age:

  Numeric: Delete only files older than this many days. If NULL, deletes
  all matching files.

- confirm:

  Logical: Ask for confirmation before deleting (default TRUE)

## Value

Invisibly returns number of files deleted

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear all cache
medsim_cache_clear("cache")

# Clear old cache only
medsim_cache_clear("cache", max_age = 30)

# Clear specific pattern
medsim_cache_clear("cache", pattern = "truth_*.rds")

# Without confirmation
medsim_cache_clear("cache", confirm = FALSE)
} # }
```
