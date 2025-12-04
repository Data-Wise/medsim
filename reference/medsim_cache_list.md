# List Cache Files

Lists all cache files in a directory with their metadata.

## Usage

``` r
medsim_cache_list(cache_dir, pattern = "*.rds")
```

## Arguments

- cache_dir:

  Character: Directory containing cache files

- pattern:

  Character: Pattern to match files (default "\*.rds")

## Value

data.frame with columns:

- `file`: File name

- `path`: Full path

- `size_mb`: Size in MB

- `age_days`: Age in days

- `modified`: Last modified time

## Examples

``` r
# List all cache
cache_list <- medsim_cache_list("cache")
print(cache_list)
#> data frame with 0 columns and 0 rows

# List specific pattern
truth_cache <- medsim_cache_list("cache", pattern = "truth_*.rds")
```
