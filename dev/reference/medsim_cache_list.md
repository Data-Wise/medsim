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
# Create a small cache in a temp directory, then list it
cache_dir <- file.path(tempdir(), "medsim_cache_list_example")
dir.create(cache_dir, showWarnings = FALSE)
medsim_cache_save(list(a = 1), file.path(cache_dir, "result1.rds"))
#> * Cached to: /tmp/RtmpGuo4g6/medsim_cache_list_example/result1.rds
medsim_cache_save(list(b = 2), file.path(cache_dir, "result2.rds"))
#> * Cached to: /tmp/RtmpGuo4g6/medsim_cache_list_example/result2.rds

cache_list <- medsim_cache_list(cache_dir)
print(cache_list)
#>          file                                                  path
#> 2 result2.rds /tmp/RtmpGuo4g6/medsim_cache_list_example/result2.rds
#> 1 result1.rds /tmp/RtmpGuo4g6/medsim_cache_list_example/result1.rds
#>        size_mb     age_days            modified
#> 2 0.0001983643 9.147657e-09 2026-06-12 00:25:52
#> 1 0.0002002716 1.947637e-08 2026-06-12 00:25:52

unlink(cache_dir, recursive = TRUE)
```
