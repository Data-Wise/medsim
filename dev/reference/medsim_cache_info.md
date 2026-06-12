# Get Cache Info

Retrieves metadata about a cached object without loading the full
object.

## Usage

``` r
medsim_cache_info(file)
```

## Arguments

- file:

  Character: Path to cache file

## Value

List with cache metadata:

- `exists`: Logical - file exists

- `size_mb`: Numeric - file size in MB

- `modified`: POSIXct - last modified time

- `age_days`: Numeric - age in days

- `timestamp`: POSIXct - creation time (if available)

- `r_version`: Character - R version used (if available)

## Examples

``` r
# Save a cached object to a temp file, then inspect it
cache_file <- tempfile(fileext = ".rds")
medsim_cache_save(list(value = 42), cache_file)
#> * Cached to: /tmp/RtmpGuo4g6/file19fd7e6b566e.rds
info <- medsim_cache_info(cache_file)
print(info)
#> $exists
#> [1] TRUE
#> 
#> $size_mb
#> [1] 0.0002031326
#> 
#> $modified
#> [1] "2026-06-12 00:25:52 UTC"
#> 
#> $age_days
#> [1] 9.936867e-09
#> 
#> $timestamp
#> [1] "2026-06-12 00:25:52 UTC"
#> 
#> $r_version
#> [1] "R version 4.6.0 (2026-04-24)"
#> 
unlink(cache_file)
```
