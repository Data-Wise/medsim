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
#> * Cached to: /tmp/Rtmp4MeBCV/file19b110dabf10.rds
info <- medsim_cache_info(cache_file)
print(info)
#> $exists
#> [1] TRUE
#> 
#> $size_mb
#> [1] 0.0001983643
#> 
#> $modified
#> [1] "2026-08-17 07:04:11 UTC"
#> 
#> $age_days
#> [1] 1.20589e-08
#> 
#> $timestamp
#> [1] "2026-08-17 07:04:11 UTC"
#> 
#> $r_version
#> [1] "R version 4.6.1 (2026-06-24)"
#> 
unlink(cache_file)
```
