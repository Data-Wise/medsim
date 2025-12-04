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
info <- medsim_cache_info("cache/truth.rds")
print(info)
#> $exists
#> [1] FALSE
#> 
#> $size_mb
#> [1] NA
#> 
#> $modified
#> [1] NA
#> 
#> $age_days
#> [1] NA
#> 
#> $timestamp
#> [1] NA
#> 
#> $r_version
#> [1] NA
#> 
```
