# Initialize Cache Directory

Creates a cache directory with proper structure and a README file
explaining the cache system.

## Usage

``` r
medsim_cache_init(cache_dir, create_readme = TRUE)
```

## Arguments

- cache_dir:

  Character: Path to cache directory

- create_readme:

  Logical: Create README.md explaining cache (default TRUE)

## Value

Invisibly returns cache directory path

## Examples

``` r
cache_dir <- file.path(tempdir(), "medsim_cache_example")
medsim_cache_init(cache_dir)
#> * Created cache directory: /tmp/RtmpGuo4g6/medsim_cache_example
#> * Created README: /tmp/RtmpGuo4g6/medsim_cache_example/README.md
unlink(cache_dir, recursive = TRUE)
```
