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
medsim_cache_init("cache")
#> * Created cache directory: cache
#> * Created README: cache/README.md
medsim_cache_init("simulation_results/cache")
#> * Created cache directory: simulation_results/cache
#> * Created README: simulation_results/cache/README.md
```
