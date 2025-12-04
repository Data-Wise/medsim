# Simulation Cache

This directory contains cached simulation results to avoid expensive recomputation.

## Contents

- `truth_scenario_*.rds` - Ground truth values for each scenario
- `*.rds` - Other cached results

## Cache Management

Clear old cache:
```r
medsim::medsim_cache_clear("cache", max_age = 30)
```

List cache files:
```r
medsim::medsim_cache_list("cache")
```

## Notes

- Cache files are in RDS format (compressed R objects)
- Automatically managed by medsim package
- Safe to delete (will be regenerated as needed)

