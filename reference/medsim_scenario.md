# Create Custom Simulation Scenario

Define a custom scenario for simulation studies. A scenario consists of
a name, description, data generation function, and population
parameters.

## Usage

``` r
medsim_scenario(name, description = "", data_generator, params = list())
```

## Arguments

- name:

  Character: Descriptive name for the scenario

- description:

  Character: Detailed description (optional)

- data_generator:

  Function: Takes n (sample size) and returns data.frame

- params:

  List: Known population parameters for validation

## Value

A scenario object (list with class "medsim_scenario")

## Details

### Data Generator Function

The data_generator function must:

- Accept `n` (sample size) as first argument

- Return a data.frame with at least: X (treatment), M (mediator), Y
  (outcome)

- Can include additional variables or covariates

### Parameters List

The params list should include known population values:

- `a`: X -\> M path coefficient

- `b`: M -\> Y path coefficient

- `c_prime`: X -\> Y direct effect

- `indirect`: True indirect effect (a \* b)

- Additional parameters as needed

## See also

[`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md)
for standard scenarios

## Examples

``` r
# Simple custom scenario
my_scenario <- medsim_scenario(
  name = "Large Effects",
  description = "Both paths have large effects",
  data_generator = function(n = 200) {
    X <- rnorm(n)
    M <- 0.7 * X + rnorm(n)
    Y <- 0.7 * M + rnorm(n)
    data.frame(X = X, M = M, Y = Y)
  },
  params = list(
    a = 0.7,
    b = 0.7,
    indirect = 0.49
  )
)

# Generate data
data <- my_scenario$data_generator(n = 100)

# Scenario with covariates
covariate_scenario <- medsim_scenario(
  name = "With Covariates",
  data_generator = function(n = 200) {
    C1 <- rnorm(n)
    C2 <- rbinom(n, 1, 0.5)
    X <- rnorm(n)
    M <- 0.3 * X + 0.2 * C1 + rnorm(n)
    Y <- 0.3 * M + 0.2 * C2 + rnorm(n)
    data.frame(X = X, M = M, Y = Y, C1 = C1, C2 = C2)
  },
  params = list(
    a = 0.3,
    b = 0.3,
    indirect = 0.09,
    gamma_m = 0.2,
    gamma_y = 0.2
  )
)
```
