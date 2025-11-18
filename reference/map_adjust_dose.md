# Adjust doses to achieve a target metric using MAP Bayesian estimation.

Given a set of levels and a model definition, performs MAP Bayesian
estimation of individual PK/PD parameters, then finds the appropriate
dose to achieve the specified PK/PD target and updates the individual's
regimen accordingly.

## Usage

``` r
map_adjust_dose(
  tdms,
  est_model,
  parameters,
  omega,
  ruv,
  regimen,
  covariates = NULL,
  target_design,
  dose_update,
  grid = NULL,
  settings = NULL,
  ...
)
```

## Arguments

- tdms:

  observations to be included in MAP estimates

- est_model:

  model used for estimation

- parameters:

  population parameters of estimation model

- omega:

  omega matrix of estimation model

- ruv:

  residual error distribution of estimation model, a named list with
  elements for `prop` (proportional) and `add` (additive).

- regimen:

  PKPDsim regimen object

- covariates:

  named list of PKPDsim covariates

- target_design:

  object specifying target design, created using function
  [`create_target_design()`](create_target_design.md)

- dose_update:

  update dose from which dose?

- grid:

  vector specifying doses or intervals to use as test grid, Example:
  `seq(from = 50, to = 500, by = (500 - 50) / 10)`

- settings:

  list of arguments to be used in dose update, e.g. `dose_resolution`.

- ...:

  arguments passed on to PKPDmap::get_map_estimates and/or PKPDsim::sim

## Value

Returns a named list: `regimen`: the updated regimen; `additional_info`:
the MAP parameter estimates
