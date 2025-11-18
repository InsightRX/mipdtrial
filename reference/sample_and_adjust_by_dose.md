# Adjust dosing using MIPD on TDMs at specified dose numbers

Doses are iteratively adjusted for the specified dose numbers.
Estimation is performed by MAP Bayesian estimation, and simulation is
used to identify the dose most likely to reach the specified target. See
`dose_grid_search` for dose-finding logic. Optionally, disparate models
can be used for simulation and estimation, to allow for intentional
model misspecification. Covariates supplied must support both models.
True patient parameters (`pars_true_i`) must match the model used for
simulation.

## Usage

``` r
sample_and_adjust_by_dose(
  regimen_update_design,
  sampling_design,
  target_design,
  regimen,
  covariates = NULL,
  pars_true_i,
  sim_model,
  sim_ruv = NULL,
  verbose = FALSE,
  accumulate_data = TRUE,
  ...
)
```

## Arguments

- regimen_update_design:

  a data.frame with scheme with details on how and when to update the
  regimen in the MIPD trial. Schemes are created using
  [`create_regimen_update_design()`](create_regimen_update_design.md).

- sampling_design:

  a data.frame with a sampling scheme, created using
  [`create_sampling_design()`](create_sampling_design.md).

- target_design:

  target design created using
  [`create_target_design()`](create_target_design.md)

- regimen:

  PKPDsim regimen object, containing initial dosing regimen.

- covariates:

  named list of PKPDsim covariates.

- pars_true_i:

  PK parameters for the individual. See `generate_iiv`.

- sim_model:

  model to use for simulating "true" patient response.

- sim_ruv:

  residual variability for `sim_model`. Named list for proportional
  (`prop`) and additive (`add`) error.

- verbose:

  verbose output?

- accumulate_data:

  if `TRUE`, will use all available data up until the adjustment
  timepoint. If set to `FALSE`, will use only the data since the last
  adjustment timepoint and the current one.

- ...:

  arguments passed on to `simulate_fit` or dose_optimization_method
  function.

## Value

a named list containing `final_regimen` (all doses after adjustment),
`tdms` (all collected levels, both true and measured, that is, both with
and without residual variability), and `additional_info`, which varies
by dose_optimization_method. See selected function for details.

## Details

Trial design or MIPD protocol can vary considerably in complexity. This
function is intended to cover a broad range of relatively straight
forward designs, and may serve as a template for more complex designs.
