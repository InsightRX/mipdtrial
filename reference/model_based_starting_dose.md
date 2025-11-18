# Model-based starting dose

A light wrapper for [dose_grid_search](dose_grid_search.md) for finding
model-based starting doses and returning an appropriate regimen object.

## Usage

``` r
model_based_starting_dose(design, covariates, cov_mapping, ...)
```

## Arguments

- design:

  trial design, created using
  [`create_trial_design()`](create_trial_design.md)

- covariates:

  named list of PKPDsim covariates. E.g.: list(WT =
  PKPDsim::new_covariate(70))

- cov_mapping:

  a named vector indicating correspondence between model covariates and
  data frame columns. Vector names should reflect all covariates defined
  by the model, and vector values should represent the associated column
  in the provided data set.

- ...:

  arguments passed on to `dose_grid_search`

## Value

Returns a PKPDsim regimen populated with the model-predicted dose
