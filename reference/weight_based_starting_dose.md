# Weight-based starting dose (e.g., mg/kg)

This nomogram is a possible `method` supplied to
[`create_initial_regimen_design()`](create_initial_regimen_design.md).
When this method is used,
[`create_initial_regimen_design()`](create_initial_regimen_design.md)
must also be supplied with `mgkg` and `dosing_weight`, which must match
a column name in the supplied data set.

## Usage

``` r
weight_based_starting_dose(covariates, design, cov_mapping)
```

## Arguments

- covariates:

  named list of PKPDsim covariates. E.g.: list(WT =
  PKPDsim::new_covariate(70))

- design:

  trial design, created using
  [`create_trial_design()`](create_trial_design.md)

- cov_mapping:

  a named vector indicating correspondence between model covariates and
  data frame columns. Vector names should reflect all covariates defined
  by the model, and vector values should represent the associated column
  in the provided data set.
