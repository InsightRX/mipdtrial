# Calculate exposure metrics

Calculates drug concentration or area under the curve (AUC) for a MIPD
trial run.

## Usage

``` r
calc_concentration_from_regimen(regimen, parameters, model, target_design, ...)

calc_auc_from_regimen(regimen, parameters, model, target_design, ...)
```

## Arguments

- regimen:

  PKPDsim regimen object

- parameters:

  use MAP estimation to get estimated exposure, use true patient
  parameters to get true exposure. Parameters must correspond to the
  model used. Accepts parameters supplied as a data frame row, a named
  vector or as a list.

- model:

  model to use for exposure calculations.

- target_design:

  target design, created using
  [`create_target_design()`](create_target_design.md)

- ...:

  arguments passed on to PKPDsim::sim. Typical arguments include
  `covariates` or `iov_bins`

## Value

`calc_concentration_from_regimen` returns a numeric vector of
concentrations between each simulated time point. Control when
concentration is estimated using `target_time`.

`calc_auc_from_regimen` returns a numeric vector of AUCs between each
simulated time point. Control time period over which AUC should be
calculated using `target_time`.

## Details

This family of functions calculates the exposure metric for a given
regimen, model, model parameters, and target design. Supply the final
regimen and the final parameter estimates to get the final estimated
exposure metric. Supply the final regimen and the true individual
parameter estimates to get the final true exposure metric.
