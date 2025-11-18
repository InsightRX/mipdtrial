# Get MAP Bayesian parameters

A light wrapper around
[`PKPDmap::get_map_estimates`](https://rdrr.io/pkg/PKPDmap/man/get_map_estimates.html)
and
[`PKPDmap::create_iov_object`](https://rdrr.io/pkg/PKPDmap/man/create_iov_object.html).
For more fine-grained control, use `PKPDmap` functions directly.

## Usage

``` r
simulate_fit(est_model, parameters, omega, ruv, tdms, covariates, regimen, ...)
```

## Arguments

- est_model:

  model used for estimation

- parameters:

  population parameters of estimation model

- omega:

  omega matrix of estimation model

- ruv:

  residual error distribution of estimation model, a named list with
  elements for `prop` (proportional) and `add` (additive).

- tdms:

  observations to be included in MAP estimates

- covariates:

  named list of PKPDsim covariates

- regimen:

  PKPDsim regimen object

- ...:

  arguments passed on to PKPDmap::get_map_estimates

## Value

named list of individual PK parameter estimates

## Details

MAP Bayesian fits sometimes fail when simulated values are very
unlikely. To prevent a long simulation from stalling, fit is wrapped in
a tryCatch, and NULL is returned for appropriate processing outside the
function.
