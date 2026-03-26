# Simulate TDM collection

Using the "ground truth" model, simulate collection of drug/biomarker
levels. Returns a data frame of sample time, true value, and measured
value (with residual error added).

## Usage

``` r
collect_tdms(
  sim_model,
  t_obs,
  res_var,
  pars_i,
  lloq = NULL,
  est_model = NULL,
  est_pars_i = NULL,
  ...
)
```

## Arguments

- sim_model:

  model used for simulated patient response ("truth").

- t_obs:

  sample collection times (since start of treatment)

- res_var:

  data frame of residual variability quantities to add. See
  `pregenerate_ruv` for details. Expects columns `prop` and `add`. Error
  is added in the linear domain, so log-transformed models should supply
  error as proportional error with 0 additive error.

- pars_i:

  true parameters for the individual (named list)

- lloq:

  lower limit of quantification. If non-NULL, all TDMs below LLOQ will
  be set to half the LLOQ.

- est_model:

  model used for estimation (e.g. the model used in MAP fitting). If
  provided, a predictive individual prediction is simulated using
  `est_pars_i` and stored in the `predictive_ipred` column.

- est_pars_i:

  parameters for `est_model`. Typically population or MAP-estimated
  individual parameters.

- ...:

  arguments passed on to PKPDsim::sim

## Value

a data frame with columns `t` (time), `true_y` (actual level), `y`
(measured level), and `predictive_ipred` (predicted level from
estimation model; `NA` if `est_model` is not supplied), with rows
corresponding to t_obs.
