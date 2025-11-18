# Simulate TDM collection

Using the "ground truth" model, simulate collection of drug/biomarker
levels. Returns a data frame of sample time, true value, and measured
value (with residual error added).

## Usage

``` r
collect_tdms(sim_model, t_obs, res_var, pars_i, lloq = NULL, ...)
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

- ...:

  arguments passed on to PKPDsim::sim

## Value

a data frame with columns `t` (time), `true_y` (actual level) and `y`
(measured level), with rows corresponding to t_obs.
