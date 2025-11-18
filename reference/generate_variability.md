# Generate variability terms

To enable comparison of multiple treatment conditions in a reproducible
manner, it is recommended that interindividual variability terms and
residual variability terms be generated prior to all analyses. This
design also allows for resuming a simulation part-way through, when the
random seed position may not be known.

Generate IIV for one or more individuals and one or more iterations per
individual according to the supplied omega matrix.

Generate unexplained variability for one or more individuals and one or
more iterations per individual according to the supplied proportional
and additive error.

## Usage

``` r
generate_iiv(
  sim_model,
  omega,
  parameters,
  ids = 1,
  n_iter = 1,
  seed = NULL,
  ...
)

generate_ruv(tdm_sample_time, prop, add, ids = 1, n_iter = 1, seed = NULL)
```

## Arguments

- sim_model:

  model used for simulated patient response ("truth").

- omega:

  omega matrix, with covariance terms. See
  [`PKPDsim::sim`](https://insightrx.github.io/PKPDsim/reference/sim.html)
  for details.

- parameters:

  simulation model parameters (population estimates), a named list.

- ids:

  vector of ids, can be numeric or character.

- n_iter:

  number of sets of individual parameters to generate per id

- seed:

  set random seed

- ...:

  arguments passed on to PKPDsim::sim

- tdm_sample_time:

  time of tdm, since start of treatment course (or other vector of
  identifiers to use for each tdm). For example, for three days of daily
  dosing and peak-trough sample collection, `c(1, 24, 25, 48, 49, 73)`.

- prop:

  proportional error

- add:

  additive error

## Value

`generate_iiv` a data frame with columns `id` (corresponding to `ids`),
`iter` ( numbers 1 to n_iter) and columns for each individual parameter
value.

`generate_ruv` returns a data frame with identifier columns of
`tdm_number`, `iteration`, `id`, plus columns for proportional (`prop`)
and additive (`add`) error.

## Details

This family of functions generates variability terms to allow for
reproducible analyses. Using multiple iterations per individual ID
allows for PK variability within one set of covariates.

By default, `generate_iiv`assumes a log-normal (exponential)
distribution. See
[`PKPDsim::sim`](https://insightrx.github.io/PKPDsim/reference/sim.html)
documentation for the `omega_type` argument to provide finer grain
control.

`generate_ruv` Assumes a normal distribution for proportional and
additional error.
