# Run an MIPD trial

This function is a wrapper function to make it easier to run a basic
MIPD trial with minimal function calls. For fine-tune control over
logic, it may be easier to compose each step of the precision-dosing
workflow yourself. For an example, see the "Busulfan MIPD" vignette.

## Usage

``` r
run_trial(
  data,
  design,
  cov_mapping,
  n_ids = NULL,
  seed = 0,
  verbose = FALSE,
  progress = TRUE,
  threads = 1
)
```

## Arguments

- data:

  data set to use for simulation, a data frame. Should contain
  covariates in columns, corresponding to values in `cov_mapping` and a
  column for `ID`, as a patient identifier.

- design:

  trial design, created using
  [`create_trial_design()`](create_trial_design.md)

- cov_mapping:

  a named vector indicating correspondence between model covariates and
  data frame columns. Vector names should reflect all covariates defined
  by the model, and vector values should represent the associated column
  in the provided data set.

- n_ids:

  number of subjects to use in simulated trial. If not specified, will
  use all subjects in `data`.

- seed:

  argument passed to [`set.seed()`](https://rdrr.io/r/base/Random.html)
  to set the seed for all random variability generation to ensure
  reproducibility of results. Even when run in parallel, reproducibility
  is ensures since each thread will receive a reproducible seed number.

- verbose:

  verbose output?

- progress:

  show progress bar? (default is `TRUE`)

- threads:

  number of threads to run the simulations on. By default the
  simulations will run on 4 cores, or less if less are available (one
  core will always be reserved). The user can override by specifying
  manually.

## Details

This function performs the following steps:

1.  Dose the patient using the function specified in
    `initial_dose_method`.

2.  Collect levels at the specified times (`tdm_times`).

3.  Adjust dosing at each specified dose (`adjust_doses_at`) using the
    logic specified in `dose_optimization_method`

4.  Return an object with data collected during and at the end of the
    MIPD trial.
