# Core function to simulate a single subject

Core function to simulate a single subject

## Usage

``` r
sim_subject(data, cov_mapping, pars_true_i, design, verbose = FALSE, ...)
```

## Arguments

- data:

  data for subject

- cov_mapping:

  a named vector indicating correspondence between model covariates and
  data frame columns. Vector names should reflect all covariates defined
  by the model, and vector values should represent the associated column
  in the provided data set.

- pars_true_i:

  PK parameters for the individual. See `generate_iiv`.

- design:

  trial design, created using
  [`create_trial_design()`](create_trial_design.md)

- verbose:

  verbose output?

- ...:

  arguments passed on to `simulate_fit` or dose_optimization_method
  function.
