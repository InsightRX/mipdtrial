# Combine all sub-designs into the overall trial design object

Combine all sub-designs into the overall trial design object

## Usage

``` r
create_trial_design(
  file = NULL,
  sampling_design = NULL,
  target_design = NULL,
  initial_regimen_design = NULL,
  regimen_update_design = NULL,
  sim_design = NULL,
  est_design = NULL,
  eval_design = NULL
)
```

## Arguments

- file:

  a YAML file with all subdesigns

- sampling_design:

  Design for sampling, from
  [`create_sampling_design()`](create_sampling_design.md)

- target_design:

  Design for target attainment, from
  [`create_target_design()`](create_target_design.md)

- initial_regimen_design:

  Design for initial regimen, from
  [`create_initial_regimen_design()`](create_initial_regimen_design.md)

- regimen_update_design:

  Design for dose/regimen optimization, from
  [`create_regimen_update_design()`](create_regimen_update_design.md)

- sim_design:

  Design for simulation model, from
  [`create_model_design()`](create_model_design.md)

- est_design:

  Design for estimation, from
  [`create_model_design()`](create_model_design.md)

- eval_design:

  Design for evaluation metrics, from
  [`create_eval_design()`](create_eval_design.md)
