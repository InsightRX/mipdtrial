# Simulate different doses/intervals in a dose/interval grid

Simulate different doses/intervals in a dose/interval grid

## Usage

``` r
simulate_dose_interval(
  value,
  grid_type = "dose",
  dose_update,
  regimen,
  parameters,
  covariates = NULL,
  md,
  pta,
  target_design,
  model,
  omega,
  obs,
  ruv,
  ...
)
```

## Arguments

- value:

  element of the dose/interval grid

- grid_type:

  either `dose` grid or `interval` grid

- dose_update:

  update dose from which dose?

- regimen:

  PKPDsim regimen object

- parameters:

  list of model parameters

- covariates:

  covariates object

- md:

  metadata object (only needed if we have to use
  [`get_quantity_from_variable()`](get_quantity_from_variable.md) to
  generate target value)

- pta:

  probability of target attainment, list with arguments `type` and
  `value`, also requires `omega` if non-NULL. If `NULL`, will just aim
  for specific conc or auc.

- target_design:

  object specifying target design, created using function
  [`create_target_design()`](create_target_design.md)

- model:

  model for simulating dose (estimation model)

- omega:

  IIV matrix, for estimation model, for probability of target attainment
  target types.

- obs:

  Value of `obs` as determined by
  [`dose_grid_search()`](dose_grid_search.md) (i.e. either "obs" or AUC
  compartment)

- ruv:

  list specifying residual error for estimation model:
  `list(prop = 0.1, add = 1.5)`, for probability of target attainment
  target types.

- ...:

  passed on to PKPDsim function
