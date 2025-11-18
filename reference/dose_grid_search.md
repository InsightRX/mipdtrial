# Perform a grid search for a particular target by simulating a grid of doses

Set refine = TRUE if the model is nonlinear so that the grid search
happens iteratively.

## Usage

``` r
dose_grid_search(
  est_model = NULL,
  regimen,
  target_design = create_target_design(targettype = "conc", targetvalue = 10, time = 24),
  auc_comp = NULL,
  pta = NULL,
  omega = NULL,
  ruv = NULL,
  dose_update = 1,
  grid = seq(1, 6000, by = 10),
  grid_type = "dose",
  dose_resolution = 1,
  refine = NULL,
  refine_range = c(0.7, 1.4),
  check_boundaries = TRUE,
  max_dose = NULL,
  min_dose = NULL,
  md = list(),
  parameters = NULL,
  covariates = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- est_model:

  model used for estimation ("clinician facing")

- regimen:

  PKPDsim regimen object

- target_design:

  object specifying target design, created using function
  [`create_target_design()`](create_target_design.md)

- auc_comp:

  auc compartment (starting from 1, R-style not C-style!)

- pta:

  probability of target attainment, list with arguments `type` and
  `value`, also requires `omega` if non-NULL. If `NULL`, will just aim
  for specific conc or auc.

- omega:

  IIV matrix, for estimation model, for probability of target attainment
  target types.

- ruv:

  list specifying residual error for estimation model:
  `list(prop = 0.1, add = 1.5)`, for probability of target attainment
  target types.

- dose_update:

  update dose from which dose?

- grid:

  vector specifying doses or intervals to use as test grid, Example:
  `seq(from = 50, to = 500, by = (500 - 50) / 10)`

- grid_type:

  either "dose" or "interval"

- dose_resolution:

  to which precision should the output be rounded (e.g. 50), useful when
  in practice only a specific set of dose units. Can of course also be
  controlled by altering the grid.

- refine:

  should the found optimal dose be refined more? If not specified, will
  refine if the model linearity (in `attr(model, "misc")`) is not
  described as `"linear"`

- refine_range:

  after initial optimization, should a second refinement step be
  implemented? If `refine_range` is specified e.g. as `c(0.9, 1.1)` then
  it will implement a second optimization using a grid spanning from 90%
  to 110% of the initial optimal dose. Useful only for non-linear
  models.

- check_boundaries:

  if optimal dose is at lower/upper boundary of grid, should grid be
  expanded?

- max_dose:

  maximum dose cap

- min_dose:

  minimum dose cap

- md:

  metadata object (only needed if we have to use
  [`get_quantity_from_variable()`](get_quantity_from_variable.md) to
  generate target value)

- parameters:

  list of model parameters

- covariates:

  covariates object

- verbose:

  verbose output?

- ...:

  passed on to PKPDsim function

## Value

A numeric value indicating the recommended dose
