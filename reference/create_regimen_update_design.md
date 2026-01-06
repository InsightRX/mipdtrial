# Create scheme for updating dose or interval during dose optimization trial

Create scheme for updating dose or interval during dose optimization
trial

## Usage

``` r
create_regimen_update_design(
  at,
  anchor,
  update_type = c("dose", "interval"),
  dose_optimization_method = map_adjust_dose,
  ...
)
```

## Arguments

- at:

  numeric vector of the dose or day number to "anchor" the sampling
  times to. Vector needs to be of same length as `time`. If `anchor` is
  set to `day`, then the first dose in that day is used. If later doses
  in the day are preferred, the anchor can also be specified
  fractionally, e.g. `1.5` will use the time of the first dose in the
  second half of the 1st day.

- anchor:

  either `day` or `dose`. Single value required, i.e. anchor types
  cannot be mixed.

- update_type:

  update either future `dose` (default) or `interval`.

- dose_optimization_method:

  A function that determines how doses should be adjusted given the
  collected drug levels. The function must return a named list of the
  structure `list(regimen = reg, additional_info = x)`, where `reg` is
  the updated PKPDsim regimen for the patient and `x` can be another
  other information useful for post-processing of trial results.

- ...:

  arguments passed on to `dose_optimization_method`

## Examples

``` r
create_regimen_update_design(
  at = c(2, 5, 8),
  anchor = "dose"
)
#> $dose_optimization_method
#> function (tdms, est_model, parameters, omega, ruv, regimen, covariates = NULL, 
#>     target_design, dose_update, grid = NULL, settings = NULL, 
#>     ...) 
#> {
#>     fit <- simulate_fit(est_model = est_model, parameters = parameters, 
#>         omega = omega, ruv = ruv, tdms = tdms, covariates = covariates, 
#>         regimen = regimen, ...)
#>     if (!all(c("parameters", "pred", "ipred", "dv", "weights") %in% 
#>         names(fit))) {
#>         cli::cli_warn("Fit error, please investigate. Continuing but using population estimates.")
#>         est_par <- parameters
#>         gof <- data.frame(pred = NA, ipred = NA, dv = NA, weights = NA)
#>     }
#>     else {
#>         est_par <- fit$parameters
#>         gof <- data.frame(pred = fit$pred, ipred = fit$ipred, 
#>             dv = fit$dv, weights = fit$weights)
#>     }
#>     if (is.null(grid)) {
#>         d1 <- regimen$dose_amts[1]
#>         grid <- seq(d1/5, d1 * 5, length.out = 10)
#>     }
#>     new_dose <- dose_grid_search(est_model = est_model, regimen = regimen, 
#>         parameters = est_par, target_design = target_design, 
#>         auc_comp = PKPDsim::get_model_auc_compartment(est_model), 
#>         dose_update = dose_update, grid = grid, grid_type = "dose", 
#>         covariates = covariates, iov_bins = PKPDsim::get_model_iov(est_model)$bins, 
#>         dose_resolution = settings$dose_resolution, ...)
#>     regimen <- update_regimen(regimen, new_dose = new_dose, dose_update_number = dose_update)
#>     list(regimen = regimen, dose_update = dose_update, new_dose = new_dose, 
#>         new_interval = NA, additional_info = est_par, gof = gof)
#> }
#> <bytecode: 0x564e14a919d8>
#> <environment: namespace:mipdtrial>
#> 
#> $scheme
#>   base offset at anchor scatter update_type
#> 1 dose      0  2   dose       0        dose
#> 2 dose      0  5   dose       0        dose
#> 3 dose      0  8   dose       0        dose
#> 
#> $args
#> list()
#> 
create_regimen_update_design(
  at = c(4, 8),
  anchor = "day",
  update_type = "interval",
  dose_optimization_method = map_adjust_interval
)
#> $dose_optimization_method
#> function (tdms, est_model, parameters, omega, ruv, regimen, covariates = NULL, 
#>     target_design, dose_update, grid = NULL, settings = NULL, 
#>     ...) 
#> {
#>     fit <- simulate_fit(est_model = est_model, parameters = parameters, 
#>         omega = omega, ruv = ruv, tdms = tdms, covariates = covariates, 
#>         regimen = regimen, ...)
#>     if ("error" %in% class(fit)) {
#>         cli::cli_abort("Fit to simulated data failed, please check model and simulation designs.")
#>     }
#>     est_par <- fit$parameters
#>     gof <- data.frame(pred = fit$pred, ipred = fit$ipred, dv = fit$dv, 
#>         weights = fit$weights)
#>     if (is.null(grid)) {
#>         cli::cli_abort("Interval-optimization requires `grid` argument.")
#>     }
#>     new_interval <- dose_grid_search(est_model = est_model, regimen = regimen, 
#>         parameters = est_par, target_design = target_design, 
#>         auc_comp = PKPDsim::get_model_auc_compartment(est_model), 
#>         dose_update = dose_update, grid = grid, grid_type = "interval", 
#>         covariates = covariates, iov_bins = PKPDsim::get_model_iov(est_model)$bins, 
#>         verbose = TRUE, ...)
#>     regimen <- update_regimen(regimen, new_interval = new_interval, 
#>         dose_update_number = dose_update)
#>     list(regimen = regimen, dose_update = dose_update, new_dose = NA, 
#>         new_interval = new_interval, additional_info = est_par, 
#>         gof = gof)
#> }
#> <bytecode: 0x564e1495bd00>
#> <environment: namespace:mipdtrial>
#> 
#> $scheme
#>   base offset at anchor scatter update_type
#> 1 dose      0  4    day       0    interval
#> 2 dose      0  8    day       0    interval
#> 
#> $args
#> list()
#> 
```
