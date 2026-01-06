# Creates a design for the initial regimen for patients in the trial

Creates a design for the initial regimen for patients in the trial

## Usage

``` r
create_initial_regimen_design(method, ...)
```

## Arguments

- method:

  a function to call. The function should always return a PKPDsim
  regimen object.

- ...:

  options passed on depending on `method`. Commonly includes `regimen`,
  a named list of arguments passed on to PKPDsim::new_regimen (see
  examples)

## Value

a list object with the design specs

## Examples

``` r
create_initial_regimen_design(
  method = weight_based_starting_dose,
  mgkg = 15,
  dosing_weight = "total_body_weight",
  regimen = list(
    interval = 12,
    n = 10,
    t_inf = 1,
    type = "infusion"
  )
)
#> $method
#> function (covariates, design, cov_mapping) 
#> {
#>     reg_def <- design$initial_regimen$regimen
#>     if (is.null(design$initial_regimen$mgkg)) {
#>         cli::cli_abort(paste0("For weight-based initial dosing, ", 
#>             "specify `create_initial_regimen_design(mgkg = ...)`"))
#>     }
#>     if (is.null(design$initial_regimen$dosing_weight)) {
#>         cli::cli_abort(paste0("For weight-based initial dosing, ", 
#>             "specify `create_initial_regimen_design(dosing_weight = ...)`"))
#>     }
#>     cov_for_dosing <- names(which(cov_mapping == design$initial_regimen$dosing_weight))
#>     if (is.null(cov_for_dosing) || length(cov_for_dosing) == 
#>         0) {
#>         cli::cli_abort("dosing_weight covariate missing from `cov_mapping`")
#>     }
#>     weight_for_dosing <- covariates[[cov_for_dosing]]$value
#>     if (is.null(weight_for_dosing)) {
#>         cli::cli_abort("Dosing weight not available in data set")
#>     }
#>     reg_def$amt <- weight_for_dosing * design$initial_regimen$mgkg
#>     do.call(PKPDsim::new_regimen, reg_def)
#> }
#> <bytecode: 0x564e176261e0>
#> <environment: namespace:mipdtrial>
#> 
#> $mgkg
#> [1] 15
#> 
#> $dosing_weight
#> [1] "total_body_weight"
#> 
#> $regimen
#> $regimen$interval
#> [1] 12
#> 
#> $regimen$n
#> [1] 10
#> 
#> $regimen$t_inf
#> [1] 1
#> 
#> $regimen$type
#> [1] "infusion"
#> 
#> 
```
