# Create a design for models to be used

This function should be used for setting up both the model to be used
for simulation as well as for estimation. To use PKPDsim models as
defined on installation, supplying the model as a string using the `lib`
argument is likely easiest. To modify the model parameters or error
models (e.g., for sensitivity analysis), the other arguments provide a
convenient API.

## Usage

``` r
create_model_design(
  lib = NULL,
  model = NULL,
  parameters = NULL,
  omega_matrix = NULL,
  ruv = NULL
)
```

## Arguments

- lib:

  PKPDsim model name, as a string.

- model:

  PKPDsim model object

- parameters:

  model parameters, as a named list

- omega_matrix:

  omega matrix, provided as a numeric vector containing the lower
  diagonal of the omega matrix.

- ruv:

  residual error model, as a named list

## Examples

``` r
if (FALSE) { # \dontrun{
create_model_design("pkbusulfanmccune")
requireNamespace("pkbusulfanmccune")
create_model_design(
  model = model,
  parameters = list(
    CL = 2.99, V = 0.675, Q = 2.28, V2 = 0.732,
    TH_CRCL = 0.0154, TDM_INIT = 0
  ),
  omega_matrix = c(0.0729, 0.01, 0.0225, 0, 0, 0.2401, 0, 0, 0, 1.69),
  ruv = list(prop = 0.1, add = 5)
)
} # }
```
