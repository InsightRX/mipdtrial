# Create a list of PKPDsim covariates for modeling

Data sets typically come in rectangular formats while for MIPD trials we
model each individual separately. PKPDsim expects covariates to be
provided as a list of PKPDsim covariates. This function helps convert
rectangular (wide) data to model-ready covariates.

## Usage

``` r
create_cov_object(dat_i, mapping, implementation = NULL, time_column = NULL)
```

## Arguments

- dat_i:

  a rectangular data set, containing only rows for an individual

- mapping:

  named vector or list indicating which columns to extract, and what the
  covariate names are expected in the model. Names should be model
  covariates and values should be data frame column names.

- implementation:

  named vector indicating covariate implementation, one of
  `c("interpolate", "locf")`. If NULL, assumes "interpolate" for all
  covariates. See
  [`PKPDsim::new_covariate`](https://insightrx.github.io/PKPDsim/reference/new_covariate.html)
  for details. Names should be model covariates and values should be
  implementation method.

- time_column:

  if covariates are time-varying, supply a column for time. Otherwise,
  initial value only is taken for all covariates.

## Value

Returns a named list of PKPDsim covariates.

## Details

The data is assumed to already be numeric (e.g., categorical covariates
like smoking status have already been mapped to 1 or 0).

The function returns NULL if required columns are missing or NA.

## Examples

``` r
dat <- data.frame(ID = 1, weight = 70, CRCL = 4)
mapping <- c(WT = "weight", CRCL = "CRCL")
create_cov_object(dat, mapping)
#> $WT
#>   value times      unit implementation
#> 1    70     0 undefined    interpolate
#> 
#> $CRCL
#>   value times      unit implementation
#> 1     4     0 undefined    interpolate
#> 
```
