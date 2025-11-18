# Calculate sampling times based on a given sampling schema and a regimen.

Calculate sampling times based on a given sampling schema and a regimen.

## Usage

``` r
get_sampling_times_from_scheme(scheme, regimen)
```

## Arguments

- scheme:

  sampling schema created using `create_tdm_schema()`

- regimen:

  a `PKPDsim` regimen object

## Value

vector of numeric sampling times
