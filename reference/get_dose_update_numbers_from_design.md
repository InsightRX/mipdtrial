# Get dose number to update dose/interval at from the regime update scheme and a provided regimen.

Get dose number to update dose/interval at from the regime update scheme
and a provided regimen.

## Usage

``` r
get_dose_update_numbers_from_design(design, regimen)
```

## Arguments

- design:

  regimen update design created using
  [`create_regimen_update_design()`](create_regimen_update_design.md)

- regimen:

  a `PKPDsim` regimen object

## Value

vector of dose numbers to update at
