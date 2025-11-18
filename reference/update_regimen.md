# Update a regimen with a new dose

Update a regimen with a new dose

## Usage

``` r
update_regimen(
  regimen,
  new_dose = NULL,
  new_interval = NULL,
  dose_update_number
)
```

## Arguments

- regimen:

  PKPDsim regimen object

- new_dose:

  value of new dose

- new_interval:

  value of new interval

- dose_update_number:

  integer indicating which dose and onwards should be updated

## Value

Returns a PKPDsim regimen object with the new dose applied.
