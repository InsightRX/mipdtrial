# Calculate time to target attainment

For AUC targets, this function returns the first time of the start of
the dosing interval plus the infusion length where the AUC24 or 12 is on
target.

## Usage

``` r
calc_time_to_target(regimen, target_design, auc_comp, model, ...)
```

## Arguments

- regimen:

  PKPDsim regimen object

- target_design:

  target design, created using
  [`create_target_design()`](create_target_design.md)

- auc_comp:

  compartment to look for AUC

- model:

  PKPDsim model object

- ...:

  arguments passed on to
  [`PKPDsim::sim`](https://insightrx.github.io/PKPDsim/reference/sim.html)

## Value

a numeric value indicating the time, in hours, if a dosing interval
contained the target metric within range. Returns Inf if target was
never achieved. Returns NA if the target type is not supported.

## Details

For trough targets, this returns the time of start of the dosing
interval plus the infusion length immediately prior to the trough being
on target (i.e. the dose + infusion length of the first dose that
brought the trough to target).
