# Bind together the results from sampling and dose adjusting

Bind together the results from sampling and dose adjusting

## Usage

``` r
bind_results_from_adjustments(
  out,
  j,
  regimen,
  adjust_at_dose,
  dose_before_update,
  auc_final,
  trough_final
)
```

## Arguments

- out:

  output object

- j:

  index number

- regimen:

  regimen

- adjust_at_dose:

  adjust at dose number

- dose_before_update:

  dose before update

- auc_final:

  final AUC

- trough_final:

  final Ctrough

## Value

a data.frame with results
