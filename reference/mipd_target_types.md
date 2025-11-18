# Accepted PK/PD exposure targets

Model-based dose-finding is currently implemented for the following
target types:

- peak / cmax: Peak concentration

- cmax_1hr: Peak concentration 1hr after dose

- trough, cmin: Trough concentration

- conc: generic concentration

- cum_auc: Cumulative AUC

- auc: auc over a dosing interval

- auc24: auc normalized to a 24-hour period

- auc12: auc normalized to a 12-hour period

## Usage

``` r
mipd_target_types()
```

## Value

Returns a character vector of accepted target types.
