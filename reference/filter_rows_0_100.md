# Filter rows with values 0 or 100

Remove all rows of 0 or 1 if there are at least 2 non-(0,1) rows, and
otherwise keep all but the highest 0 row and the lowest 1 row.

## Usage

``` r
filter_rows_0_100(tab)
```

## Arguments

- tab:

  data frame with columns `dose` and `y`

## Details

Taken from a proprietary package, written by Kara Woo.
