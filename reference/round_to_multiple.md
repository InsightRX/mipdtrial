# Round to a multiple of any number (e.g. round to the nearest 5, 10, 100)

Round to a multiple of any number (e.g. round to the nearest 5, 10, 100)

## Usage

``` r
round_to_multiple(x, multiple)
```

## Arguments

- x:

  value to be rounded

- multiple:

  accuracy to round to. If NULL, x will be returned unrounded.

## Examples

``` r
# Uses "round-to-even" strategy
round_to_multiple(12.5, 5)
#> [1] 10
round_to_multiple(17.5, 5)
#> [1] 20
```
