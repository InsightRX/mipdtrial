# Checks that an object represents a single finite number

Checks that an object represents a single finite number

## Usage

``` r
is_valid_number(x)
```

## Arguments

- x:

  an object to check

## Examples

``` r
mipdtrial:::is_single_valid_number(9)
#> [1] TRUE
mipdtrial:::is_single_valid_number(mtcars)
#> [1] FALSE
mipdtrial:::is_single_valid_number(c(1, 2))
#> [1] FALSE
```
