# Checks that an object represents a vectir of finite number with no NA or NaN or Inf

Checks that an object represents a vectir of finite number with no NA or
NaN or Inf

## Usage

``` r
is_valid_numeric_vector(x)
```

## Arguments

- x:

  an object to check

## Examples

``` r
mipdtrial:::is_valid_numeric_vector(9)
#> [1] TRUE
mipdtrial:::is_valid_numeric_vector(mtcars)
#> [1] FALSE
mipdtrial:::is_valid_numeric_vector(c(1, 2))
#> [1] TRUE
```
