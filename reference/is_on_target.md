# Checks if a value (or vector of values) is within the specified target range

Checks if a value (or vector of values) is within the specified target
range

## Usage

``` r
is_on_target(v, target)
```

## Arguments

- v:

  exposure metric, a single value or a vector

- target:

  target specification created with
  [`create_target_design()`](create_target_design.md), or a named list
  with `min` and `max` specified.

## Value

Returns a logical value of `TRUE` or `FALSE` for each value in v.
