# Create target object

This function helps the user define the PKPD target for a simulation.
When a minimum value and maximum value are supplied, the algorithm
targets the mid- point. Alternatively, a single midpoint can be
supplied.

## Usage

``` r
create_target_design(
  targettype = mipd_target_types(),
  targetmin = NULL,
  targetmax = NULL,
  targetvalue = NULL,
  single_point_variation = 0.2,
  time = NULL,
  when = NULL,
  offset = NULL,
  at = NULL,
  anchor = c("dose", "day")
)
```

## Arguments

- targettype:

  target type, one of accepted types (see
  [`mipd_target_types()`](mipd_target_types.md))

- targetmin:

  minimum value acceptable, must be specified with `targetmax`. Can be a
  vector. In that case, length has to match length of other
  vector-arguments to this function, and length of regimen update
  occasions.

- targetmax:

  maximum value acceptable, must be specified with `targetmin`. Can be a
  vector. In that case, length has to match length of other
  vector-arguments to this function, and length of regimen update
  occasions.

- targetvalue:

  value for a target, overrides min and max values. Can be a vector. In
  that case, length has to match length of other vector-arguments to
  this function, and length of regimen update occasions.

- single_point_variation:

  acceptable variation from targetvalue. By default 20%. Considered for
  assessment of target attainment a posteriori, not used for
  dose-finding logic.

- time:

  a vector of numeric values at which to measure and optimize the
  target. In most cases `time` is not required as argument, and will be
  inferred from the the `targettype`. If `at` values are supplied, the
  target times will be calculated adaptively during the trial. The `at`
  determine which dose is used as reference anchor. and `time` will be
  relative to the specified `at`. If no `at` values are specified, the
  `time` values will be used as the fixed absolute target times in the
  simulated trial.

- when:

  character vector of same length as `time` (or single value)
  determining how to interpret the provided target `time`. If `NULL`
  will use the dose time as offset (default). Other options are `cmax`
  or `peak`, which will use the end of infusion as the base for the
  `time`, or `cmin` or `trough`, which will use the time of next dose as
  the offset.

- offset:

  offset from a `when` moment (dose, peak, or trough).

- at:

  numeric vector of the dose or day number to "anchor" the target times
  to. Vector needs to be of same length as `t`. If `anchor` is set to
  `day`, then the first dose in that day is used. If later doses in the
  day are preferred, the anchor can also be specified fractionally, e.g.
  `1.5` will use the time of the first dose in the second half of the
  1st day.

- anchor:

  either `day` or `dose`. Single value required, i.e. anchor types
  cannot be mixed.

## Examples

``` r
## Target cumulative AUC, always exactly at 72 hours:
create_target_design(
  targettype = "cum_auc",
  targetvalue = 90,
  time = 72
)
#> $type
#> [1] "cum_auc"
#> 
#> $value
#> [1] 90
#> 
#> $min
#> [1] 72
#> 
#> $max
#> [1] 108
#> 
#> $scheme
#>   base offset at anchor
#> 1 dose     72  1   dose
#> 

## Target trough concentration at trough after dose 4.
create_target_design(
  targettype = "cmin",
  targetvalue = 15,
  at = 4,
  anchor = "dose"
)
#> $type
#> [1] "cmin"
#> 
#> $value
#> [1] 15
#> 
#> $min
#> [1] 12
#> 
#> $max
#> [1] 18
#> 
#> $scheme
#>   base offset at anchor scatter
#> 1 cmin      0  4   dose       0
#> 

## Target AUC24 over day 4
create_target_design(
  targettype = "auc24",
  targetvalue = 500,
  at = 4,
  anchor = "day"
)
#> $type
#> [1] "auc24"
#> 
#> $value
#> [1] 500
#> 
#> $min
#> [1] 400
#> 
#> $max
#> [1] 600
#> 
#> $scheme
#>   base offset at anchor scatter
#> 1 dose     24  4    day       0
#> 

## Target cmin first at dose 4 (15 mg/L), then at dose 8 (25 mg/L)
create_target_design(
  targettype = "cmin",
  targetvalue = c(15, 25),
  at = c(4, 8),
  anchor = "day"
)
#> $type
#> [1] "cmin"
#> 
#> $value
#> [1] 15 25
#> 
#> $min
#> [1] 12 20
#> 
#> $max
#> [1] 18 30
#> 
#> $scheme
#>   base offset at anchor scatter
#> 1 cmin      0  4    day       0
#> 2 cmin      0  8    day       0
#> 
```
