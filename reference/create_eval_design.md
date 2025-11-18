# Create evaluation object

This function defines the evaluation metric and timing for non-target
metrics. Use this function to record outputs from the simulation like
troughs or AUC that are not at the target times.

## Usage

``` r
create_eval_design(
  evaltype = mipd_target_types(),
  time = NULL,
  when = NULL,
  offset = NULL,
  at = NULL,
  anchor = c("dose", "day")
)
```

## Arguments

- evaltype:

  evaluation metric(s) to use. Types from
  [`mipd_target_types()`](mipd_target_types.md)

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
