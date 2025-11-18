# Get a single target from a potentially time-varying target design

In most cases, the target design will just specify a single target. But
in some scenarios, a time-varying target may make sense. In that case,
for each regimen update instance, a different target can be specified.
This function just grabs the right target values from the overall target
design. If just a single target is specified, it will just return the
target design as-is.

## Usage

``` r
get_single_target_design(target_design, idx = nrow(target_design$scheme))
```

## Arguments

- target_design:

  target design, created using `create_target_design`.

- idx:

  number of regimen update. By default it will take the last instance.
