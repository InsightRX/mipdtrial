# Check / clean when element

Check / clean when element

## Usage

``` r
check_when(when, offset, at)
```

## Arguments

- when:

  character vector of same length as `time` (or single value)
  determining how to interpret the provided sampling `time`. If `NULL`
  will use the dose time as offset (default). Other options are: `cmax`
  or `peak`, which will use the end of infusion as the base for the
  `time`, or `cmin` or `trough`, which will use the time of next dose as
  the offset, or `middle` or `cmid` which will use the middle between
  the anchored dose and the next, or `random` which takes a random time
  point between the anchored dose and the next.

- offset:

  offset from standardized PK moments specified in `when`, e.g.
  `c(1, -1)` with `when = c("peak", "trough")` to sample 1 hour after
  peak and 1 hour before trough.

- at:

  numeric vector of the dose or day number to "anchor" the sampling
  times to. Vector needs to be of same length as `time`. If `anchor` is
  set to `day`, then the first dose in that day is used. If later doses
  in the day are preferred, the anchor can also be specified
  fractionally, e.g. `1.5` will use the time of the first dose in the
  second half of the 1st day.
