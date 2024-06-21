#' Create sampling schemes (static or adaptive) for use in simulated trial
#'
#' Sampling times can usually be pre-specified if the dosing schedule is fixed.
#' In that case a scheme can be specified with static timepoints e.g.
#' `time = c(1, 7.5, 25, 31.5)` for taking peak-trough samples at dose
#' 1 and 4 for an 8-hour regimen.
#'
#' However, when the dosing interval is adaptive, the peak and trough sampling
#' times cannot stay static and have to be adapted on-the-fly during
#' simulations.
#'
#' @param time a vector of numeric values. If no `anchor` values are specified,
#' these will be used as the fixed sampling times in the simulated trial.
#' If `anchor` values are supplied, the sampling times  will be calculated
#' adaptively during the trial. The `anchor` determine which dose is
#' used as reference anchor, and `time` will be relative to the specified
#' `anchor`.
#' @param offset_base character vector of same length as `time` (or single
#' value) determining how to interpret the provided sampling `time`. If `NULL`
#' will use the dose time as offset (default). Other options are `cmax` or
#' `peak`, which will use the end of infusion as the base for the `time`, or
#' `cmin` or `trough`, which will use the time of next dose as the offset.
#' @param anchor numeric vector of the dose or day number to "anchor"
#' the sampling times to. Vector needs to be of same length as `t`.
#' If `anchor_by` is set to `day`, then the first dose in that day is used.
#' If later doses in the day are preferred, the anchor can also be specified
#' fractionally, e.g. `1.5` will use the time of the first dose in the
#' second half of the 1st day.
#' @param anchor_by either `day` or `dose`. Single value required, i.e. anchor
#' types cannot be mixed.
#'
#' @export
#'
create_sampling_scheme <- function(
  time,
  offset_base = NULL,
  anchor = NULL,
  anchor_by = c("day", "dose")
) {
  anchor_by <- match.arg(anchor_by)
  if(!is.null(anchor)) {
    offset_base <- check_offset_base(offset_base, time, anchor)
    scheme <- data.frame(
      base = offset_base,
      offset = time,
      anchor = anchor,
      anchor_by = anchor_by
    )
  } else {
    if(! all(is.numeric(time))) {
      stop("When not anchoring times to `dose` or `day`, `time` must be all numeric.")
    }
    scheme <- data.frame(
      base = "dose",
      offset = time,
      anchor = 1,
      anchor_by = "dose"
    )
  }
  scheme
}

#' Check / clean offset_base element
#'
check_offset_base <- function(offset_base, time, anchor) {
  if(length(anchor) != length(time)) {
    stop("Please specify `anchor` with same length as `time`")
  }
  if(length(offset_base) == 1) offset_base <- rep(offset_base, length(time))
  if(length(offset_base) != length(time)) {
    stop("Please specify `offset` with same length as `time`, or as single value.")
  }
  allowed_bases <- c("dose", "trough", "peak", "cmax", "cmin")
  if(any(! unique(offset_base) %in% c(allowed_bases))) {
    stop(
      "Please specify only any of: ", paste(allowed_bases, collapse = ", "),
      " when using adaptive times."
    )
  }
  offset_base
}
