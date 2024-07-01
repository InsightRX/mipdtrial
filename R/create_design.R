#' Create timing designs (static or adaptive) for use in simulated trial, such
#' as sampling designs, target designs etc.
#'
#' This function is used by the three functions `create_sampling_design()`,
#' `create_regimen_update_design()` and `create_target_design()`, but is itself
#' not intended for direct usage.
#'
#' @param time a vector of numeric values. If no `anchor` values are specified,
#' these will be used as the fixed sampling times in the simulated trial.
#' If `anchor` values are supplied, the sampling times  will be calculated
#' adaptively during the trial. The `anchor` determine which dose is
#' used as reference anchor, and `time` will be relative to the specified
#' `anchor`.
#' @param offset_from character vector of same length as `time` (or single
#' value) determining how to interpret the provided sampling `time`. If `NULL`
#' will use the dose time as offset (default). Other options are `cmax` or
#' `peak`, which will use the end of infusion as the base for the `time`, or
#' `cmin` or `trough`, which will use the time of next dose as the offset.
#' @param anchor numeric vector of the dose or day number to "anchor"
#' the sampling times to. Vector needs to be of same length as `time`.
#' If `anchor_by` is set to `day`, then the first dose in that day is used.
#' If later doses in the day are preferred, the anchor can also be specified
#' fractionally, e.g. `1.5` will use the time of the first dose in the
#' second half of the 1st day.
#' @param anchor_by either `day` or `dose`. Single value required, i.e. anchor
#' types cannot be mixed.
#'
create_design <- function(
  time,
  offset_from = NULL,
  anchor = NULL,
  anchor_by = c("day", "dose")
) {
  anchor_by <- match.arg(anchor_by)
  if(!is.null(anchor)) {
    offset_from <- check_offset_from(offset_from, time, anchor)
    scheme <- data.frame(
      base = offset_from,
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

#' Check / clean offset_from element
#'
#' @inheritParams create_design
#'
check_offset_from <- function(offset_from, time, anchor) {
  if(length(anchor) != length(time)) {
    stop("Please specify `anchor` with same length as `time`")
  }
  if(length(offset_from) == 1) offset_from <- rep(offset_from, length(time))
  if(length(offset_from) != length(time)) {
    stop("Please specify `offset_from` with same length as `time`, or as single value.")
  }
  allowed_bases <- c("dose", "trough", "peak", "cmax", "cmin")
  if(any(! unique(offset_from) %in% c(allowed_bases))) {
    stop(
      "Please specify only any of: ", paste(allowed_bases, collapse = ", "),
      " when using adaptive times."
    )
  }
  offset_from
}
