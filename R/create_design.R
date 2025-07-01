#' Create timing designs (static or adaptive) for use in simulated trial, such
#' as sampling designs, target designs etc.
#'
#' This function is used by the three functions `create_sampling_design()`,
#' `create_regimen_update_design()` and `create_target_design()`, but is itself
#' not intended for direct usage.
#'
#' @param time a vector of numeric values. If no `at` values are specified,
#' these will be used as the fixed sampling times in the simulated trial.
#' If `at` values are supplied, the sampling times  will be calculated
#' adaptively during the trial. The `at` determine which dose or day is
#' used as reference, and `time` will be relative to the specified `at` anchor.
#' @param when character vector of same length as `time` (or single
#' value) determining how to interpret the provided sampling `time`. If `NULL`
#' will use the dose time as offset (default). Other options are: `cmax` or
#' `peak`, which will use the end of infusion as the base for the `time`, or
#' `cmin` or `trough`, which will use the time of next dose as the offset, or
#' `middle` or `cmid` which will use the middle between the anchored dose and
#' the next, or `random` which takes a random time point between the anchored
#' dose and the next.
#' @param offset offset from standardized PK moments specified in `when`, e.g.
#' `c(1, -1)` with `when = c("peak", "trough")` to sample 1 hour after peak and
#' 1 hour before trough.
#' @param scatter optional random variation in time, specified as the standard
#' deviation, e.g. `scatter = 0.1` to allow for variation in sampling time with
#' an SD of 0.1 hours. Only relevant for sampling times, not for
#' regimen_update designs or target designs. Random variation does not protect
#' for peaks or troughs becoming sampled during infusion or in previous / next
#' dose. So value for `scatter` should be chosen appropriately and probably
#' used in conjunction with approriate `offset` values.
#' @param at numeric vector of the dose or day number to "anchor"
#' the sampling times to. Vector needs to be of same length as `time`.
#' If `anchor` is set to `day`, then the first dose in that day is used.
#' If later doses in the day are preferred, the anchor can also be specified
#' fractionally, e.g. `1.5` will use the time of the first dose in the
#' second half of the 1st day.
#' @param anchor either `day` or `dose`. Single value required, i.e. anchor
#' types cannot be mixed.
#'
create_design <- function(
  time = NULL,
  when = NULL,
  offset = NULL,
  scatter = NULL,
  at = NULL,
  anchor = c("dose", "day")
) {
  if(!is.null(time) && (!is.null(when) | !is.null(offset))) {
    cli::cli_abort("`time` cannot be specified at the same with `when` and/or `offset` arguments.")
  }
  anchor <- match.arg(anchor)
  if(is.null(time)) {
    if(!is.null(when) && is.null(at)) {
      at <- rep(1, length(when))
    }
    if(is.null(offset)) {
      offset <- rep(0, length(at))
    } else {
      if(length(offset) == 1) {
        offset <- rep(offset, length(at))
      }
    }
    if(is.null(scatter)) {
      scatter <- rep(0, length(at))
    } else {
      if(length(scatter) == 1) {
        scatter <- rep(scatter, length(at))
      }
    }
    when <- check_when(when, offset, at)
    scheme <- data.frame(
      base = when,
      offset = offset,
      at = at,
      anchor = anchor,
      scatter = scatter
    )
  } else {
    if(! all(is.numeric(time))) {
      cli::cli_abort("When not anchoring times to `dose` or `day`, `time` must be all numeric.")
    }
    scheme <- data.frame(
      base = "dose",
      offset = time,
      at = 1,
      anchor = "dose"
    )
  }
  scheme
}

#' Check / clean when element
#'
#' @inheritParams create_design
#'
check_when <- function(when, offset, at) {
  if(length(at) != length(offset)) {
    cli::cli_abort("Please specify `at` with same length as `offset`")
  }
  if(length(when) == 1) when <- rep(when, length(offset))
  if(length(when) != length(offset)) {
    cli::cli_abort("Please specify `when` with same length as `offset`, or as single value.")
  }
  allowed_bases <- c("dose", "trough", "peak", "cmax", "cmin", "middle", "cmid", "random")
  if(!all(unique(when) %in% c(allowed_bases))) {
    cli::cli_abort(
      paste0(
        "Please specify only any of: ", paste(allowed_bases, collapse = ", "),
        " when using adaptive times."
      )
    )
  }
  when
}
