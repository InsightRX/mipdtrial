#' Create target object
#'
#' This function helps the user define the PKPD target for a simulation. When a
#' minimum value and maximum value are supplied, the algorithm targets the mid-
#' point. Alternatively, a single midpoint can be supplied.
#'
#' @param targettype target type, one of accepted types (see
#'   [mipd_target_types()])
#' @param targetmin minimum value acceptable, must be specified with 
#' `targetmax`. Can be a vector. In that case, length has to match length of
#' other vector-arguments to this function, and length of regimen update
#' occasions.
#' @param targetmax maximum value acceptable, must be specified with 
#' `targetmin`. Can be a vector. In that case, length has to match length of
#' other vector-arguments to this function, and length of regimen update
#' occasions.
#' @param targetvalue value for a target, overrides min and max values.
#' Can be a vector. In that case, length has to match length of
#' other vector-arguments to this function, and length of regimen update
#' occasions.
#' @param single_point_variation acceptable variation from targetvalue. By
#'   default 20%. Considered for assessment of target attainment a posteriori,
#'   not used for dose-finding logic.
#' @param time a vector of numeric values at which to measure and optimize the
#' target. In most cases `time` is not required as argument, and will be inferred
#' from the the `targettype`.
#' If `at` values are supplied, the target times will be calculated
#' adaptively during the trial. The `at` determine which dose is
#' used as reference anchor. and `time` will be relative to the specified
#' `at`. If no `at` values are specified, the `time` values will be
#' used as the fixed absolute target times in the simulated trial.
#' @param when character vector of same length as `time` (or single
#' value) determining how to interpret the provided target `time`. If `NULL`
#' will use the dose time as offset (default). Other options are `cmax` or
#' `peak`, which will use the end of infusion as the base for the `time`, or
#' `cmin` or `trough`, which will use the time of next dose as the offset.
#' @param offset offset from a `when` moment (dose, peak, or trough).
#' @param at numeric vector of the dose or day number to "anchor"
#' the target times to. Vector needs to be of same length as `t`.
#' If `anchor` is set to `day`, then the first dose in that day is used.
#' If later doses in the day are preferred, the anchor can also be specified
#' fractionally, e.g. `1.5` will use the time of the first dose in the
#' second half of the 1st day.
#' @param anchor either `day` or `dose`. Single value required, i.e. anchor
#' types cannot be mixed.
#'
#' @export
#'
#' @examples
#'
#' ## Target cumulative AUC, always exactly at 72 hours:
#' create_target_design(
#'   targettype = "cum_auc",
#'   targetvalue = 90,
#'   time = 72
#' )
#'
#' ## Target trough concentration at trough after dose 4.
#' create_target_design(
#'   targettype = "cmin",
#'   targetvalue = 15,
#'   at = 4,
#'   anchor = "dose"
#' )
#'
#' ## Target AUC24 over day 4
#' create_target_design(
#'   targettype = "auc24",
#'   targetvalue = 500,
#'   at = 4,
#'   anchor = "day"
#' )
#'
#' ## Target cmin first at dose 4 (15 mg/L), then at dose 8 (25 mg/L)
#' create_target_design(
#'   targettype = "cmin",
#'   targetvalue = c(15, 25),
#'   at = c(4, 8),
#'   anchor = "day"
#' )

create_target_design <- function(
    targettype = mipd_target_types(),
    targetmin = NULL,
    targetmax = NULL,
    targetvalue = NULL,
    single_point_variation = 0.20,
    time = NULL,
    when = NULL,
    offset = NULL,
    at = NULL,
    anchor = c("dose", "day")
) {
  targettype <- match.arg(tolower(targettype), mipd_target_types())
  anchor <- match.arg(anchor)

  ## Infer `time` and `when` from targettype
  if(is.null(when)) {
    if(!is.null(time)) { # assume user wants to specify timepoint manually
      when <- NULL
    } else {
      offset <- 0
      time <- NULL
      switch(
        targettype,
        "cmin" =, "trough" = { when <- "cmin" },
        "cmax" =, "peak" = { when <- "cmax" },
        "auc24" = { offset <- 24; when <- "dose" },
        "auc12" = { offset <- 12; when <- "dose" },
        "conc" = { when <- "dose" },
        "cum_auc" = {when <- "dose"}
      )
    }
  }

  scheme <- create_design(
    time = time,
    when = when,
    offset = offset,
    at = at,
    anchor = anchor
  )

  ## Parse targets
  if ((is.null(targetmin) || is.null(targetmax)) && is.null(targetvalue)) {
    stop("Either targetmin + targetmax or midpoint must be supplied")
  }
  if (!is.null(targetmin) && !is.null(targetmax)) {
    if (!is_valid_numeric_vector(targetmin) || !is_valid_numeric_vector(targetmax) || length(targetmin) != length(targetmax)) {
      stop("targetmin or targetmax misspecified/not numeric, or not of same length")
    }
    midpoint <- apply(cbind(targetmin, targetmax), 1, mean)
    lowerbound <- targetmin
    upperbound <- targetmax
  }
  if (!is.null(targetvalue)) {
    if (!is_valid_numeric_vector(targetvalue)) {
      stop("targetvalue misspecified/not numeric")
    }
    if (!is_valid_numeric_vector(single_point_variation)) {
      stop("single_point_variation misspecified/not numeric")
    }
    midpoint <- targetvalue
    lowerbound <- (1 - single_point_variation) * targetvalue
    upperbound <- (1 + single_point_variation) * targetvalue
  }

  list(
    type = targettype,
    value = midpoint,
    min = lowerbound,
    max = upperbound,
    scheme = scheme
  )
}

#' Accepted PK/PD exposure targets
#'
#' Model-based dose-finding is currently implemented for the following target
#' types:
#' - peak / cmax: Peak concentration
#' - cmax_1hr: Peak concentration 1hr after dose
#' - trough, cmin: Trough concentration
#' - conc: generic concentration
#' - cum_auc: Cumulative AUC
#' - auc: auc over a dosing interval
#' - auc24: auc normalized to a 24-hour period
#' - auc12: auc normalized to a 12-hour period
#'
#'
#' @returns Returns a character vector of accepted target types.
#' @export

mipd_target_types <- function() {
  c(target_types_auc, target_types_conc, target_types_time)
}

target_types_auc <- c("cum_auc", "auc", "auc24", "auc12")
target_types_time <- c("t_gt_mic","t_gt_4mic","t_gt_mic_free","t_gt_4mic_free")
target_types_conc <- c("peak", "cmax", "cmax_1hr", "trough", "cmin", "conc")

#' Checks if a value (or vector of values) is within the specified target range
#'
#' @param v exposure metric, a single value or a vector
#' @param target target specification created with [create_target_design()], or
#'   a named list with `min` and `max` specified.
#' @export
#' @returns Returns a logical value of `TRUE` or `FALSE` for each value in v.

is_on_target <- function(v, target) {
  v >= target$min & v <= target$max
}


#' Create evaluation object
#'
#' This function defines the evaluation metric and timing for non-target metrics.
#' Use this function to record outputs from the simulation like troughs or AUC
#' that are not at the target times.
#'
#' @inheritParams create_target_design
#' @param evaltype evaluation metric(s) to use. Types from `mipd_target_types()`
#' @export
create_eval_design <- function(
    evaltype = mipd_target_types(),
    time = NULL,
    when = NULL,
    offset = NULL,
    at = NULL,
    anchor = c("dose", "day")
) {
  out <- list()
  anchor <- match.arg(anchor)
  original_when <- when

  for (type_eval in evaltype){
    ## Infer `time` and `when` from evaltype
    if(is.null(original_when)) {
      if(!is.null(time)) { # assume user wants to specify timepoint manually
        when <- NULL
      } else {
        offset <- 0
        time <- NULL
        switch(
          type_eval,
          "cmin" =, "trough" = { when <- "cmin" },
          "cmax" =, "peak" = { when <- "cmax" },
          "auc24" = { offset <- 24; when <- "dose" },
          "auc12" = { offset <- 12; when <- "dose" },
          "conc" = { when <- "dose" },
          "cum_auc" = {when <- "dose"},
          when <- "unknown"
        )
      }
    }

    tmp <- create_design(
      time = time,
      when = when,
      offset = offset,
      at = at,
      anchor = anchor
    )

    out[[type_eval]] <- tmp
  }

  out
}
