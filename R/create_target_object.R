#' Create target object
#'
#' This function helps the user define the PKPD target for a simulation. When a
#' minimum value and maximum value are supplied, the algorithm targets the mid-
#' point. Alternatively, a single midpoint can be supplied.
#'
#' @param targettype target type, one of accepted types (see
#'   [mipd_target_types()])
#' @param targetmin minimum value acceptable, must be specified with `targetmax`
#' @param targetmax maximum value acceptable, must be specified with `targetmin`
#' @param targetvalue single value for a target, overrides min and max values.
#' @param single_point_variation acceptable variation from targetvalue. By
#'   default 20%. Considered for assessment of target attainment a posteriori,
#'   not used for dose-finding logic.
#' @export
#' @examples
#' create_target_object(targettype = "cum_auc", targetvalue = 90)
#'

create_target_object <- function(
 targettype = mipd_target_types(),
 targetmin = NULL,
 targetmax = NULL,
 targetvalue = NULL,
 single_point_variation = 0.20
) {
  targettype <- match.arg(tolower(targettype), mipd_target_types())
  if ((is.null(targetmin) || is.null(targetmax)) && is.null(targetvalue)) {
    stop("Either targetmin + targetmax or midpoint must be supplied")
  }
  if (!is.null(targetmin) && !is.null(targetmax)) {
    if (!is_single_valid_number(targetmin) || !is_single_valid_number(targetmax)) {
      stop("targetmin or targetmax misspecified/not numeric")
    }
    midpoint <- mean(c(targetmin, targetmax))
    lowerbound <- targetmin
    upperbound <- targetmax
  }
  if (!is.null(targetvalue)) {
    if (!is_single_valid_number(targetvalue)) {
      stop("targetvalue misspecified/not numeric")
    }
    if (!is_single_valid_number(single_point_variation)) {
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
    max = upperbound
  )
}

#' Accepted PK/PD exposure targets
#'
#' Model-based dose-finding is currently implemented for the following target
#' types:
#' - cmax: Peak concentration
#' - cmax_1hr: Peak concentration 1hr after dose
#' - ctrough, cmin: Trough concentration
#' - conc: generic concentration
#' - cum_auc: Cumulative AUC
#' - auc: auc over a dosing interval
#' - auc24: auc normalized to a 24-hour period
#'
#'
#' @returns Returns a character vector of accepted target types.
#' @export

mipd_target_types <- function() {
  c(target_types_auc, target_types_conc, target_types_time)
}

target_types_auc <- c("cum_auc", "auc", "auc24")
target_types_time <- c("t_gt_mic","t_gt_4mic","t_gt_mic_free","t_gt_4mic_free")
target_types_conc <- c("cmax", "cmax_1hr", "ctrough", "cmin", "conc")

#' Checks if a value (or vector of values) is within the specified target range
#'
#' @param v exposure metric, a single value or a vector
#' @param target target specification created with [create_target_object()], or
#'   a named list with `min` and `max` specified.
#' @export
#' @returns Returns a logical value of `TRUE` or `FALSE` for each value in v.

is_on_target <- function(v, target) {
  v >= target$min & v <= target$max
}
