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
#' @export
#' @examples
#' create_target_object(targettype = "cum_auc", targetvalue = 90)
#'

create_target_object <- function(
 targettype = mipd_target_types(),
 targetmin = NULL,
 targetmax = NULL,
 targetvalue = NULL
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
  }
  if (!is.null(targetvalue)) {
    if (!is_single_valid_number(targetvalue)) {
      stop("targetvalue misspecified/not numeric")
    }
    midpoint <- targetvalue
  }

  list(
    type = targettype,
    value = midpoint
  )
}

#' Accepted PK/PD exposure targets
#'
#' Model-based dose-finding is currently implemented for the following target
#' types:
#' - cmax: Peak concentration
#' - cmax_1hr: Peak concentration 1hr after dose
#' - ctrough, cmin: Trough concentration
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
target_types_conc <- c("cmax", "cmax_1hr", "ctrough", "cmin")


