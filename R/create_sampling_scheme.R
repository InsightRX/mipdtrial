#' Sampling times can usually be pre-specified if the dosing schedule is fixed.
#' In that case a scheme can be prespecified with static timepoints e.g.
#' `c(1, 7.5, 25, 31.5)` for taking peak-trough samples at dose 1 and 4 for an
#' 8-hour regimen.
#'
#' However, when the dosing interval is adaptive, the peak and trough sampling
#' times cannot stay static and have to be adapted on-the-fly during
#' simulations.
#'
#' @param time a vector of either all numeric values or all character values. If
#' numeric values are specified, these will be used as the fixed sampling times
#' in the simulated trial. If character values are supplied, the sampling times
#' will be calculated adaptively during the trial, if needed. The "times" should
#' then be of the form "<anchor><+/-><offset>", e.g. "trough-0.5" or "peak+0.5",
#' meaning the trough for the referenced dose minus half an hour, or the peak for the
#' referenced dose plus half an hour. The `anchors` determine which dose is
#' used as reference anchor.
#' @param anchors numeric vector of the dose number of day number to "anchor"
#' the TDMs to. Vector needs to be of same length as `t`. If `anchor_by` is set
#' to `day`, then the first dose in that day is used. If later doses in the day
#' are preferred, the anchor can also be specified fractionally, e.g. `1.5`
#' will use the time of the first dose in the second half of the 1st day.
#' @param anchor_by either "day" or "dose". Single value required, i.e. anchor
#' types cannot be mixed.
#'
#' @export
#'
create_sampling_scheme <- function(
    time = NULL,
    anchors = NULL,
    anchor_by = c("day", "dose")
) {
  anchor_by <- match.arg(anchor_by)
  if(inherits(time, "character")) {
    if(is.null(anchors)) {
      stop("Please specify `anchors` when supplying `time` as adaptive values.")
    }
    if(length(anchors) != length(time)) {
      stop("Please specify `anchors` with same length as `time`")
    }
    allowed_times <- c("dose", "trough", "peak", "cmax", "cmin")
    t_clean <- unlist(
      lapply(stringr::str_split(time, "[+-]"), function(x) x[1])
    )
    if(any(! unique(t_clean) %in% c(allowed_times))) {
      stop(
        "Please specify only any of: ", paste(allowed_times, collapse = ", "),
        " when using adaptive times."
      )
    }
    offsets <- as.numeric(stringr::str_extract_all(time, "[+-].*$"))
    if(any(is.na(offsets))) offsets[is.na(offsets)] <- 0
    scheme <- data.frame(
      time = t_clean,
      offset_from_dose = offsets,
      anchor = anchors,
      anchor_by = anchor_by
    )
  } else {
    if(! all(is.numeric(time))) {
      stop("When not anchoring TDM times to dose or day, `t` must be all numeric.")
    }
    scheme <- data.frame(
      time = time,
      offset_from_dose = NA,
      anchor = NA,
      anchor_by = "time"
    )
  }
  scheme
}
