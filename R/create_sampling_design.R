#' Function for creating sampling designs.
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
#' @inheritParams create_design
#'
#' @export
create_sampling_design <- function(
    time,
    offset_from = NULL,
    anchor = NULL,
    anchor_by = c("day", "dose")
) {
  create_design(
    time = time,
    offset_from = offset_from,
    anchor = anchor,
    anchor_by = anchor_by
  )
}
