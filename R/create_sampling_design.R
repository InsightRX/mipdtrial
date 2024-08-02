#' Function for creating sampling designs.
#'
#' Sampling times can usually be pre-specified if the dosing schedule is fixed.
#' In that case a scheme can be specified with static timepoints e.g.
#' `time = c(1, 7.5, 25, 31.5)` for taking peak-trough samples at dose
#' 1 and 4 for an 8-hour regimen.
#' However, when the dosing interval is adaptive, the peak and trough sampling
#' times cannot stay static and have to be adapted on-the-fly during
#' simulations. In that case, it is possible to use `when` to specify
#' sampling at `peak`, `trough` or `dose` concentrations. In that case, the
#' `time` is not allowed. If not exact peak and trough concentrations should be
#' sample, use the `offset` parameter to sample e.g. an hour post-peak, or
#' shortly before true troughs.
#'
#' @param lloq lower limit of quantification for TDMs
#' @inheritParams create_design
#'
#' @examples
#' # to sample at 1.5/8.5/23.5 hours
#' create_sampling_design(time = c(1.5, 11.5, 23.5))
#'
#' # to sample at the peak (end of infusion) and trough (start of
#' # next dose) of each dose
#' create_sampling_design(
#'   when = c("peak", "trough")
#' )
#'
#' # to sample 30 min post-peak and 30-min pre-trough for the first dose
#' create_sampling_design(
#'   when = c("peak", "trough"),
#'   offset = c(0.5, -0.5),
#'   at = c(1, 1)
#' )
#'
#' # The same as above, but now for the first and third dose
#' create_sampling_design(
#'   when = c("peak", "trough", "peak", "trough"),
#'   offset = c(0.5, -0.5, 0.5, -0.5),
#'   at = c(1, 1, 3, 3),
#'   anchor = "dose"
#' )
#'
#' @export
create_sampling_design <- function(
    time = NULL,
    when = NULL,
    offset = NULL,
    scatter = NULL,
    at = NULL,
    lloq = 0,
    anchor = c("dose", "day")
) {
  if(is.null(time)) {
    if(is.null(when)) when <- "dose"
  }
  scheme <- create_design(
    time = time,
    when = when,
    offset = offset,
    scatter = scatter,
    at = at,
    anchor = anchor
  )
  list(
    lloq = lloq,
    scheme = scheme
  )
}
