#' Create scheme for updating dose or interval during dose optimization trial
#'
#' @inheritParams create_sampling_design
#' @param update_type update either future `dose` (default) or `interval`.
#' @param dose_optimization_method A function that determines how doses should
#'   be adjusted given the collected drug levels. The function must return a
#'   named list of the structure `list(regimen = reg, additional_info = x)`,
#'   where `reg` is the updated PKPDsim regimen for the patient and `x` can be
#'   another other information useful for post-processing of trial results.
#'
#' @export
create_regimen_update_design <- function(
    time = NULL,
    anchor,
    anchor_by,
    update_type = c("dose", "interval"),
    dose_optimization_method = map_adjust_dose
) {
  if(is.null(time)) time <- rep(0, length(anchor))
  update_type <- match.arg(update_type)
  scheme <- create_design(
    time = time,
    offset_from = rep("dose", length(anchor)),
    anchor = anchor,
    anchor_by = anchor_by
  )
  scheme$update_type <- update_type
  list(
    dose_optimization_method = dose_optimization_method,
    scheme = scheme
  )
}
