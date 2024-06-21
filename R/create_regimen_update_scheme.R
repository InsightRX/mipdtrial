#' Create scheme for updating dose or interval during dose optimization trial
#'
#' @inheritParams create_sampling_scheme
#'
#' @export
create_regimen_update_scheme <- function(
    time = NULL,
    anchor,
    anchor_by
) {
  if(is.null(time)) time <- rep(0, length(anchor))
  create_sampling_scheme(
    time = time,
    offset_base = rep("dose", length(anchor)),
    anchor = anchor,
    anchor_by = anchor_by
  )
}
