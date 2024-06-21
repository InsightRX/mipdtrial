#' Create scheme for updating dose or interval during dose optimization trial
#'
#' @inheritParams create_sampling_scheme
#' @param update_type update either future `dose` (default) or `interval`.
#'
#' @export
create_regimen_update_scheme <- function(
    time = NULL,
    anchor,
    anchor_by,
    update_type = c("dose", "interval")
) {
  if(is.null(time)) time <- rep(0, length(anchor))
  update_type <- match.arg(update_type)
  scheme <- create_sampling_scheme(
    time = time,
    offset_base = rep("dose", length(anchor)),
    anchor = anchor,
    anchor_by = anchor_by
  )
  scheme$update_type <- update_type
  scheme
}
