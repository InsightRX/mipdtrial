#' Creates a design for the initial regimen for patients in the trial
#'
#' @param method a function to call. The function should always return a
#' PKPDsim regimen object.
#'
#' @returns a list object with the design specs
#'
create_initial_regimen_design <- function(
  method,
  ...
) {
  design <- list(
    method = method,
    ...
  )
  design
}
