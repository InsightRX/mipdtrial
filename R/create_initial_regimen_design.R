#' Creates a design for the initial regimen for patients in the trial
#'
#' @param method a function to call. The function should always return a
#' PKPDsim regimen object.
#' @param ... options passed on depending on `method`. Commonly includes
#' `regimen`, a named list of arguments passed on to PKPDsim::new_regimen
#' (see examples)
#'
#' @returns a list object with the design specs
#' @examples
#' # create_initial_regimen_design(
#'   method = weight_based_starting_dose,
#'   mgkg = 15,
#'   dosing_weight = "total_body_weight",
#'   regimen = list(
#'     interval = 12,
#'     n = 10,
#'     t_inf = 1,
#'     type = "infusion"
#'   )
#' )
#'
#' @export
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
