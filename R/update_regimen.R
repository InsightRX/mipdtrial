#' Update a regimen with a new dose
#'
#' @param regimen PKPDsim regimen object
#' @param new_dose value of new dose
#' @param dose_update_number integer indicating which dose and onwards should
#'   be updated
#' @returns Returns a PKPDsim regimen object with the new dose applied.
#' @export

update_regimen <- function(regimen, new_dose, dose_update_number) {
  regimen$dose_amts[dose_update_number:length(regimen$dose_amts)] <- new_dose
  regimen
}
