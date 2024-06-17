#' Update a regimen with a new dose
#'
#' @param regimen PKPDsim regimen object
#' @param new_dose value of new dose
#' @param new_interval value of new interval
#' @param dose_update_number integer indicating which dose and onwards should
#'   be updated
#' @returns Returns a PKPDsim regimen object with the new dose applied.
#' @export

update_regimen <- function(
    regimen,
    new_dose = NULL,
    new_interval = NULL,
    dose_update_number
  ) {
  if(!is.null(new_dose)) {
    regimen$dose_amts[dose_update_number:length(regimen$dose_amts)] <- new_dose
  }
  if(!is.null(new_interval)) {
    n_doses <- length(dose_update_number:length(regimen$dose_amts))
    regimen$dose_times[dose_update_number:length(regimen$dose_amts)] <-
      regimen$dose_times[dose_update_number] + (seq(1:n_doses)-1) * new_interval
  }
  regimen
}
