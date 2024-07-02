#' Get dose number to update dose/interval at from the regime update scheme
#' and a provided regimen.
#'
#' @param design regimen update design created using
#' `create_regimen_update_design()`
#' @param regimen a `PKPDsim` regimen object
#'
#' @returns vector of dose numbers to update at
#'
#' @export
get_dose_update_numbers_from_design <- function(design, regimen) {
  adjust_at_dose <- c()
  for(i in 1:length(design$scheme$anchor)) {
    adjust_at_dose <- c(adjust_at_dose, get_dose_update_core(design$scheme[i,], regimen))
  }
  adjust_dose_checks(adjust_at_dose, regimen)
  sort(adjust_at_dose)
}

#' Core function to calculate the dose update number for a row in a
#' regimen update data.frame
#'
#' @param row a single row from a regimen_update data.frame
#' @param regimen PKPDsim regimen
#'
get_dose_update_core <- function(row, regimen) {
  if(row$anchor == "dose") {
    dose_anchor <- row$anchor
  } else { ## find dose closest to specified day
    t_aim <- (row$anchor-1) * 24
    dose_anchor <- which.min(abs(regimen$dose_times - t_aim))
  }
  dose_anchor
}

#' Checks for dose_update_number obtained from dose_update scheme
#'
#' @param adjust_at_dose number of dose at which to adjust
#' @param regimen PKPDsim regimen
adjust_dose_checks <- function(adjust_at_dose, regimen) {
  if (max(adjust_at_dose) > length(regimen$dose_times)) {
    stop("Insufficient doses in `regimen` for all dose adjustments specified.")
  }
  if (any(adjust_at_dose <= 1)) {
    stop("TDM collection before the first dose is not yet supported")
  }
}
