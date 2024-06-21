#' Get dose number to update dose/interval at from the regime update scheme
#' and a provided regimen.
#'
#' @param scheme regimen update schema created using `create_regimen_update_schema()`
#' @param regimen a `PKPDsim` regimen object
#'
#' @returns vector of dose numbers to update at
#'
#' @export
get_dose_update_numbers_from_scheme <- function(scheme, regimen) {
  dose <- c()
  for(i in 1:length(scheme$anchor)) {
    dose <- c(dose, get_dose_update_core(scheme[i,], regimen))
  }
  sort(dose)
}

#' Core function to calculate the dose update number for a row in a
#' regimen update data.frame
#'
get_dose_update_core <- function(row, regimen) {
  if(row$anchor_by == "dose") {
    dose_anchor <- row$anchor
  } else { ## find dose closest to specified day
    t_aim <- (row$anchor-1) * 24
    dose_anchor <- which.min(abs(regimen$dose_times - t_aim))
  }
  dose_anchor
}
