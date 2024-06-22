#' Calculate sampling times based on a given sampling schema and a regimen.
#'
#' @param scheme sampling schema created using `create_tdm_schema()`
#' @param regimen a `PKPDsim` regimen object
#'
#' @returns vector of numeric sampling times
#' @export
get_sampling_times_from_scheme <- function(
    scheme,
    regimen
) {
  t <- c()
  for(i in 1:length(scheme$offset)) {
    t <- c(t, get_sampling_time_core(scheme[i,], regimen))
  }
  return(t)
}

#' Core function to calculate the sampling time for a row in a sampling schema
#' data.frame.
#'
get_sampling_time_core <- function(row, regimen) {
  if(row$anchor_by == "dose") {
    dose_anchor <- row$anchor
  } else { ## find dose closest to specified day
    t_aim <- (row$anchor-1) * 24
    dose_anchor <- which.min(abs(regimen$dose_times - t_aim))
  }
  if(row$base %in% c("peak", "cmax")) {
    t_anchor <- regimen$dose_times[dose_anchor] + regimen$t_inf[dose_anchor]
  } else if(row$base %in% c("trough", "cmin")) {
    t_anchor <- regimen$dose_times[dose_anchor + 1]
  } else { # dose
    t_anchor <- regimen$dose_times[dose_anchor]
  }
  if(is.na(t_anchor)) {
    stop("Not enough doses in regimen to update, please increase initial regimen length.")
  }
  t_anchor + row$offset
}
