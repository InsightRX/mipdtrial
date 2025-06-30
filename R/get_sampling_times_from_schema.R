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
#' @param row a single row from a sampling_time_design data.frame
#' @param regimen PKPDsim regimen
get_sampling_time_core <- function(row, regimen) {
  if(row$anchor == "dose") {
    dose_anchor <- row$at
  } else { ## find dose closest to specified day
    t_aim <- (row$at - 1) * 24
    dose_anchor <- which.min(abs(regimen$dose_times - t_aim))
  }
  if(row$base %in% c("peak", "cmax")) {
    t_anchor <- regimen$dose_times[dose_anchor] + regimen$t_inf[dose_anchor]
  } else if(row$base %in% c("middle", "cmid")) {
    t_anchor <- round(mean(c(
      regimen$dose_times[dose_anchor] + regimen$t_inf[dose_anchor],
      regimen$dose_times[dose_anchor + 1]
    )), 2)
  } else if(row$base %in% c("random")) {
    t_anchor <- round(runif(
      1,
      regimen$dose_times[dose_anchor] + regimen$t_inf[dose_anchor],
      regimen$dose_times[dose_anchor + 1]
    ), 2)
  } else if(row$base %in% c("trough", "cmin")) {
    t_anchor <- regimen$dose_times[dose_anchor + 1]
  } else { # dose
    t_anchor <- regimen$dose_times[dose_anchor]
  }
  if(is.na(t_anchor)) {
    msg <- "Not enough doses in regimen to update, please increase initial regimen length."
    if(row$base %in% c("trough", "cmin")) {
      msg <- c(msg, " For `cmin`, `middle`, or `random` calculation, you will need one more dose in the regimen than the dose for which the `cmin` is calculated.")
    }
    cli::cli_abort(msg)
  }
  scatter <- 0
  if(!is.null(row$scatter)) {
    scatter <- round(rnorm(1, 0, row$scatter), 2)
  }
  t_anchor + row$offset + scatter
}
