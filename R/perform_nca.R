#' Perform non-compartmental analysis (NCA)
#'
#' A light wrapper around `clinPK::nca`. Given a set of TDMs, performs
#' non-compartmental analysis. NCA only considers TDMs from a single dosing
#' interval. This function identifies the dose corresponding to the final TDM(s)
#' and uses all TDMs following that dose.
#'
#' @param tdms a data frame of TDMs, containing at least a column `t` for time
#'   and `y` for observed measurement to be used in estimation. Will filter for
#'   the samples from the final dosing interval. Baseline estimates must be
#'   collected within 30 minutes of the time of the start of dose administration
#'   in order to be considered baseline values.
#' @param regimen A PKPDsim regimen object
#' @param ... arguments passed on to `clinPK::nca`.
#' @export
#' @returns Returns an object of class "nca_output", defined in `clinPK`. This
#'   object is a named list, with components for `pk` (containing PK
#'   parameters), `descriptive` (containing exposure estimates) and `settings`
#'   (containing input parameters to the NCA calculations). See `help("nca")`
#'   for more information.
#'
perform_nca <- function(
  tdms,
  regimen,
  ...
) {
  # identify dose/TDMs
  last_tdm_time <- max(tdms$t)
  dose_idx <- max(which(regimen$dose_times < last_tdm_time))
  if (dose_idx == length(regimen$dose_times)) {
    tau <- regimen$interval
  } else {
    tau <- diff(regimen$dose_times[dose_idx:(dose_idx+1)])
  }

  dose_time <- regimen$dose_times[dose_idx]
  tdms$t <- tdms$t - dose_time
  tdms <- tdms[tdms$t >= 0,]

  colnames(tdms)[colnames(tdms) == "t"] <- "time"
  colnames(tdms)[colnames(tdms) == "y"] <- "dv"

  clinPK::nca(
    data = tdms,
    dose = regimen$dose_amts[dose_idx],
    t_inf = regimen$t_inf[dose_idx],
    tau = tau,
    has_baseline = (round(tdms$t[1]) == 0),
    ...
  )
}
