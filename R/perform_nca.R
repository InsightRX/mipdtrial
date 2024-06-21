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
  tau <- tau_from_regimen(regimen, dose_idx)

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

#' Calculate a new dose given an estimated AUC and target AUC
#'
#' Uses a simple ratio of `dose2 = dose1 * auc2 / auc1` (i.e., assumes
#' constant clearance) to identify a new dose. For cumulative AUC targets, takes
#' into consideration previous AUC and number of doses remaining.
#'
#' @param target object describing exposure targets, see `create_target_object`.
#' @param intv_auc AUC estimated for a sampled dosing interval
#' @param regimen PKPDsim regimen object
#' @param dose_update index of dose that should be updated
#' @param cum_auc AUC estimated for all dosing intervals up to the dose_update
#'   interval.

dose_from_nca_auc <- function(
  target,
  intv_auc,
  regimen,
  dose_update,
  cum_auc = 0
) {

  if (!target$type %in% c("auc24", "auc", "cum_auc")) {
    stop(paste0("target type ", target$type, " not yet supported!"))
  }
  dose <- regimen$dose_amts[dose_update]
  intv <- tau_from_regimen(regimen, dose_update)

  if (target$type == "auc") {
    return(dose * target$value / intv_auc)
  }
  if (target$type == "auc24") {
    return(dose * target$value / (intv_auc * 24/intv))
  }

  remaining_doses <- length(regimen$dose_amts) + 1 - dose_update
  remaining_auc <- target$value - cum_auc

  pmax(0, (dose/intv_auc) * (remaining_auc/remaining_doses))
}

#' Get dosing interval (tau) from PKPDsim regimen object
#'
#' Uses time of next dose for all but the last interval, which takes from the
#' PKPDsim regimen interval specification.
#'
#' @inheritParams perform_nca
#' @param dose_n index of dose for which tau is requested
#' @returns a single numeric value

tau_from_regimen <- function(regimen, dose_n) {
  if (dose_n == length(regimen$dose_times)) {
    regimen$interval
  } else {
    diff(regimen$dose_times[dose_n:(dose_n+1)])
  }
}
