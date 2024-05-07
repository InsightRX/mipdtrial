#' Simulate TDM collection
#'
#' Using the "ground truth" model, simulate collection of drug/biomarker levels.
#' Returns a data frame of sample time, true value, and measured value (with
#' residual error added).
#'
#' @param sim_model model used for simulated patient response ("truth").
#' @param t_obs sample collection times (since start of treatment)
#' @param res_var data frame of residual variability quantities to add
#' @param regimen PKPDsim regimen object
#' @param pars_i true parameters for the individual (named list)
#' @param lloq lower limit of quantification. If non-NULL, all TDMs below LLOQ
#'   will be set to half the LLOQ.
#' @param ... arguments passed on to PKPDsim::sim
#' @returns a data frame with columns `t` (time), `true_tdm` (actual level) and
#'   `y` (measured level), with rows corresponding to t_obs.
