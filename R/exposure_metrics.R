#' Get AUC from a simulation
#'
#' @param sim_output output of a `PKPDsim::sim` call
#' @param auc_comp auc compartment
#' @returns numeric vector of AUCs between each simulated time point. Control
#'   time period over which AUC should be calculated using `target_time`
#'   argument to `PKPDsim::sim`.
#' @export
calc_auc_from_sim <- function(sim_output, auc_comp) {
  aucs <- sim_output$y[sim_output$comp == auc_comp]
  if (length(aucs) == 1) {
    aucs # i.e., cumulative
  } else {
    diff(aucs)
  }
}

#' Get concentration from a regimen
#'
#' Supply the final regimen and the final parameter estimates to get the final
#' estimated concentration. Supply the final regimen and the true individual
#' parameter estimates to get the final true concentration.
#'
#' @param regimen PKPDsim regimen object
#' @param parameters use MAP estimation to get estimated concentration, use true
#'   patient parameters to get true concentration. Parameters must correspond to
#'   the model used. Accepts parameters supplied as a data frame row, a named
#'   vector or as a list.
#' @param model model to use for concentration calculations.
#' @param target_design target design, created using `create_target_design()`
#' @param ... arguments passed on to PKPDsim::sim. Typical arguments include
#'   `covariates` or `iov_bins`
#' @returns numeric vector of concentrations between each simulated time point.
#'   Control when concentration is estimated using `target_time`.
#' @export
calc_concentration_from_regimen <- function(
    regimen,
    parameters,
    model,
    target_design,
    ...
){
  if (!all(attr(model, "parameters") %in% names(parameters))) {
    stop("Model/parameter mismatch")
  }
  if (inherits(parameters, "data.frame") || is.atomic(parameters)) {
    parameters <- as.list(parameters)
  }

  iov <- PKPDsim::get_model_iov(model)
  if (is.null(iov[["bins"]])) iov[["bins"]] <- c(0, 9999)

  target_time <- get_sampling_times_from_scheme(
    target_design$scheme,
    regimen
  )
  sim_output <- PKPDsim::sim(
    model,
    parameters = parameters,
    regimen = regimen,
    t_obs = target_time,
    iov_bins = iov[["bins"]],
    ...
  )
  sim_output[sim_output$comp == "obs",]$y
}

#' Get AUC from a regimen
#'
#' Supply the final regimen and the final parameter estimates to get the final
#' estimated AUC. Supply the final regimen and the true individual parameter
#' estimates to get the final true AUC.
#'
#' @param regimen PKPDsim regimen object
#' @param parameters use MAP estimation to get estimated AUC, use true patient
#'   parameters to get true AUC. Parameters must correspond to the model used.
#'   Accepts parameters supplied as a data frame row, a named vector or as a
#'   list.
#' @param model model to use for AUC calculations.
#' @param target_design target design, created using `create_target_design()`
#' @param ... arguments passed on to PKPDsim::sim. Typical arguments include
#'   `covariates` or `iov_bins`
#' @returns numeric vector of AUCs between each simulated time point. Control
#'   time period over which AUC should be calculated using `target_time`.
#' @export

calc_auc_from_regimen <- function(
  regimen,
  parameters,
  model,
  target_design,
  ...
){
  if (!all(attr(model, "parameters") %in% names(parameters))) {
    stop("Model/parameter mismatch")
  }
  if (inherits(parameters, "data.frame") || is.atomic(parameters)) {
    parameters <- as.list(parameters)
  }

  iov <- PKPDsim::get_model_iov(model)
  if (is.null(iov[["bins"]])) iov[["bins"]] <- c(0, 9999)

  target_time <- get_sampling_times_from_scheme(
    target_design$scheme,
    regimen
  )
  if(target_design$type == "auc24") {
    target_time <- c(target_time - 24, target_time)
  }
  sim_output <- PKPDsim::sim(
    model,
    parameters = parameters,
    regimen = regimen,
    t_obs = target_time,
    iov_bins = iov[["bins"]],
    ...
  )
  calc_auc_from_sim(sim_output, attr(model, "size"))
}
