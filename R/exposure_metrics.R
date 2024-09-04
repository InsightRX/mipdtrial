#' Calculate exposure metrics
#'
#' Calculates drug concentration or area under the curve (AUC) for a MIPD
#' trial run.
#'
#' This family of functions calculates the exposure metric for a given regimen,
#' model, model parameters, and target design. Supply the final regimen and the
#' final parameter estimates to get the final estimated exposure metric. Supply
#' the final regimen and the true individual parameter estimates to get the
#' final true exposure metric.
#'
#' @name exposure_metrics
NULL

#' Get AUC from a simulation
#'
#' @param sim_output output of a `PKPDsim::sim` call
#' @param auc_comp auc compartment
#' @returns `calc_auc_from_sim` returns a numeric vector of AUCs between each
#'   simulated time point. Control time period over which AUC should be
#'   calculated using `target_time` argument to `PKPDsim::sim`.
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
#' @param regimen PKPDsim regimen object
#' @param parameters use MAP estimation to get estimated exposure, use true
#'   patient parameters to get true exposure. Parameters must correspond to the
#'   model used. Accepts parameters supplied as a data frame row, a named vector
#'   or as a list.
#' @param model model to use for exposure calculations.
#' @param target_design target design, created using `create_target_design()`
#' @param ... arguments passed on to PKPDsim::sim. Typical arguments include
#'   `covariates` or `iov_bins`
#' @returns `calc_concentration_from_regimen` returns a numeric vector of
#' concentrations between each simulated time point. Control when concentration
#' is estimated using `target_time`.
#' @rdname exposure_metrics
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
#' @returns `calc_auc_from_regimen` returns a numeric vector of AUCs between
#'   each simulated time point. Control time period over which AUC should be
#'   calculated using `target_time`.
#'
#' @rdname exposure_metrics
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
