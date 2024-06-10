#' Get AUC from a simulation
#'
#' @param sim_output output of a `PKPDsim::sim` call
#' @param auc_comp auc compartment
#' @returns numeric vector of AUCs between each simulated time point. Control
#'   time period over which AUC should be calculated using `t_obs` argument to
#'   `PKPDsim::sim`.
#' @export

calc_auc_from_sim <- function(sim_output, auc_comp) {
  aucs <- sim_output$y[sim_output$comp == auc_comp]
  if (length(aucs) == 1) {
    aucs # i.e., cumulative
  } else {
    diff(aucs)
  }
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
#' @param t_obs timepoint(s) at which to calculate AUC
#' @param ... arguments passed on to PKPDsim::sim. Typical arguments include
#'   `covariates` or `iov_bins`
#' @returns numeric vector of AUCs between each simulated time point. Control
#'   time period over which AUC should be calculated using `t_obs`.
#' @export

calc_auc_from_regimen <- function(regimen, parameters, model, t_obs, ...) {
  if (!all(attr(model, "parameters") %in% names(parameters))) {
    stop("Model/parameter mismatch")
  }
  if (inherits(parameters, "data.frame") || is.atomic(parameters)) {
    parameters <- as.list(parameters)
  }
  sim_output <- PKPDsim::sim(
    model,
    parameters = parameters,
    regimen = regimen,
    t_obs = t_obs,
    ...
  )
  calc_auc_from_sim(sim_output, attr(model, "size"))
}
