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
  diff(aucs)
}
