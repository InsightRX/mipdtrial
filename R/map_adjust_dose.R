#' Adjust doses to achieve a target metric using MAP Bayesian estimation.
#'
#' Given a set of levels and a model definition, performs MAP Bayesian
#' estimation of individual PK/PD parameters, then finds the appropriate dose
#' to achieve the specified PK/PD target and updates the individual's regimen
#' accordingly.
#'
#' @inheritParams simulate_fit
#' @inheritParams dose_grid_search
#' @param ... arguments passed on to PKPDmap::get_map_estimates and/or
#'   PKPDsim::sim
#' @returns Returns a named list: `regimen`: the updated regimen;
#'   `additional_info`: the MAP parameter estimates
#' @export
#'
map_adjust_dose <- function(
    tdms,
    est_model,
    parameters,
    omega,
    ruv,
    regimen,
    covariates = NULL,
    target_design,
    dose_update,
    grid = NULL,
    ...
) {
  # get MAP fit, using model for estimation
  fit <- simulate_fit(
    est_model = est_model,
    parameters = parameters,
    omega = omega,
    ruv = ruv,
    tdms = tdms,
    covariates = covariates,
    regimen = regimen,
    ...
  )
  est_par <- parameters
  gof <- data.frame(pred = fit$pred, ipred = fit$ipred, dv = fit$dv, weights = fit$weights)

  # calculate new dose, using the estimation model
  if (is.null(grid)) {
    # base dose finding grid on initial regimen
    d1 <- regimen$dose_amts[1]
    grid <- seq(d1/5, d1 * 5, length.out = 10)
  }
  new_dose <- dose_grid_search(
    est_model = est_model,
    regimen = regimen,
    parameters = est_par, # we want to use our "best guess" to get the dose
    target_design = target_design,
    auc_comp = PKPDsim::get_model_auc_compartment(est_model),
    dose_update = dose_update,
    grid = grid,
    grid_type = "dose",
    covariates = covariates,
    iov_bins = PKPDsim::get_model_iov(est_model)$bins
  )
  # return new regimen
  regimen <- update_regimen(
    regimen,
    new_dose = new_dose,
    dose_update_number = dose_update
  )
  list(
    regimen = regimen,
    dose_update = dose_update,
    new_dose = new_dose,
    new_interval = NA,
    additional_info = est_par,
    gof = gof
  )
}
