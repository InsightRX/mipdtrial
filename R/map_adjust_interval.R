#' Adjust intervals to achieve a target metric using MAP Bayesian estimation by
#' adapting the dosing interval
#'
#' Given a set of levels and a model definition, performs MAP Bayesian
#' estimation of individual PK/PD parameters, then finds the appropriate dosing
#' interval to achieve the specified PK/PD target and updates the
#' individual's regimen accordingly.
#'
#' @inheritParams map_adjust_dose
#' 
#' @returns Returns a named list: `regimen`: the updated regimen;
#'   `additional_info`: the MAP parameter estimates
#' @export
map_adjust_interval <- function(
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
    settings = NULL,
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
  if("error" %in% class(fit)) {
    cli::cli_abort("Fit to simulated data failed, please check model and simulation designs.")
  }
  est_par <- fit$parameters
  gof <- data.frame(pred = fit$pred, ipred = fit$ipred, dv = fit$dv, weights = fit$weights)

  # calculate new dose, using the estimation model
  if (is.null(grid)) {
    cli::cli_abort("Interval-optimization requires `grid` argument.")
  }
  new_interval <- dose_grid_search(
    est_model = est_model,
    regimen = regimen,
    parameters = est_par, # we want to use our "best guess" to get the dose
    target_design = target_design,
    auc_comp = PKPDsim::get_model_auc_compartment(est_model),
    dose_update = dose_update,
    grid = grid,
    grid_type = "interval",
    covariates = covariates,
    iov_bins = PKPDsim::get_model_iov(est_model)$bins,
    verbose = TRUE,
    ...
  )

  # return new regimen
  regimen <- update_regimen(
    regimen,
    new_interval = new_interval,
    dose_update_number = dose_update
  )
  list(
    regimen = regimen,
    dose_update = dose_update,
    new_dose = NA,
    new_interval = new_interval,
    additional_info = est_par,
    gof = gof
  )
}
