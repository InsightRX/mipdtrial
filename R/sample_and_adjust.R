#' Adjust dosing using MIPD on TDMs at specified dose numbers
#'
#' Doses are iteratively adjusted for the specified dose numbers. Estimation is
#' performed by MAP Bayesian estimation, and simulation is used to identify the
#' dose most likely to reach the specified target. See `dose_grid_search` for
#' dose-finding logic. Optionally, disparate models can be used for simulation
#' and estimation, to allow for intentional model misspecification. Covariates
#' supplied must support both models. True patient parameters (`pars_true_i`)
#' must match the model used for simulation.
#'
#' Trial design or MIPD protocol can vary considerably in complexity. This
#' function is intended to cover a broad range of relatively straight forward
#' designs, and may serve as a template for more complex designs.
#'
#' @param adjust_at_dose vector of integers indicating which doses to adjust
#' @param tdm_times vector of times to sample at, in hours starting from the
#'   time of first dose.
#' @param regimen PKPDsim regimen object, containing initial dosing regimen.
#' @param covariates named list of PKPDsim covariates.
#' @param pars_true_i PK parameters for the individual. See `generate_iiv`.
#' @param sim_model model to use for simulating "true" patient response.
#' @param sim_ruv residual variability for `sim_model`. Named list for
#'   proportional (`prop`) and additive (`add`) error.
#' @param ... arguments passed on to `simulate_fit` or dose_optimization_method
#'   function.
#' @returns a named list containing `final_regimen` (all doses after
#' adjustment), `final_estimates` (MAP Bayesian estimation made with all
#' available levels) and `tdms` (all collected levels, both true and measured,
#' that is, both with and without residual variability).
#' @export

sample_and_adjust_by_dose <- function(
  adjust_at_dose,
  tdm_times,
  regimen,
  covariates = NULL,
  pars_true_i,
  sim_model,
  sim_ruv = NULL,
  dose_optimization_method = dose_adjust_map,
  ...
) {

  if (inherits(pars_true_i, "data.frame")) pars_true_i <- as.list(pars_true_i)

  # base dose finding grid on initial regimen
  if (is.null(dose_grid)) {
    d1 <- regimen$dose_amts[1]
    dose_grid <- seq(d1/5, d1 * 5, length.out = 10)
  }

  if (max(adjust_at_dose) > length(regimen$dose_times)) {
    stop("Insufficient doses in `regimen` for all dose adjustments specified.")
  }

  adjust_at_dose <- sort(adjust_at_dose)
  if (any(adjust_at_dose <= 1)) {
    stop("TDM collection before the first dose is not yet supported")
  }
  first_dose_time <- regimen$dose_times[adjust_at_dose[1]]
  if (!any(tdm_times < first_dose_time)) {
    stop("At least one TDM must be collected before dose adjustment")
  }

  # randomly draw error terms
  ruv_i <- generate_ruv(
    tdm_sample_time = tdm_times,
    prop = sim_ruv$prop,
    add = sim_ruv$add
  )

  # initialize objects for loop
  tdms_i <- data.frame(
    t = numeric(0),
    obs_type = numeric(0),
    true_y = numeric(0),
    y = numeric(0)
  )
  last_adjust_time <- 0

  for (j in adjust_at_dose) {
    # collect TDMs from today (use model for simulation!)
    adjust_time <- regimen$dose_times[j]
    collect_idx <- (tdm_times >= last_adjust_time & tdm_times < adjust_time)
    last_adjust_time <- adjust_time
    new_tdms <- collect_tdms(
      sim_model = sim_model,
      t_obs = tdm_times[collect_idx],
      res_var = ruv_i[collect_idx,],
      pars_i = pars_true_i,
      regimen = regimen,
      covariates = covariates
    )
    tdms_i <- rbind(tdms_i, new_tdms)

    # update regimen based on specified algorithm
    regimen <- dose_optimization_method(tdms = tdms, dose_update = j, ...)
  }
  list(
    final_regimen = regimen,
    final_estimates = est_par,
    tdms = tdms_i
  )
}

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
#' @returns

dose_adjust_map <- function(
  tdms,
  est_model,
  parameters,
  omega,
  ruv,
  regimen,
  covariates,
  t_obs,
  target,
  dose_update,
  dose_grid,
  ...
) {
  # get MAP fit, using model for estimation
  est_par <- simulate_fit(
    est_model = est_model,
    parameters = parameters,
    omega = omega,
    ruv = ruv,
    tdms = tdms,
    covariates = covariates,
    regimen = regimen,
    ...
  )

  # calculate new dose, using the estimation model
  new_dose <- dose_grid_search(
    est_model = est_model,
    regimen = regimen,
    parameters = est_par, # we want to use our "best guess" to get the dose
    t_obs = target_time,
    target = target,
    obs_comp = PKPDsim::get_model_auc_compartment(est_model),
    dose_update = dose_update,
    dose_grid = dose_grid,
    covariates = covariates,
    iov_bins = PKPDsim::get_model_iov(est_model)$bins,
    ...
  )
  # return new regimen
  regimen <- update_regimen(regimen, new_dose, dose_update)
  list(regimen = regimen, additional_info = est_par)
}
