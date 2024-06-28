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
#' @param dose_optimization_method A function that determines how doses should
#'   be adjusted given the collected drug levels. The function must return a
#'   named list of the structure `list(regimen = reg, additional_info = x)`,
#'   where `reg` is the updated PKPDsim regimen for the patient and `x` can be
#'   another other information useful for post-processing of trial results.
#' @param ... arguments passed on to `simulate_fit` or dose_optimization_method
#'   function.
#' @returns a named list containing `final_regimen` (all doses after
#' adjustment), `tdms` (all collected levels, both true and measured, that is,
#' both with and without residual variability), and `additional_info`, which
#' varies by `dose_optimization_method`. See selected function for details.
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

  additional_info <- c()

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
    out <- dose_optimization_method(
      tdms = tdms_i,
      dose_update = j,
      regimen = regimen,
      covariates = covariates,
      additional_info = additional_info,
      ...
    )
    regimen <- out$regimen
    additional_info <- c(
      additional_info,
      setNames(list(out$additional_info), paste0("dose_", j))
    )
  }

  list(
    final_regimen = regimen,
    tdms = tdms_i,
    additional_info = additional_info
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
#' @returns Returns a named list: `regimen`: the updated regimen;
#'   `additional_info`: the MAP parameter estimates
#' @export

dose_adjust_map <- function(
  tdms,
  est_model,
  parameters,
  omega,
  ruv,
  regimen,
  covariates = NULL,
  target_time,
  target,
  dose_update,
  dose_grid = NULL,
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
  if (is.null(dose_grid)) {
    # base dose finding grid on initial regimen
    d1 <- regimen$dose_amts[1]
    dose_grid <- seq(d1/5, d1 * 5, length.out = 10)
  }
  new_dose <- dose_grid_search(
    est_model = est_model,
    regimen = regimen,
    parameters = est_par, # we want to use our "best guess" to get the dose
    target_time = target_time,
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


#' Adjust doses to achieve a target metric using NCA
#'
#' Given a set of levels calculates the AUC for a given dose, and adjusts the
#' subsequent doses to achieve either the target AUC per dosing interval
#' (`create_target_object(targettype = 'auc')`), AUC24
#' (`create_target_object(targettype = 'auc24')`) or cumulative AUC
#' (`create_target_object(targettype = 'cum_auc')`). Uses the most recently
#' sampled dosing interval to estimate AUC, extrapolating that over all doses
#' between the dose to be updated and the last sampled dose. Note that in order
#' to consider AUC contributions from previous doses, a baseline TDM collected
#' within 30 minutes of the dose administration is required. This is a
#' limitation of the algorithm, and not of the function. See [dose_from_auc()]
#' for more information about dose selection logic.
#'
#' @inheritParams dose_adjust_map
#' @param additional_info object returned and iteratively built in
#'   `sample_and_adjust_by_dose`. See `returns`
#' @param ... arguments passed on to `clinPK::nca`
#' @returns Returns a named list: `regimen`: the updated regimen;
#'   `additional_info`: output of clinPK::nca with a field for cumulative auc by
#'   that dose.
#' @export
dose_adjust_nca <- function(
  tdms,
  regimen,
  target,
  dose_update,
  additional_info,
  ...
) {
  # perform NCA,
  nca_res <- perform_nca(
    tdms = tdms,
    regimen = regimen,
    ...
  )

  # calculate new AUC since last NCA calculation, necessary for e.g., busulfan,
  # where might dose Q6 but sample only 1 interval
  intv_auc <- nca_res$descriptive$auc_tau
  if (length(additional_info) > 0) {
    last_dose_updated <- as.numeric(gsub(
      "dose_", "",
      names(additional_info[length(additional_info)])
    ))
    cumulative_auc <- additional_info[[length(additional_info)]]$cumulative_auc
  } else {
    last_dose_updated <- 1
    cumulative_auc <- 0
  }
  auc_since_last_update <- intv_auc * (dose_update - last_dose_updated)
  nca_res$cumulative_auc <- cumulative_auc + auc_since_last_update


  # calculate new dose, using a ratio of AUC to dose
  new_dose <- dose_from_auc(
    target = target,
    intv_auc = nca_res$descriptive$auc_tau,
    regimen = regimen,
    dose_update = dose_update,
    cum_auc = nca_res$cumulative_auc
  )

  # return new regimen
  regimen <- update_regimen(regimen, new_dose, dose_update)
  list(regimen = regimen, additional_info = nca_res)
}
