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
#' @param regimen_update_design a data.frame with scheme with details on how
#' and when to update the regimen in the MIPD trial. Schemes are created using
#' `create_regimen_update_design()`.
#' @param sampling_design a data.frame with a sampling scheme, created using
#' `create_sampling_design()`.
#' @param target_design target design created using `create_target_design()`
#' @param regimen PKPDsim regimen object, containing initial dosing regimen.
#' @param covariates named list of PKPDsim covariates.
#' @param pars_true_i PK parameters for the individual. See `generate_iiv`.
#' @param sim_model model to use for simulating "true" patient response.
#' @param sim_ruv residual variability for `sim_model`. Named list for
#'   proportional (`prop`) and additive (`add`) error.
#' @param accumulate_data if `TRUE`, will use all available data up until the
#' adjustment timepoint. If set to `FALSE`, will use only the data since the
#' last adjustment timepoint and the current one.
#' @param ... arguments passed on to `simulate_fit` or dose_optimization_method
#'   function.
#' @param verbose verbose output?
#' @returns a named list containing `final_regimen` (all doses after
#' adjustment), `tdms` (all collected levels, both true and measured, that is,
#' both with and without residual variability), and `additional_info`, which
#' varies by dose_optimization_method. See selected function for details.
#'
#' @export
sample_and_adjust_by_dose <- function(
  regimen_update_design,
  sampling_design,
  target_design,
  regimen,
  covariates = NULL,
  pars_true_i,
  sim_model,
  sim_ruv = NULL,
  verbose = FALSE,
  accumulate_data = TRUE,
  ...
) {

  if (inherits(pars_true_i, "data.frame")) pars_true_i <- as.list(pars_true_i)

  adjust_at_dose <- get_dose_update_numbers_from_design(regimen_update_design, regimen)
  first_adjust_time <- regimen$dose_times[adjust_at_dose[1]]
  tdm_times <- get_sampling_times_from_scheme(sampling_design$scheme, regimen)
  if (!any(tdm_times < first_adjust_time)) {
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
  dose_updates <- c()
  aucs_i <- c()

  if(verbose) {
    message("Starting dose: ", round(regimen$dose_amts[1]))
  }

  for (j in 1:length(adjust_at_dose)) {
    if(verbose) message("Adjustment of dose# ", adjust_at_dose[j])
    # collect TDMs from today (use model for simulation!)
    adjust_time <- regimen$dose_times[adjust_at_dose[j]]
    tdm_times <- get_sampling_times_from_scheme(sampling_design$scheme, regimen)
    collect_idx <- (tdm_times >= last_adjust_time & tdm_times < adjust_time)
    if(!any(collect_idx)) {
      stop("No new samples in current adjustment interval, check target and sampling settings.")
    }
    last_adjust_time <- adjust_time
    if(verbose) {
      message("Samples times: ", paste0(tdm_times[collect_idx], collapse=", "))
    }
    new_tdms <- collect_tdms(
      sim_model = sim_model,
      t_obs = tdm_times[collect_idx],
      res_var = ruv_i[collect_idx,],
      pars_i = pars_true_i,
      regimen = regimen,
      covariates = covariates,
      lloq = sampling_design$lloq
    )
    auc_current_regimen <- calc_auc_from_regimen(
      regimen = regimen,
      parameters = pars_true_i, # true patient parameters
      model = sim_model,
      target_design = target_design,
      covariates = covariates
    )
    trough_current_regimen <- calc_concentration_from_regimen(
      regimen = regimen,
      parameters = pars_true_i, # true patient parameters
      model = sim_model,
      target_design = target_design,
      covariates = covariates
    )
    if(verbose) {
      message("TDMs: ", paste(round(new_tdms$y, 1), collapse=", "))
    }
    if(accumulate_data) {
      tdms_i <- rbind(tdms_i, new_tdms)
    } else {
      tdms_i <- new_tdms
    }
    dose_updates <- dplyr::bind_rows(
      dose_updates,
      data.frame(
        t = ifelse(j == 1, 0, regimen$dose_times[adjust_at_dose[j-1]]),
        dose_update = adjust_at_dose[j],
        t_adjust = regimen$dose_times[adjust_at_dose[j]],
        dose_before_update = regimen$dose_amts[adjust_at_dose[j]], # previous dose
        interval_before_update = regimen$interval, # previous interval
        auc_before_update = auc_current_regimen,
        trough_before_update = trough_current_regimen
      )
    )

    # update regimen based on specified algorithm
    out <- regimen_update_design$dose_optimization_method(
      tdms = tdms_i,
      dose_update = adjust_at_dose[j],
      regimen = regimen,
      target_design = target_design,
      covariates = covariates,
      ...
    )
    regimen <- out$regimen
    if(verbose) {
      message("New dose / interval: ", out$regimen$dose_amts[adjust_at_dose[j]], " / ", out$regimen$interval)
    }

    ## update the vector of dose_udpate numbers, if needed
    adjust_at_dose <- get_dose_update_numbers_from_design(
      regimen_update_design,
      regimen
    )

    additional_info <- c(
      additional_info,
      setNames(list(out$additional_info), paste0("dose_", adjust_at_dose[j]))
    )

  }

  ## Calculate AUC for final regimen
  auc_final <- calc_auc_from_regimen(
    regimen = regimen,
    parameters = pars_true_i, # true patient parameters
    model = sim_model,
    target_design = target_design,
    covariates = covariates
  )
  trough_final <- calc_concentration_from_regimen(
    regimen = regimen,
    parameters = pars_true_i, # true patient parameters
    model = sim_model,
    target_design = target_design,
    covariates = covariates
  )
  dose_updates <- dplyr::bind_rows(
    dose_updates,
    data.frame(
      t = regimen$dose_times[adjust_at_dose[j]],
      dose_update = NA,
      t_adjust = NA,
      dose_before_update = out$new_dose,
      interval_before_update = out$new_interval,
      auc_before_update = auc_final,
      trough_before_update = trough_final
    )
  )

  list(
    final_regimen = regimen,
    tdms = tdms_i,
    aucs = aucs_i,
    additional_info = additional_info,
    dose_updates = dose_updates
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
    iov_bins = PKPDsim::get_model_iov(est_model)$bins,
    ...
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
    additional_info = est_par
  )
}

#' Adjust intervals to achieve a target metric using MAP Bayesian estimation by
#' adapting the dosing interval
#'
#' Given a set of levels and a model definition, performs MAP Bayesian
#' estimation of individual PK/PD parameters, then finds the appropriate dosing
#' interval to achieve the specified PK/PD target and updates the
#' individual's regimen accordingly.
#'
#' @inheritParams simulate_fit
#' @inheritParams dose_grid_search
#' @param ... arguments passed on to PKPDmap::get_map_estimates and/or
#'   PKPDsim::sim
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
  if (is.null(grid)) {
    stop("Interval-optimization requires `grid` argument.")
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
    verbose = T,
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
    additional_info = est_par
  )
}
