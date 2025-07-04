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
  iov_bins_sim <- attr(sim_model, "iov")$bins

  ## Get times to adjust dose
  if(!is.null(regimen_update_design)) {
    adjust_at_dose <- get_dose_update_numbers_from_design(regimen_update_design, regimen)
    first_adjust_time <- regimen$dose_times[adjust_at_dose[1]]
  } else {
    adjust_at_dose <- c()
    first_adjust_time <- NA
  }

  ## Get sampling times
  if(!is.null(sampling_design)) {
    tdm_times <- get_sampling_times_from_scheme(sampling_design$scheme, regimen)
    if (!any(tdm_times < first_adjust_time)) {
      msg <- paste0(
        "At least one TDM must be collected before dose adjustment.\n",
        "Sampling times: ", paste0(tdm_times, collapse = ", "), "\n",
        "Dose adjustment times: ", paste0(regimen$dose_times[adjust_at_dose], collapse = ", ")
      )
      cli::cli_abort(msg)
    }
  } else {
    tdm_times <- c()
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
  dose_updates <- data.frame(
    t = numeric(0),
    dose_update = numeric(0),
    t_adjust = numeric(0),
    dose_before_update = numeric(0),
    interval_before_update = numeric(0),
    auc_before_update = numeric(0),
    trough_before_update = numeric(0)
  )
  aucs_i <- c()
  gof <- data.frame(
    pred = numeric(0),
    ipred = numeric(0),
    dv = numeric(0),
    weights = numeric(0),
    update = numeric(0)
  )

  if(verbose) {
    cli::cli_alert_info(paste0("Starting dose: ", round(regimen$dose_amts[1])))
  }

  out <- list()
  for (j in seq_along(adjust_at_dose)) {
    if(verbose) {
      cli::cli_alert_info(paste0("Adjustment of dose# ", adjust_at_dose[j]))
    }
    # collect TDMs from today (use model for simulation!)
    adjust_time <- regimen$dose_times[adjust_at_dose[j]]
    tdm_times <- get_sampling_times_from_scheme(sampling_design$scheme, regimen)
    collect_idx <- (tdm_times >= last_adjust_time & tdm_times < adjust_time)
    if(!any(collect_idx)) {
      cli::cli_abort("No new samples in current adjustment interval, check target and sampling settings.")
    }
    last_adjust_time <- adjust_time
    if(verbose) {
      cli::cli_alert_info(paste0("Sample times: ", paste0(tdm_times[collect_idx], collapse=", ")))
    }
    new_tdms <- collect_tdms(
      sim_model = sim_model,
      t_obs = tdm_times[collect_idx],
      res_var = ruv_i[collect_idx,],
      pars_i = pars_true_i,
      regimen = regimen,
      covariates = covariates,
      lloq = sampling_design$lloq,
      iov_bins = iov_bins_sim
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
      cli::cli_alert_info(paste0("TDMs: ", paste(round(new_tdms$y, 1), collapse=", ")))
    }
    if(accumulate_data) {
      tdms_i <- rbind(tdms_i, new_tdms)
    } else {
      tdms_i <- new_tdms
    }
    dose_updates <- rbind(
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
    method_args <- regimen_update_design$args
    method_args <- append(method_args, list(
      tdms = tdms_i,
      dose_update = adjust_at_dose[j],
      regimen = regimen,
      target_design = target_design,
      covariates = covariates
    ))
    method_args <- append(method_args, list(...))
    out <- do.call(regimen_update_design$dose_optimization_method, method_args)
    regimen <- out$regimen
    if(verbose) {
      cli::cli_alert_info(paste0("New dose / interval: ", out$regimen$dose_amts[adjust_at_dose[j]], " / ", out$regimen$interval))
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
    if (!is.null(out$gof) & inherits(out$gof, "data.frame")) {
      out$gof$update <- j
      gof <- rbind(gof, out$gof)
    }
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

  dose_updates <- rbind(
    dose_updates,
    bind_results_from_adjustments(
      out,
      j,
      regimen,
      adjust_at_dose,
      dose_before_update,
      auc_final,
      trough_final
    )
  )

  list(
    final_regimen = regimen,
    tdms = tdms_i,
    aucs = aucs_i,
    dose_updates = dose_updates,
    additional_info = additional_info,
    gof = gof
  )

}

#' Bind together the results from sampling and dose adjusting
#' 
#' @param out output object
#' @param j index number
#' @param regimen regimen
#' @param adjust_at_dose adjust at dose number
#' @param dose_before_update dose before update
#' @param auc_final final AUC
#' @param trough_final final Ctrough
#'  
#' @returns a data.frame with results
#'
#' 
bind_results_from_adjustments <- function(
  out,
  j,
  regimen,
  adjust_at_dose,
  dose_before_update,
  auc_final,
  trough_final
) {
  ## Calculate AUC for final regimen
  if(length(adjust_at_dose) > 0) {
    t <- regimen$dose_times[adjust_at_dose[j]]
  } else {
    t <- rep(NA, length(auc_final))
  }
  if(!is.null(out$new_dose)) {
    dose_before_update <- out$new_dose
  } else {
    dose_before_update <- rep(NA, length(auc_final))
  }
  if(!is.null(out$new_interval)) {
    interval_before_update <- out$new_interval
  } else {
    interval_before_update <- rep(NA, length(auc_final))
  }
  data.frame(
    t = t,
    dose_update = NA,
    t_adjust = NA,
    dose_before_update = dose_before_update,
    interval_before_update = interval_before_update,
    auc_before_update = auc_final,
    trough_before_update = trough_final
  )
}
