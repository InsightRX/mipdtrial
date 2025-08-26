#' Core function to simulate a single subject
#'
#' @inheritParams run_trial
#' @param data data for subject
#' @param pars_true_i PK parameters for the individual. See `generate_iiv`.
#' @param ... arguments passed on to `simulate_fit` or dose_optimization_method
#'   function.
#'
sim_subject <- function(
  data,
  cov_mapping,
  pars_true_i,
  design,
  verbose = FALSE,
  ...
) {

  ############################################################################
  ## Create individual
  ############################################################################
  id <- data$id
  covs <- create_cov_object(
    data,
    mapping = cov_mapping
  )

  ############################################################################
  ## find initial starting dose: define basic regimen, then update
  ## the function that is called. Any initial regimen function
  ## should have design, covariates, and cov_mapping as arguments.
  ############################################################################
  initial_reg <- design$initial_regimen$method(
    design = design,
    covariates = covs,
    cov_mapping = cov_mapping
  )

  ############################################################################
  ## Main patient-level loop: run through regimen optimization
  ############################################################################
  res <- sample_and_adjust_by_dose(
    regimen_update_design = design$regimen_update,
    sampling_design = design$sampling,
    target_design = design$target,
    regimen = initial_reg,
    covariates = covs,
    pars_true_i = pars_true_i,
    sim_model = design$sim$model,
    sim_ruv = design$sim$ruv,
    omega = design$est$omega_matrix,
    ruv = design$est$ruv,
    verbose = verbose,
    # below are not formal parameters, passed onwards using ...!
    parameters = design$est$parameters,
    est_model = design$est$model
  )

  # post-processing to get common exposure read-outs
  if(design$target$type %in% target_types_auc) {
    auc_true <- calc_auc_from_regimen(
      regimen = res$final_regimen,
      parameters = pars_true_i, # true patient parameters
      model = design$sim$model,
      target_design = design$target,
      covariates = covs
    )
    if(!is.null(res$additional_info)) {
      auc_est <- calc_auc_from_regimen(
        regimen = res$final_regimen,
        parameters = tail(res$additional_info, 1)[[1]],
        model = design$est$model,
        target_design = design$target,
        covariates = covs
      )
    } else {
      auc_est <- rep(NA, length(auc_true))
    }
    time_to_target <- calc_time_to_target(
      regimen = res$final_regimen,
      target_design = get_single_target_design(design$target),
      auc_comp = attr(design$sim$model, "size"),
      model = design$sim$model,
      parameters = pars_true_i,
      covariates = covs
    )
    final_exposure <- data.frame(
      id = id,
      auc_true = auc_true,
      auc_est = auc_est,
      tta = time_to_target,
      target_index = seq_along(auc_true)
    )
  } else if (design$target$type %in% target_types_conc) {
    conc_true <- calc_concentration_from_regimen(
      regimen = res$final_regimen,
      parameters = pars_true_i, # true patient parameters
      model = design$sim$model,
      target_design = design$target,
      covariates = covs
    )
    if(!is.null(res$additional_info)) {
      conc_est <- calc_concentration_from_regimen(
        regimen = res$final_regimen,
        parameters = tail(res$additional_info, 1)[[1]],
        model = design$est$model,
        target_design = design$target,
        covariates = covs
      )
    } else{
      conc_est <- rep(NA, length(conc_true))
    }
    time_to_target <- calc_time_to_target(
      regimen = res$final_regimen,
      target_design = get_single_target_design(design$target),
      auc_comp = NULL,
      model = design$sim$model,
      parameters = pars_true_i,
      covariates = covs
    )
    final_exposure <- data.frame(
      id = id,
      conc_true = conc_true,
      conc_est = conc_est,
      tta = time_to_target,
      target_index = seq_along(conc_true)
    )
  }

  # post-processing to get evaluation metrics
  eval_exposure <- NULL
  if (!is.null(design$evaluation)) {
    for (design_type in names(design$evaluation)){
      tmp_eval_design <- list(
        type = design_type,
        scheme = design$evaluation[[design_type]]
      )
      if (design_type %in% target_types_auc){
        f_calc <- calc_auc_from_regimen
      } else if (design_type %in% target_types_conc){
        f_calc <- calc_concentration_from_regimen
      }
      exposure_metric <- f_calc(
        regimen = res$final_regimen,
        parameters = pars_true_i, # true patient parameters
        model = design$sim$model,
        target_design = tmp_eval_design,
        covariates = covs
      )
      eval_time <- get_sampling_times_from_scheme(
        tmp_eval_design$scheme,
        res$final_regimen
      )
      eval_exposure <- data.frame(
        id = id,
        time = eval_time,
        value = exposure_metric,
        type = design_type
      )
    }
  }

  ############################################################################
  ## Collect data into object
  ############################################################################
  if(nrow(res$tdms) > 0) {
    res$tdms$id <- id
  }
  res$dose_updates$id <- id
  res$additional_info$id <- id
  if(nrow(res$gof) > 0) {
    res$gof$id <-  id
  }

  ## Return object
  list(
    tdms = res$tdms,
    dose_updates = res$dose_updates,
    additional_info = res$additional_info,
    gof = res$gof,
    final_exposure = final_exposure,
    eval_exposure = eval_exposure
  )
}
