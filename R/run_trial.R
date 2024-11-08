#' Run an MIPD trial
#'
#' This function is a wrapper function to make it easier to run a basic MIPD
#' trial with minimal function calls. For fine-tune control over logic, it may
#' be easier to compose each step of the precision-dosing workflow yourself. For
#' an example, see the "Busulfan MIPD" vignette.
#'
#' This function performs the following steps:
#'
#' 1. Dose the patient using the function specified in `initial_dose_method`.
#' 2. Collect levels at the specified times (`tdm_times`).
#' 3. Adjust dosing at each specified dose (`adjust_doses_at`) using the logic
#'    specified in `dose_optimization_method`
#' 4. Return an object with data collected during and at the end of the MIPD
#'    trial.
#'
#' @inheritParams model_based_starting_dose
#' @param progress show progress bar? (default is `TRUE`)
#' @param n_ids number of subjects to use in simulated trial. If not specified,
#' will use all subjects in `data`.
#'
#' @export
#'
run_trial <- function(
    data,
    design,
    cov_mapping,
    n_ids = NULL,
    seed = NULL,
    progress = TRUE
) {
  if(!is.null(seed)) set.seed(seed) # important for reproducibility

  ## Set up data collectors
  tdms <- data.frame()
  dose_updates <- data.frame()
  additional_info <- list() # keep generic for other methods
  sim_parameters <- data.frame()
  gof <- data.frame()
  final_exposure <- data.frame()

  ## Set up number of individuals to simulate
  if(is.null(n_ids)) {
    n_ids <- nrow(data)
  } else {
    if(n_ids <= nrow(data)) {
      data <- data[1:n_ids,]
    } else {
      stop("`n_ids` cannot be larger than number of subjects in dataset.")
    }
  }

  ## Main loop
  if(progress) pb <- txtProgressBar(min = 1, max = n_ids, style = 2)
  for (i in data$ID) {
    if(progress) setTxtProgressBar(pb, i)
    #################################################################################
    ## Create individual
    #################################################################################
    # get patient covariates
    covs <- create_cov_object(
      data[data$ID == i,],
      mapping = cov_mapping
    )
    # randomly draw individual PK parameters
    pars_true_i <- generate_iiv(
      sim_model  = design$sim$model,
      omega      = design$sim$omega_matrix,
      parameters = design$sim$parameters
    )

    #################################################################################
    ## find initial starting dose: define basic regimen, then update
    ## the function that is called should be ble to take `design`, `covariates`,
    ## and `cov_mapping`.
    #################################################################################
    initial_reg <- design$initial_regimen$method(
      design = design,
      covariates = covs,
      cov_mapping = cov_mapping
    )

    #################################################################################
    ## Main patient-level loop: run through regimen optimization
    #################################################################################
    res <- sample_and_adjust_by_dose(
      regimen_update_design = design$regimen_update,
      sampling_design = design$sampling,
      target_design = design$target,
      regimen = initial_reg,
      covariates = covs,
      pars_true_i = pars_true_i,
      sim_model = design$sim$model,
      sim_ruv = design$sim$ruv,
      est_model = design$est$model,
      parameters = design$est$parameters,
      omega = design$est$omega_matrix,
      ruv = design$est$ruv
    )
    res$tdms$id <- i
    res$dose_updates$id <- i
    res$additional_info$id <- i
    sim_pars_i <- pars_true_i
    sim_pars_i$id <- i
    res$gof$id <-  i

    #################################################################################
    ## Collect data into object
    #################################################################################
    tdms <- rbind(tdms, res$tdms)
    dose_updates <- rbind(dose_updates, res$dose_updates)
    additional_info <- c(additional_info, res$additional_info)
    sim_parameters <- rbind(sim_parameters, sim_pars_i)
    gof <- rbind(gof, res$gof)

    if(design$target$type %in% target_types_auc) {
      auc_true <- calc_auc_from_regimen(
        regimen = res$final_regimen,
        parameters = pars_true_i, # true patient parameters
        model = design$sim$model,
        target_design = design$target,
        covariates = covs
      )
      auc_est <- calc_auc_from_regimen(
        regimen = res$final_regimen,
        parameters = res$additional_info[[1]],
        model = design$est$model,
        target_design = design$target,
        covariates = covs
      )
      final_exposure <- rbind(
        final_exposure,
        data.frame(id = i, auc_true = auc_true, auc_est = auc_est)
      )
    } else if (design$target$type %in% target_types_conc) {
      conc_true <- calc_concentration_from_regimen(
        regimen = res$final_regimen,
        parameters = pars_true_i, # true patient parameters
        model = design$sim$model,
        target_design = design$target,
        covariates = covs
      )
      conc_est <- calc_concentration_from_regimen(
        regimen = res$final_regimen,
        parameters = res$additional_info[[1]],
        model = design$est$model,
        target_design = design$target,
        covariates = covs
      )
      final_exposure <- rbind(
        final_exposure,
        data.frame(id = i, conc_true = conc_true, conc_est = conc_est)
      )
    }
  }

  out <- list(
    tdms = tdms,
    dose_updates = dose_updates,
    additional_info = additional_info,
    sim_parameters = sim_parameters,
    design = design,
    gof = gof,
    final_exposure = final_exposure
  )
  class(out) <- c("mipdtrial_results", "list")
  out
}
