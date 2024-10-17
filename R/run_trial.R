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
  if(!is.null(seed)) set.seed(12345) # important for reproducibility
  tdms <- c()
  dose_updates <- c()
  est_parameters <- c()
  sim_parameters <- c()
  if(is.null(n_ids)) {
    n_ids <- nrow(data)
  } else {
    if(n_ids <= nrow(data)) {
      data <- data[1:n_ids,]
    } else {
      stop("`n_ids` cannot be larger than number of subjects in dataset.")
    }
  }
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
    ## Find initial starting dose
    #################################################################################
    # find initial starting dose: define basic regimen, then upda
    initial_reg <- model_based_starting_dose(
      sampling_design = design$tdm,
      target_design = design$target,
      n = 12,
      interval = 12,
      t_inf = 2,
      dose_resolution = 250, # round to nearest 250 mg
      grid = seq(250, 6000, by = 250),
      grid_type = "dose",
      est_model = design$est$model,
      parameters = design$est$parameters,
      covariates = covs,
      auc_comp = attr(design$est$model, "size")
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

    #################################################################################
    ## Collect data into object
    #################################################################################
    tdms <- dplyr::bind_rows(
      tdms,
      res$tdms %>% dplyr::mutate(id = i)
    )
    dose_updates <- dplyr::bind_rows(
      dose_updates,
      res$dose_updates %>% dplyr::mutate(id = i)
    )
    est_parameters <- dplyr::bind_rows(
      est_parameters,
      bind_rows(lapply(res$est_parameters, data.frame)) %>%
        dplyr::mutate(id = i, dose_update = 1:nrow(.))
    )
    sim_parameters <- dplyr::bind_rows(
      sim_parameters,
      pars_true_i %>% dplyr::mutate(id = i)
    )
  }

  out <- list(
    tdms = tdms,
    dose_updates = dose_updates,
    est_parameters = est_parameters,
    sim_parameters = sim_parameters,
    design = design
  )
  class(out) <- c("mipdtrial_results", "list")
  out
}
