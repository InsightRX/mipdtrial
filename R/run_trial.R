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
    sampling_design,
    target_design,
    regimen_update_design,
    sim_design,
    est_design,
    cov_mapping,
    n_ids = NULL,
    seed = NULL,
    progress = TRUE
) {
  if(!is.null(seed)) set.seed(12345) # important for reproducibility
  tdms <- c()
  dose_updates <- c()
  est_parameters <- c()
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
      sim_model = sim_design$model,
      omega = sim_design$omega_matrix,
      parameters = sim_design$parameters
    )

    #################################################################################
    ## Find initial starting dose
    #################################################################################
    # find initial starting dose: define basic regimen, then upda
    initial_reg <- model_based_starting_dose(
      sampling_design = tdm_design,
      target_design = target_design,
      n = 12,
      interval = 12,
      t_inf = 2,
      dose_resolution = 250, # round to nearest 250 mg
      grid = seq(250, 6000, by = 250),
      grid_type = "dose",
      est_model = sim_design$model,
      parameters = sim_design$parameters,
      covariates = covs,
      auc_comp = attr(sim_design$model, "size")
    )

    #################################################################################
    ## Main patient-level loop: run through regimen optimization
    #################################################################################
    res <- sample_and_adjust_by_dose(
      regimen_update_design = dose_update_design,
      sampling_design = tdm_design,
      target_design = target_design,
      regimen     = initial_reg,
      covariates  = covs,
      pars_true_i = pars_true_i,
      sim_model   = sim_design$model,
      sim_ruv     = sim_design$ruv,
      est_model   = est_design$model,
      parameters  = est_design$parameters,
      omega       = est_design$omega_matrix,
      ruv         = est_design$ruv
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
  }

  list(
    tdms = tdms,
    dose_updates = dose_updates,
    est_parameters = est_parameters,
    sim_parameters = pars_true_i
  )

}
