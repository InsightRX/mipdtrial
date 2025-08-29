#' Model-based starting dose
#'
#' A light wrapper for [dose_grid_search] for finding model-based starting doses
#' and returning an appropriate regimen object.
#'
#' @inheritParams run_trial
#' @param covariates named list of PKPDsim covariates. E.g.:
#'   list(WT = PKPDsim::new_covariate(70))
#' @param ... arguments passed on to `dose_grid_search`
#' @export
#' @returns Returns a PKPDsim regimen populated with the model-predicted dose

model_based_starting_dose <- function(
    design,
    covariates,
    cov_mapping,
    ...
) {
  ## Parsing and checking design specs
  reg_md <- design$initial_regimen$regimen
  interval <- reg_md$interval
  t_inf <- reg_md$t_inf
  type <- reg_md$type
  if(is.null(interval) || is.null(type)) {
    cli::cli_abort("`interval` and type` are needed for `initial_regimen` design.")
  }
  if(is.null(t_inf)) {
    t_inf <- 0
    if(type == "infusion") {
      cli::cli_warn("No `t_inf` specified but `type` is `infusion`. Setting `t_inf` to 0 (bolus).")
    }
  }

  # number of doses for initial regimen evaluation: get the dose # at which we
  # want to reach the target
  initial_target <- get_single_target_design(design$target, idx = 1)
  scheme <- initial_target$scheme[1,]
  n_doses <- get_n_doses_from_scheme(scheme, interval) 
  
  ## create a dummy regimen as input to dose_grid_search:
  reg <- PKPDsim::new_regimen(
    amt = 1,
    n = n_doses,
    type = type,
    t_inf = t_inf,
    interval = interval
  )
  args <- list(
    est_model = design$est$model,
    regimen = reg,
    target_design = initial_target,
    parameters = design$est$parameters,
    omega = design$est$omega_matrix,
    ruv = design$est$ruv,
    covariates = covariates,
    dose_update = 1,
    grid_type = "dose"
  )

  ## extra settings for the optimization can be specified in the initial regimen
  ## design, e.g. auc_comp, dose_grid
  settings <- design$initial_regimen$settings
  # convert grid to actual grid from specified seq() params
  if(!is.null(settings$dose_grid)) {
    settings$grid <- seq(settings$dose_grid[1], settings$dose_grid[2], settings$dose_grid[3])
    settings$dose_grid <- NULL
  }
  args <- c(args, settings)

  ## Call core function
  starting_dose <- do.call(dose_grid_search, args)
  
  ## elongate regimen, if multiple time-varying targets specified
  ## For a target design specified with multiple time-varying targets,
  ## the initial dose regimen will need to be determined on the initial target.
  ## However, the total regimen will need to have at least sufficient doses
  ## to also support the final target time. Therefore we need to increase the
  ## length of the initial regimen.
  if(nrow(design$target$scheme) > 1) {
    final_target <- get_single_target_design(design$target)
    final_scheme <- final_target$scheme[1,]
    n_doses_final <- get_n_doses_from_scheme(final_scheme, interval)
    reg <- PKPDsim::new_regimen(
      amt = starting_dose,
      n = n_doses_final,
      type = type,
      t_inf = t_inf,
      interval = interval
    )
  } else { ## otherwise, just set all amounts in regimen to selected dose
    reg$dose_amts <- rep(starting_dose, length(reg$dose_amts))
  }
  
  reg
}

get_n_doses_from_scheme <- function(scheme, interval) {
  if(scheme$anchor == "dose") {
    n_doses <- scheme$at
  } else {
    t_aim <- (scheme$at-1) * 24
    n_doses <- which.min(abs(seq(0, 100*interval, interval) - t_aim))
  }
}
