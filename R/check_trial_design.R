#' Check design
#'
#' @param design a trial design object
#'
#' @returns a trial design object
#'
check_trial_design <- function(design) {
  if(is.null(design$initial_regimen)) {
    cli::cli_abort("Please specify initial regimen design.")
  }
  if(is.null(design$sampling)) {
    cli::cli_warn("No `sampling_design` provided, will not perform sampling.")
  }
  if(is.null(design$regimen_update)) {
    cli::cli_warn("No `regimen_update_design` provided, will not perform regimen optimization.")
  }
  ## Check that target vector length is either 1, or same as 
  ## regimen update vector length.
  if(! (nrow(design$target$scheme) == 1 || nrow(design$target$scheme) == nrow(design$regimen_update$scheme))) {
    cli::cli_abort("The number of target instances should be either 1 or the same length as the number of regimen update instances.")
  }
  ##  `regimen_update_design$dose_optimization_method` can be passed as
  ##  reference to function, not a function itself, in that case we need to
  ##  `get()` the actual function.
  if(inherits(design$regimen_update$dose_optimization_method, "character")) {
    design$regimen_update$dose_optimization_method <- get(design$regimen_update$dose_optimization_method)
  }
  ##  `initial_regimen$method`: same, can be passed as character or function.
  if(inherits(design$initial_regimen$method, "character")) {
    design$initial_regimen$method <- get(design$initial_regimen$method)
  }
  if(is.null(design$sim)) {
    cli::cli_abort("A simulation model is required.")
  }
  if(is.null(design$est)) {
    design$est <- design$sim
    cli::cli_warn("No model for estimation (`est`) defined, using same model as specified for simulations.")
  }
  if(sum(unlist(design$est$ruv)) == 0) {
    cli::cli_abort("Residual error magnitude for estimation model cannot be zero.")
  }
  design
}