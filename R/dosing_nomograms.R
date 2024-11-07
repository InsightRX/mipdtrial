#' Weight-based starting dose (e.g., mg/kg)
#'
#' This nomogram is a possible `method` supplied to
#' `create_initial_regimen_design()`. An argument for `mgkg` should also be
#' supplied to that function call. This function expects that a covariate `"WT"`
#' exists in the processed patient parameters.
#'
#' @inheritParams model_based_starting_dose
#' @export

weight_based_starting_dose <- function(covariates, design, cov_mapping) {
  reg_def <- design$initial_regimen$regimen

  if (is.null(design$initial_regimen$mgkg)) {
    stop(
    "For weight-based initial dosing, ",
    "specify `create_initial_regimen_design(mgkg = ...)`"
    )
  }
  if (is.null(covariates$WT$value)) {
    stop("Weight (WT) must be specified for weight-based initial dosing")
  }
  reg_def$amt <- covariates$WT$value * design$initial_regimen$mgkg

  do.call(PKPDsim::new_regimen, reg_def)
}

