#' Weight-based starting dose (e.g., mg/kg)
#'
#' This nomogram is a possible `method` supplied to
#' `create_initial_regimen_design()`. When this method is used,
#' `create_initial_regimen_design()` must also be supplied with `mgkg` and
#' `dosing_weight`, which must match a column name in the supplied data set.
#'
#' @inheritParams model_based_starting_dose
#' @export

weight_based_starting_dose <- function(covariates, design, cov_mapping) {
  reg_def <- design$initial_regimen$regimen

  if (is.null(design$initial_regimen$mgkg)) {
    cli::cli_abort(
      paste0(
        "For weight-based initial dosing, ",
        "specify `create_initial_regimen_design(mgkg = ...)`"
      )
    )
  }
  if (is.null(design$initial_regimen$dosing_weight)) {
    cli::cli_abort(
      paste0(
        "For weight-based initial dosing, ",
        "specify `create_initial_regimen_design(dosing_weight = ...)`"
      )
    )
  }
  cov_for_dosing <- names(
    which(cov_mapping == design$initial_regimen$dosing_weight)
  )
  if (is.null(cov_for_dosing) || length(cov_for_dosing) == 0) {
    cli::cli_abort("dosing_weight covariate missing from `cov_mapping`")
  }

  weight_for_dosing <- covariates[[cov_for_dosing]]$value
  if (is.null(weight_for_dosing)) {
    cli::cli_abort("Dosing weight not available in data set")
  }

  reg_def$amt <- weight_for_dosing * design$initial_regimen$mgkg

  do.call(PKPDsim::new_regimen, reg_def)
}

