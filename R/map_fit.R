#' Get MAP Bayesian parameters
#'
#' A light wrapper around `PKPDmap::get_map_estimates` and
#' `PKPDmap::create_iov_object`. For more fine-grained control, use `PKPDmap`
#' functions directly.
#'
#' MAP Bayesian fits sometimes fail when simulated values are very unlikely. To
#' prevent a long simulation from stalling, fit is wrapped in a tryCatch, and
#' NULL is returned for appropriate processing outside the function.
#' @param est_model model used for estimation
#' @param parameters population parameters of estimation model
#' @param omega omega matrix of estimation model
#' @param ruv residual error distribution of estimation model, a named list with
#'   elements for `prop` (proportional) and `add` (additive).
#' @param tdms observations to be included in MAP estimates
#' @param covariates named list of PKPDsim covariates
#' @param regimen PKPDsim regimen object
#' @param ... arguments passed on to PKPDmap::get_map_estimates
#' @returns named list of individual PK parameter estimates
#' @export

simulate_fit <- function(
    est_model,
    parameters,
    omega,
    ruv,
    tdms,
    covariates,
    regimen,
    ...
){
  iov_specs <- attr(est_model, "iov")
  if (!isTRUE(iov_specs$n_bins > 1)) iov_specs$bins <- c(0, 99999)
  iov_obj <- PKPDmap::create_iov_object(
    cv = iov_specs[["cv"]],
    omega = omega,
    bins = iov_specs$bins,
    parameters = parameters,
    fixed = attr(est_model, "fixed"),
    estimate_init_level = FALSE,
    verbose = FALSE
  )

  x <- tryCatch({
    get_map_estimates(
      model = est_model,
      error = ruv,
      data = tdms,
      covariates = covariates,
      regimen = regimen,
      parameters = iov_obj$parameters,
      omega = iov_obj$omega,
      iov_bins = iov_obj$bins,
      as_eta = iov_obj$kappa,
      fixed = iov_obj$fixed,
      int_step_size = 0.01,
      verbose = FALSE,
      skip_hessian = TRUE, # faster
      ...
    )},
    error = function(e) list(parameters = NULL)
  )
  if (is.atomic(x) || !"parameters" %in% names(x)) return(NULL)
  x[["parameters"]]
}
