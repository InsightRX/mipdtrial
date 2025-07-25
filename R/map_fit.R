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
  iov_obj <- get_iov_specification(est_model, parameters, omega)

  fit <- tryCatch({
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
      verbose = FALSE,
      skip_hessian = TRUE, # faster
      ...
    )},
    error = function(e) list(parameters = NULL)
  )
  if (is.atomic(fit) || !"parameters" %in% names(fit)) return(NULL)
  fit
}

#' Get inter-occasion variability specifications
#'
#' This function is intended to be called from other functions. Using
#' specifications encoded within the model, reshapes IOV and IIV into a single
#' omega matrix, extracts the correct IOV bins, determines the appropriate eta
#' distributions, etc, ready for use in PKPDmap and PKPDsim functions.
#'
#' @param model PKPDsim model
#' @param parameters model parameters, a named list
#' @param iiv lower triangle of IIV matrix, supplied as a vector
#' @returns a named list, with at least fields for `n_bins` and `bins`, and
#'   (for models with IOV) optionally also a fully omega matrix and omega type.
#' @noRd

get_iov_specification <- function(model, parameters, iiv) {
  iov_specs <- attr(model, "iov")

  iov <- PKPDmap::create_iov_object(
    cv = iov_specs[["cv"]],
    omega = iiv,
    bins = iov_specs$bins,
    parameters = parameters,
    fixed = attr(model, "fixed"),
    estimate_init_level = FALSE,
    verbose = FALSE
  )
  iiv_par <- setdiff(names(iov$parameters), c(iov$kappa, iov$fixed))

  iov$omega_type <- c(
    rep("exponential", length(iiv_par)),
    rep("normal", length(iov$kappa))
  )
  iov
}
