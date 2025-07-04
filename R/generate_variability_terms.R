#' Generate variability terms
#'
#' To enable comparison of multiple treatment conditions in a reproducible
#' manner, it is recommended that interindividual variability terms and residual
#' variability terms be generated prior to all analyses. This design also allows
#' for resuming a simulation part-way through, when the random seed position
#' may not be known.
#'
#' This family of functions generates variability terms to allow for
#' reproducible analyses. Using multiple iterations per individual ID allows for
#' PK variability within one set of covariates.
#' @name generate_variability
NULL


#' Generate inter-individual variability for a given model.
#'
#' Generate IIV for one or more individuals and one or more iterations per
#' individual according to the supplied omega matrix.
#'
#' By default, `generate_iiv`assumes a log-normal (exponential) distribution.
#' See `PKPDsim::sim` documentation for the `omega_type` argument to provide
#' finer grain control.
#'
#' @param sim_model model used for simulated patient response ("truth").
#' @param omega omega matrix, with covariance terms.
#'   See `PKPDsim::sim` for details.
#' @param parameters simulation model parameters (population estimates), a
#'   named list.
#' @param ids vector of ids, can be numeric or character.
#' @param n_iter number of sets of individual parameters to generate per id
#' @param seed set random seed
#' @param ... arguments passed on to PKPDsim::sim
#'
#' @returns `generate_iiv` a data frame with columns `id` (corresponding to
#'   `ids`), `iter` ( numbers 1 to n_iter) and columns for each individual
#'   parameter value.
#'
#' @rdname generate_variability
#'
#' @export
generate_iiv <- function(
  sim_model,
  omega,
  parameters,
  ids = 1,
  n_iter = 1,
  seed = NULL,
  ...
) {

  # create output data.frame with placeholder NA
  iiv <- expand.grid(iteration = seq_len(n_iter), id = ids)
  iiv[names(parameters)] <- NA_real_

  # create arbitrary objects for PKPDsim call
  req_covs <- attr(sim_model, "covariates")
  covs <- setNames(
    lapply(req_covs, \(x) PKPDsim::new_covariate(value = 1)),
    req_covs
  )
  cmt_types <- attr(sim_model, "cmt_mapping")
  reg <- PKPDsim::new_regimen(
    amt = 100,
    interval = 12,
    n = 3,
    t_inf = 1,
    type = ifelse(is.null(cmt_types), "infusion", names(cmt_types)[1])
  )

  # format IIV/IOV distributions
  iov_specs <- get_iov_specification(sim_model, parameters, omega)

  # generate parameters for each row of output data frame
  if (!is.null(seed)) {
    set.seed(seed)
  }
  for (i in seq_len(nrow(iiv))) {
    pars_i <- PKPDsim::sim(
      sim_model,
      omega = iov_specs$omega,
      omega_type = iov_specs$omega_type,
      parameters = iov_specs$parameters,
      iov_bins = iov_specs$bins,
      regimen = reg,
      covariates = covs,
      return_design = TRUE,
      checks = FALSE,
      ...
    )$p
    pars_i <- as.data.frame(pars_i[names(parameters)])
    iiv[i, names(parameters)] <- pars_i
  }

  iiv
}

#' Generate residual variability for a given model.
#'
#' Generate unexplained variability for one or more individuals and one or
#' more iterations per individual according to the supplied proportional and
#' additive error.
#'
#' `generate_ruv` Assumes a normal distribution for proportional and
#' additional error.
#'
#' @param tdm_sample_time time of tdm, since start of treatment course (or
#'   other vector of identifiers to use for each tdm). For example, for three
#'   days of daily dosing and peak-trough sample collection,
#'   `c(1, 24, 25, 48, 49, 73)`.
#' @param prop proportional error
#' @param add additive error
#' @returns `generate_ruv` returns a data frame with identifier columns of
#'   `tdm_number`, `iteration`, `id`, plus columns for proportional (`prop`) and
#'   additive (`add`) error.
#' @rdname generate_variability
#' @export

generate_ruv <- function(
  tdm_sample_time,
  prop,
  add,
  ids = 1,
  n_iter = 1,
  seed = NULL
) {
  # create output data.frame with placeholder NA
  ruv <- expand.grid(
    tdm_number = tdm_sample_time,
    iteration = seq_len(n_iter),
    id = ids
  )
  # sample from normal distribution
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ruv$prop <- 1 + rnorm(nrow(ruv), 0, prop)
  ruv$add <- rnorm(nrow(ruv), 0, add)
  ruv
}


