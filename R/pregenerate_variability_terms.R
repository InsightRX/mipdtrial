#' Pre-generate variability terms
#'
#' To enable comparison of multiple treatment conditions in a reproducible
#' manner, it is recommended that interindividual variability terms and residual
#' variability terms be generated prior to all analyses. This design also allows
#' for resuming a simulation part-way through, when the random seed position
#' may not be known.
#'
#' This family of functions pregenerates variability terms to allow for
#' reproducible analyses. Using multiple iterations per individual ID allows for
#' PK variability within one set of covariates.
#' @name pregenerate_variability
NULL


#' Pre-generate inter-individual variability for a given model.
#'
#' Pre-generate IIV for one or more individuals and one or more iterations per
#' individual according to the supplied omega matrix.
#'
#' By default, `pregenerate_iiv`assumes a log-normal (exponential) distribution.
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
#' @returns `pregenerate_iiv` a data frame with columns `id` (corresponding to
#'   `ids`), `iter` ( numbers 1 to n_iter) and columns for each individual
#'   parameter value.
#' @rdname pregenerate_variability
#' @export

pregenerate_iiv <- function(
  sim_model,
  omega,
  parameters,
  ids = 1,
  n_iter = 1,
  seed = 1,
  ...
) {

  # create output data.frame with placeholder NA
  iiv <- expand.grid(iteration = seq_len(n_iter), id = ids)
  pars <- setdiff(names(parameters), attr(sim_model, "fixed"))
  iiv[pars] <- NA_real_

  # create arbitrary objects for PKPDsim call
  req_covs <- attr(sim_model, "covariates")
  covs <- setNames(
    lapply(req_covs, \(x) PKPDsim::new_covariate(value = 1)),
    req_covs
  )
  reg <- PKPDsim::new_regimen(amt = 100, interval = 12, n = 3, type = "oral")

  # generate parameters for each row of output data frame
  set.seed(seed)
  for (i in seq_len(nrow(iiv))) {
    pars_i <- PKPDsim::sim(
      sim_model,
      omega = omega,
      parameters = parameters,
      regimen = reg,
      covariates = covs,
      return_design = TRUE,
      ...
    )$p
    pars_i <- as.data.frame(pars_i[pars])
    iiv[i,pars] <- pars_i
  }
  iiv
}

#' Pre-generate residual variability for a given model.
#'
#' Pre-generate unexplained variability for one or more individuals and one or
#' more iterations per individual according to the supplied proportional and
#' additive error.
#'
#' `pregenerate_ruv` Assumes a normal distribution for proportional and
#' additional error.
#'
#' @param tdms_per_course number of tdms per treatment course. For example, for
#'   a 4-day course with 6 levels per day, enter 4 * 6 = 24.
#' @param prop proportional error
#' @param add additive error
#' @returns `pregenerate_ruv` returns a data frame with identifier columns of
#'   `tdm_number`, `iteration`, `id`, plus columns for proportional (`prop`) and
#'   additive (`add`) error.
#' @rdname pregenerate_variability
#' @export

pregenerate_ruv <- function(ids, n_iter, tdms_per_course, prop, add, seed = 2) {
  # create output data.frame with placeholder NA
  ruv <- expand.grid(
    tdm_number = seq_len(tdms_per_course),
    iteration = seq_len(n_iter),
    id = ids
  )
  # sample from normal distribution
  set.seed(seed)
  ruv$prop <- 1 + rnorm(nrow(ruv), 0, prop)
  ruv$add <- rnorm(nrow(ruv), 0, add)
  ruv
}
