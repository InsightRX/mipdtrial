#' Simulate TDM collection
#'
#' Using the "ground truth" model, simulate collection of drug/biomarker levels.
#' Returns a data frame of sample time, true value, and measured value (with
#' residual error added).
#'
#' @param sim_model model used for simulated patient response ("truth").
#' @param t_obs sample collection times (since start of treatment)
#' @param res_var data frame of residual variability quantities to add. See
#'   `pregenerate_ruv` for details. Expects columns `prop` and `add`. Error is
#'   added in the linear domain, so log-transformed models should supply error
#'   as proportional error with 0 additive error.
#' @param pars_i true parameters for the individual (named list)
#' @param lloq lower limit of quantification. If non-NULL, all TDMs below LLOQ
#'   will be set to half the LLOQ.
#' @param ... arguments passed on to PKPDsim::sim
#' @returns a data frame with columns `t` (time), `true_y` (actual level) and
#'   `y` (measured level), with rows corresponding to t_obs.
#' @export

collect_tdms <- function(
  sim_model,
  t_obs,
  res_var,
  pars_i,
  lloq = NULL,
  ...
) {
  if (!isTRUE(length(t_obs) == nrow(res_var))) {
    cli::cli_abort("mismatch in # observations & residual error provided")
  }
  if (!all(c("prop", "add") %in% colnames(res_var))) {
    cli::cli_abort("residual variability must include proportional & additive error")
  }

  # simulate TDM collection (no residual error)
  true_tdm <- sim(
    ode = sim_model,
    parameters = as.list(pars_i),
    t_obs = t_obs,
    only_obs = TRUE,
    ...
  )
  # rename output for clarity/ease (PKPDmap uses `y` column for estimation)
  colnames(true_tdm)[colnames(true_tdm) == "y"] <- "true_y"
  true_tdm <- true_tdm[, c("t", "obs_type", "true_y")]

  # add residual error
  true_tdm$y <- res_var$prop * true_tdm$true_y + res_var$add

  # LOQ handling
  if (!is.null(lloq) && !is.na(lloq)) {
    true_tdm$y[true_tdm$y < lloq] <- lloq / 2
  }

  true_tdm
}
