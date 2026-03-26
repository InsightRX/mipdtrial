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
#' @param est_model model used for estimation (e.g. the model used in MAP
#'   fitting). If provided, a predictive individual prediction is simulated
#'   using `est_pars_i` and stored in the `predictive_ipred` column.
#' @param est_pars_i parameters for `est_model`. Typically population or
#'   MAP-estimated individual parameters.
#' @returns a data frame with columns `t` (time), `true_y` (actual level),
#'   `y` (measured level), and `predictive_ipred` (predicted level from
#'   estimation model; `NA` if `est_model` is not supplied), with rows
#'   corresponding to t_obs.
#' @export

collect_tdms <- function(
  sim_model,
  t_obs,
  res_var,
  pars_i,
  lloq = NULL,
  est_model = NULL,
  est_pars_i = NULL,
  ...
) {
  if (!isTRUE(length(t_obs) == nrow(res_var))) {
    cli::cli_abort("mismatch in # observations & residual error provided")
  }
  if (!all(c("prop", "add") %in% colnames(res_var))) {
    cli::cli_abort("residual variability must include proportional & additive error")
  }

  # simulate TDM collection (no residual error)
  true_tdm <- PKPDsim::sim(
    ode = sim_model,
    parameters = pars_i,
    t_obs = t_obs,
    only_obs = TRUE,
    ...
  )
  # rename output for clarity/ease (PKPDmap uses `y` column for estimation)
  colnames(true_tdm)[colnames(true_tdm) == "y"] <- "true_y"
  true_tdm <- true_tdm[, c("t", "obs_type", "true_y")]

  # add residual error
  true_tdm$y <- res_var$prop * true_tdm$true_y + res_var$add

  # simulate prediction from current estimation model, with current parameters (for predictive analysis)
  # and add to tdm object as `est_y`
  if(!is.null(est_model)) {
    true_tdm_est <- PKPDsim::sim(
      ode = est_model,
      parameters = est_pars_i,
      t_obs = t_obs,
      only_obs = TRUE,
      ...
    )
    true_tdm$predictive_ipred <- true_tdm_est$y
  } else {
    true_tdm$predictive_ipred <- NA
  }

  # LOQ handling
  if (!is.null(lloq) && !is.na(lloq)) {
    true_tdm$y[true_tdm$y < lloq] <- lloq / 2
  }

  true_tdm
}
