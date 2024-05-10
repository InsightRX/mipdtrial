#' Adjust dosing using MIPD on TDMs at specified dose numbers
#'
#' Doses are iteratively adjusted for the specified dose numbers. Estimation is
#' performed by MAP Bayesian estimation, and simulation is used to identify the
#' dose most likely to reach the specified target. See `dose_grid_search` for
#' dose-finding logic. Optionally, disparate models can be used for simulation
#' and estimation, to allow for intentional model misspecification. Covariates
#' supplied must support both models. True patient parameters (`pars_true_i`)
#' must match the model used for simulation.
#'
#' Trial design or MIPD protocol can vary considerably in complexity. This
#' function is intended to cover a broad range of relatively straight forward
#' designs, and may serve as a template for more complex designs.
#'
#' @param adjust_at_dose vector of integers indicating which doses to adjust
#' @param tdm_times vector of times to sample at, in hours starting from the
#'   time of first dose.
#' @param regimen PKPDsim regimen object, containing initial dosing regimen.
#' @param covariates named list of PKPDsim covariates.
#' @param pars_true_i PK parameters for the individual. See `generate_iiv`.
#' @param est_model model to use for estimating patient response.
#' @param est_parameters parameters for `est_model`.
#' @param est_omega omega matrix for `est_model`
#' @param est_ruv residual variability for `est_model`. Named list for
#'   proportional (`prop`) and additive (`add`) error.
#' @param sim_model model to use for simulating "true" patient response. If
#'   NULL, uses `est_model`.
#' @param sim_ruv residual variability for `sim_model`. Named list for
#'   proportional (`prop`) and additive (`add`) error. If NULL, uses `est_ruv`.
#' @param dose_grid vector specifying doses to use as test grid for dose
#'   finding, Example: seq(from = 50, to = 500, by = (500 - 50) / 10 ). If
#'   NULL, draws from around the initial dose provided in `regimen`.
#' @param target target for dose optimization. Named list with `type` (e.g.,
#'   "conc", "auc") and `value` (e.g., 10).
#' @param target_time time (in hours since first dose) at which target should be
#'   estimated for dose finding.
#' @param ... arguments passed on to `simulate_fit` or `dose_grid_search`
#' @returns a named list containing `final_regimen` (all doses after
#' adjustment), `final_estimates` (MAP Bayesian estimation made with all
#' available levels) and `tdms` (all collected levels, both true and measured,
#' that is, both with and without residual variability).
#' @export

sample_and_adjust_by_dose <- function(
  adjust_at_dose,
  tdm_times,
  regimen,
  covariates = NULL,
  pars_true_i,
  est_model,
  est_parameters,
  est_omega,
  est_ruv,
  sim_model = NULL,
  sim_ruv = NULL,
  dose_grid = NULL,
  target = list(
    type = "conc",
    value = 10
  ),
  target_time = 24,
  ...
) {

  if (inherits(pars_true_i, "data.frame")) pars_true_i <- as.list(pars_true_i)

  # base dose finding grid on initial regimen
  if (is.null(dose_grid)) {
    d1 <- regimen$dose_amts[1]
    dose_grid <- seq(d1/5, d1 * 5, length.out = 10)
  }

  if (max(adjust_at_dose) > length(regimen$dose_times)) {
    stop("Insufficient doses in `regimen` for all dose adjustments specified.")
  }

  adjust_at_dose <- sort(adjust_at_dose)
  if (any(adjust_at_dose <= 1)) {
    stop("TDM collection before the first dose is not yet supported")
  }
  first_dose_time <- regimen$dose_times[adjust_at_dose[1]]
  if (!any(tdm_times < first_dose_time)) {
    stop("At least one TDM must be collected before dose adjustment")
  }

  if (is.null(sim_model)) sim_model <- est_model
  if (is.null(sim_ruv)) sim_ruv <- est_ruv

  # randomly draw error terms
  ruv_i <- generate_ruv(
    tdm_sample_time = tdm_times,
    prop = sim_ruv$prop,
    add = sim_ruv$add
  )

  # initialize objects for loop
  tdms_i <- data.frame(
    t = numeric(0),
    obs_type = numeric(0),
    true_y = numeric(0),
    y = numeric(0)
  )
  last_adjust_time <- 0

  for (j in adjust_at_dose) {
    # collect TDMs from today (use model for simulation!)
    adjust_time <- regimen$dose_times[j]
    collect_idx <- (tdm_times >= last_adjust_time & tdm_times < adjust_time)
    last_adjust_time <- adjust_time
    new_tdms <- collect_tdms(
      sim_model = sim_model,
      t_obs = tdm_times[collect_idx],
      res_var = ruv_i[collect_idx,],
      pars_i = pars_true_i,
      regimen = regimen,
      covariates = covariates
    )
    tdms_i <- rbind(tdms_i, new_tdms)

    # get MAP fit, using model for estimation
    est_par <- simulate_fit(
      est_model = est_model,
      parameters = est_parameters,
      omega = est_omega,
      ruv = est_ruv,
      tdms = tdms_i,
      covariates = covariates,
      regimen = regimen,
      ...
    )

    # calculate new dose, using the estimation model
    new_dose <- dose_grid_search(
      est_model = est_model,
      regimen = regimen,
      parameters = est_par, # we want to use our "best guess" to get the dose
      t_obs = target_time,
      target = target,
      obs_comp = attr(est_model, "size"), # last compartment is AUC
      dose_update = j,
      dose_grid = dose_grid,
      covariates = covariates,
      iov_bins = attr(est_model, "iov")$bins,
      ...
    )
    regimen <- update_regimen(regimen, new_dose, dose_update_number = j)
  }
  list(
    final_regimen = regimen,
    final_estimates = est_par,
    tdms = tdms_i
  )
}
