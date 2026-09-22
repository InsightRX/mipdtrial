#' Adjust both dose and interval to achieve a target metric using MAP Bayesian
#' estimation
#'
#' Given a set of levels and a model definition, performs MAP Bayesian
#' estimation of individual PK/PD parameters, then searches over a grid of
#' candidate intervals. For each interval, the optimal dose is found via
#' `dose_grid_search()`. The best (dose, interval) pair is selected based on
#' distance to target, with tie-breaking toward a preferred interval.
#'
#' @inheritParams map_adjust_dose
#' @param interval_grid numeric vector of candidate intervals (hours) to
#'   evaluate. Default: `c(6, 8, 12, 24, 48, 72)`.
#' @param preferred_interval numeric scalar indicating the preferred interval
#'   (hours) for tie-breaking when multiple (dose, interval) pairs achieve
#'   similar attainment. Default: `24`.
#' @param attainment_tolerance numeric scalar (fraction of target value)
#'   defining the tolerance band for tie-breaking. Candidates within
#'   `attainment_tolerance * abs(target_value)` of the best attainment are
#'   considered equivalent. Default: `0.05`.
#' @param settings list of arguments to be used in dose update, e.g.
#'   `dose_resolution`, `min_dose`, `max_dose`.
#' @param ... arguments passed on to PKPDsim::sim via dose grid search
#' @returns Returns a named list: `regimen`: the updated regimen;
#'   `dose_update`: the dose number updated; `new_dose`: the selected dose;
#'   `new_interval`: the selected interval; `additional_info`: the MAP
#'   parameter estimates; `gof`: goodness-of-fit data.frame.
#' @export
#'
map_adjust_dose_interval <- function(
    tdms,
    est_model,
    parameters,
    omega,
    ruv,
    regimen,
    covariates = NULL,
    target_design,
    dose_update,
    grid = NULL,
    interval_grid = c(6, 8, 12, 24, 48, 72),
    preferred_interval = 24,
    attainment_tolerance = 0.05,
    settings = NULL,
    ...
) {

  ## Input validation
  if (is.null(interval_grid) || !is.numeric(interval_grid) || length(interval_grid) < 1) {
    cli::cli_abort("`interval_grid` must be a non-NULL numeric vector with at least one element.")
  }
  if (!is.numeric(preferred_interval) || length(preferred_interval) != 1) {
    cli::cli_abort("`preferred_interval` must be a single numeric value.")
  }
  if (!is.numeric(attainment_tolerance) || length(attainment_tolerance) != 1 ||
      attainment_tolerance < 0) {
    cli::cli_abort("`attainment_tolerance` must be a single non-negative numeric value.")
  }

  ## MAP fitting (identical preamble to map_adjust_dose)
  fit <- simulate_fit(
    est_model = est_model,
    parameters = parameters,
    omega = omega,
    ruv = ruv,
    tdms = tdms,
    covariates = covariates,
    regimen = regimen
  )
  if (!all(c("parameters", "pred", "ipred", "dv", "weights") %in% names(fit))) {
    cli::cli_warn("Fit error, please investigate. Continuing but using population estimates.")
    est_par <- parameters
    gof <- data.frame(pred = NA, ipred = NA, dv = NA, weights = NA)
  } else {
    est_par <- fit$parameters
    gof <- data.frame(
      pred = fit$pred, ipred = fit$ipred,
      dv = fit$dv, weights = fit$weights
    )
  }

  ## Auto-generate dose grid if NULL
  if (is.null(grid)) {
    d1 <- regimen$dose_amts[1]
    grid <- seq(d1 / 5, d1 * 5, length.out = 10)
  }

  ## Determine obs compartment
  auc_comp <- PKPDsim::get_model_auc_compartment(est_model)
  if (target_design$type %in% c(target_types_conc, target_types_time)) {
    obs <- "obs"
  } else if (target_design$type %in% target_types_auc) {
    if (is.null(auc_comp)) {
      cli::cli_abort("AUC compartment not specified in model.")
    }
    obs <- auc_comp
  } else {
    cli::cli_abort("Target type not recognized.")
  }

  iov_bins <- PKPDsim::get_model_iov(est_model)$bins

  ## Loop over candidate intervals
  results <- data.frame(
    interval = numeric(0),
    dose = numeric(0),
    achieved = numeric(0),
    distance = numeric(0)
  )

  for (candidate_interval in interval_grid) {
    ## Update regimen with candidate interval
    test_reg <- update_regimen(
      regimen,
      new_interval = candidate_interval,
      dose_update_number = dose_update
    )

    ## Find best dose for this interval
    best_dose <- dose_grid_search(
      est_model = est_model,
      regimen = test_reg,
      parameters = est_par,
      target_design = target_design,
      auc_comp = auc_comp,
      dose_update = dose_update,
      grid = grid,
      grid_type = "dose",
      covariates = covariates,
      iov_bins = iov_bins,
      dose_resolution = settings$dose_resolution,
      max_dose = settings$max_dose,
      min_dose = settings$min_dose,
      ...
    )

    ## Evaluate achieved metric for this (dose, interval) pair
    achieved <- simulate_dose_interval(
      value = best_dose,
      grid_type = "dose",
      dose_update = dose_update,
      regimen = test_reg,
      parameters = est_par,
      covariates = covariates,
      md = list(),
      pta = NULL,
      target_design = target_design,
      model = est_model,
      omega = omega,
      obs = obs,
      ruv = ruv,
      ...
    )

    distance <- abs(achieved - target_design$value)
    results <- rbind(results, data.frame(
      interval = candidate_interval,
      dose = best_dose,
      achieved = achieved,
      distance = distance
    ))
  }

  ## Selection with tie-breaking
  ## Tolerance band: within attainment_tolerance * abs(target_value) of best
  best_distance <- min(results$distance)
  tolerance_band <- attainment_tolerance * abs(target_design$value)
  in_band <- results$distance <= best_distance + tolerance_band

  candidates <- results[in_band, , drop = FALSE]

  ## Among candidates in band, pick interval closest to preferred_interval
  candidates$interval_distance <- abs(candidates$interval - preferred_interval)
  candidates <- candidates[order(candidates$interval_distance, candidates$distance), ]
  selected <- candidates[1, ]

  ## Update regimen with selected dose and interval
  regimen <- update_regimen(
    regimen,
    new_interval = selected$interval,
    dose_update_number = dose_update
  )
  regimen <- update_regimen(
    regimen,
    new_dose = selected$dose,
    dose_update_number = dose_update
  )

  list(
    regimen = regimen,
    dose_update = dose_update,
    new_dose = selected$dose,
    new_interval = selected$interval,
    additional_info = est_par,
    gof = gof
  )
}
