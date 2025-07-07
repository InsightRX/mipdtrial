#' Calculate exposure metrics
#'
#' Calculates drug concentration or area under the curve (AUC) for a MIPD
#' trial run.
#'
#' This family of functions calculates the exposure metric for a given regimen,
#' model, model parameters, and target design. Supply the final regimen and the
#' final parameter estimates to get the final estimated exposure metric. Supply
#' the final regimen and the true individual parameter estimates to get the
#' final true exposure metric.
#'
#' @name exposure_metrics
NULL

#' Get AUC from a simulation
#'
#' @param sim_output output of a `PKPDsim::sim` call
#' @param auc_comp auc compartment
#' @param extract_time time points for AUC12 or 24 to extract
#' @param target_type type of AUC target in `c("auc24", "auc12", "cum_auc")`.
#' @returns `calc_auc_from_sim` returns a numeric vector of AUCs between each
#'   simulated time point. Control time period over which AUC should be
#'   calculated using `target_time` argument to `PKPDsim::sim`.
#' @export

calc_auc_from_sim <- function(sim_output, auc_comp, extract_time, target_type) {
  aucs <- sim_output[sim_output$comp == auc_comp, ]
  if (nrow(aucs) == 1 || grepl("cum_", target_type)){ # cumulative
    aucs$y
  } else if (grepl("auc\\d+", target_type)){ # auc24 or auc12
    aucs$diff <- c(0, diff(aucs$y))
    aucs$diff[aucs$t %in% extract_time]
  }
}

#' Get concentration from a regimen
#'
#' @param regimen PKPDsim regimen object
#' @param parameters use MAP estimation to get estimated exposure, use true
#'   patient parameters to get true exposure. Parameters must correspond to the
#'   model used. Accepts parameters supplied as a data frame row, a named vector
#'   or as a list.
#' @param model model to use for exposure calculations.
#' @param target_design target design, created using `create_target_design()`
#' @param ... arguments passed on to PKPDsim::sim. Typical arguments include
#'   `covariates` or `iov_bins`
#' @returns `calc_concentration_from_regimen` returns a numeric vector of
#' concentrations between each simulated time point. Control when concentration
#' is estimated using `target_time`.
#' @rdname exposure_metrics
#' @export
calc_concentration_from_regimen <- function(
    regimen,
    parameters,
    model,
    target_design,
    ...
){
  if (!all(attr(model, "parameters") %in% names(parameters))) {
    cli::cli_abort("Model/parameter mismatch")
  }
  if (inherits(parameters, "data.frame") || is.atomic(parameters)) {
    parameters <- as.list(parameters)
  }

  iov <- PKPDsim::get_model_iov(model)

  target_time <- get_sampling_times_from_scheme(
    target_design$scheme,
    regimen
  )
  sim_output <- PKPDsim::sim(
    model,
    parameters = parameters,
    regimen = regimen,
    t_obs = target_time,
    iov_bins = iov[["bins"]],
    ...
  )
  sim_output[sim_output$comp == "obs",]$y
}

#' Get AUC from a regimen
#'
#' @returns `calc_auc_from_regimen` returns a numeric vector of AUCs between
#'   each simulated time point. Control time period over which AUC should be
#'   calculated using `target_time`.
#'
#' @rdname exposure_metrics
#' @export

calc_auc_from_regimen <- function(
  regimen,
  parameters,
  model,
  target_design,
  ...
){
  if (!all(attr(model, "parameters") %in% names(parameters))) {
    cli::cli_abort("Model/parameter mismatch")
  }
  if (inherits(parameters, "data.frame") || is.atomic(parameters)) {
    parameters <- as.list(parameters)
  }

  iov <- PKPDsim::get_model_iov(model)

  target_time_original <- get_sampling_times_from_scheme(
    target_design$scheme,
    regimen
  )
  target_time_sim <- target_time_original
  if(target_design$type == "auc24") {
    target_time_sim <- c(target_time_original - 24, target_time_original)
  } else if (target_design$type == "auc12") {
    target_time_sim <- c(target_time_original - 12, target_time_original)
  }
  sim_output <- PKPDsim::sim(
    model,
    parameters = parameters,
    regimen = regimen,
    t_obs = unique(target_time_sim),
    iov_bins = iov[["bins"]],
    ...
  )
  calc_auc_from_sim(
    sim_output,
    attr(model, "size"),
    target_time_original,
    target_design$type
  )
}

#' Calculate time to target attainment
#'
#' For AUC targets, this function returns the first time of the start of
#' the dosing interval plus the infusion length where the AUC24 or 12 is
#' on target.
#'
#' For trough targets, this returns the time of start of the dosing interval
#' plus the infusion length immediately prior to the trough being on target
#' (i.e. the dose + infusion length of the first dose that brought the trough
#' to target).
#'
#' @param regimen PKPDsim regimen object
#' @param target_design target design, created using `create_target_design()`
#' @param auc_comp compartment to look for AUC
#' @param model PKPDsim model object
#' @param ... arguments passed on to `PKPDsim::sim`
#' @return a numeric value indicating the time, in hours, if a dosing interval
#'   contained the target metric within range. Returns Inf if target was never
#'   achieved. Returns NA if the target type is not supported.
#' @export
calc_time_to_target <- function(
    regimen,
    target_design,
    auc_comp,
    model,
    ...
) {
  target_type <- match.arg(tolower(target_design$type), mipd_target_types())

  supported_targets <- c("auc24", "auc12", "trough", "cmin")
  if (!(target_type %in% supported_targets)){
    return(NA_real_)
  }

  iov <- PKPDsim::get_model_iov(model)

  # Which dose resulted in an exposure metric on-target?
  dose_idx <- NULL
  if (grepl("auc\\d+", target_type)){ # auc24 or auc12
    auc_num <- as.numeric(gsub(".*auc(\\d+).*", "\\1", target_type, ignore.case = FALSE))
    terminal_interval <- auc_num
    t_obs_target <- c(regimen$dose_times, tail(regimen$dose_times, 1) + terminal_interval)
    sim_target <- PKPDsim::sim(
      model,
      regimen = regimen,
      only_obs = FALSE,
      t_obs = t_obs_target,
      iov_bins = iov[["bins"]],
      ...
    )

    aucs_target <- sim_target$y[sim_target$comp == auc_comp]
    auc_target_X <- auc_num * (diff(aucs_target) / diff(t_obs_target))
    aucs_on_target <- which(
      auc_target_X >= target_design$min & auc_target_X <= target_design$max
    )
    if (length(aucs_on_target) > 0) {
      # return time of *start* of dosing interval that gives target
      # plus infusion length
      dose_idx <- min(aucs_on_target)
    }
  } else if (target_type %in% c("trough", "cmin")){
    terminal_interval <- regimen$dose_times[length(regimen$dose_times)] - regimen$dose_times[length(regimen$dose_times)-1]
    t_obs_target <- c(regimen$dose_times, tail(regimen$dose_times, 1) + terminal_interval)
    sim_target <- PKPDsim::sim(
      model,
      regimen = regimen,
      only_obs = FALSE,
      t_obs = t_obs_target,
      iov_bins = iov[["bins"]],
      ...
    )
    cmin_target <- sim_target$y[sim_target$comp == "obs"]
    cmins_on_target <- which(
      cmin_target >= target_design$min & cmin_target <= target_design$max
    )
    if (length(cmins_on_target) > 0) {
      # return time of dose prior to trough in target (plus inf leng)
      # i.e., the obs time before the first trough on target, or the
      # first dose.
      dose_idx <- pmax(min(cmins_on_target)-1,1)
    }
  }

  # Return end of infusion of identified dose, or Inf if never on target
  if (!is.null(dose_idx)) {
    regimen$dose_times[dose_idx] + regimen$t_inf[dose_idx]
  } else {
    Inf
  }
}
