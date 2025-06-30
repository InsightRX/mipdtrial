#' Perform a grid search for a particular target by simulating a grid of doses
#'
#' Set refine = TRUE if the model is nonlinear so that the grid search happens
#' iteratively.
#'
#' @param est_model model used for estimation ("clinician facing")
#' @param regimen PKPDsim regimen object
#' @param target_design object specifying target design, created using function
#' `create_target_design()`
#' @param auc_comp auc compartment (starting from 1, R-style not C-style!)
#' @param pta probability of target attainment, list with arguments `type` and
#'   `value`, also requires `omega` if non-NULL. If `NULL`, will just aim for
#'   specific conc or auc.
#' @param omega IIV matrix, for estimation model, for probability of target
#'   attainment target types.
#' @param ruv list specifying residual error for estimation model:
#'   `list(prop = 0.1, add = 1.5)`, for probability of target attainment target
#'   types.
#' @param dose_update update dose from which dose?
#' @param grid vector specifying doses or intervals to use as test grid,
#' Example: `seq(from = 50, to = 500, by = (500 - 50) / 10)`
#' @param grid_type either "dose" or "interval"
#' @param dose_resolution to which precision should the output be rounded
#'   (e.g. 50), useful when in practice only a specific set of dose units.
#'   Can of course also be controlled by altering the grid.
#' @param refine should the found optimal dose be refined more? If not
#'   specified, will refine if the model linearity (in `attr(model, "misc")`) is
#'   not described as `"linear"`
#' @param refine_range after initial optimization, should a second refinement
#'   step be implemented? If `refine_range` is specified e.g. as `c(0.9, 1.1)`
#'   then it will implement a second optimization using a grid spanning from 90%
#'   to 110% of the initial optimal dose. Useful only for non-linear models.
#' @param check_boundaries if optimal dose is at lower/upper boundary of grid,
#' should grid be expanded?
#' @param max_dose maximum dose cap
#' @param min_dose minimum dose cap
#' @param n_cores Number of cores over which to simulate doses
#' @param md metadata object (only needed if we have to use
#'   `get_quantity_from_variable()` to generate target value)
#' @param parameters list of parameters
#' @param covariates covariates object
#' @param verbose verbose output?
#' @param ... passed on to PKPDsim function
#' @returns A numeric value indicating the recommended dose
#' @export
#'
dose_grid_search <- function(
    est_model = NULL,
    regimen,
    parameters,
    target_design = create_target_design(
      targettype = "conc",
      targetvalue = 10,
      time = 24
    ),
    auc_comp = NULL,
    pta = NULL,
    omega = NULL,
    ruv = NULL,
    dose_update = 1,
    grid = seq(1, 6000, by = 10),
    grid_type = "dose",
    dose_resolution = 1,
    refine = NULL,
    refine_range = c(0.7, 1.4),
    check_boundaries = TRUE,
    max_dose = NULL,
    min_dose = NULL,
    n_cores = 1,
    md = list(),
    covariates = NULL,
    verbose = FALSE,
    ...
) {

  if(target_design$type %in% target_types_time) { #
    if(min(target_design$range) >= 100) {
      target_design$variable <- ifelse(
        grepl("_free", target_design$type),
        "CONCF",
        "CONC"
      )
      target_design$type <- "cmin"
      target_design$value <- min(target_design$range)/100 * tail(covariates$MIC$value, 1)
      target_design$range <- rep(target_design$value, 2)
    }
  }

  if(target_design$type %in% c(target_types_conc, target_types_time)) {
    obs <- "obs"
  } else if(target_design$type %in% target_types_auc) {
    if(is.null(auc_comp)) {
      cli::cli_abort("AUC compartment not specified")
    }
    obs <- auc_comp
  } else {
    cli::cli_abort("Target type not recognized!")
  }

  if (target_design$type == "auc" & !is.null(pta)) {
    cli::cli_abort("PTA method for AUC currently not supported.")
  }
  if (!is.null(pta)) {
    if(is.null(omega)) {
      cli::cli_abort("PTA method requires specification of omega!")
    }
  }
  if (is.null(grid) || any(is.na(grid)) || length(grid) < 2) {
    cli::cli_abort("Must supply grid search space in `grid`")
  }
  if(is.null(target_design$value) || length(target_design$value) == 0) {
    cli::cli_abort("Target not specified!")
  }
  if (is.null(refine)){
    # unless specified otherwise, do not refine if model is linear
    refine <- !isTRUE(attr(est_model, "misc")$linearity == "linear")
    # time-based target methods also need to be refined since this target
    # is non-linear
    refine <- target_design$type %in% target_types_time || refine
  }

  y <- mclapply(
    grid,
    simulate_dose_interval,
    grid_type = grid_type,
    dose_update = dose_update,
    regimen = regimen,
    md = md,
    pta = pta,
    target_design = target_design,
    model = est_model,
    parameters = parameters,
    covariates = covariates,
    omega = omega,
    obs = obs,
    ruv = ruv,
    mc.cores = n_cores,
    ...
  )

  if(grid_type == "interval") {
    tab <- data.frame(interval = grid, y = unlist(y))
    if (target_design$type %in% target_types_time) {
      tab <- filter_rows_0_100(tab)
    }
    interval <- tab[which.min(abs(tab$y - target_design$value)),]$interval
    return(interval)
  } else {
    tab <- data.frame(
      dose = grid,
      y = unlist(y)
    )
  }

  ## get best dose:
  if (target_design$type %in% target_types_time) {
    tab <- filter_rows_0_100(tab)
  }

  # Get two closest doses above and below target if possible, otherwise get
  # two closest doses (even if both are above or below)
  if (any(tab$y < target_design$value) && any(tab$y >= target_design$value)) {
    closest_below <- max(order(tab$y - target_design$value)[tab$y - target_design$value < 0])
    closest_above <- min(order(tab$y - target_design$value)[tab$y - target_design$value >= 0])
    tmp <- tab[c(closest_below, closest_above),]
  } else {
    tmp <- tab[order(abs(tab$y - target_design$value)),][1:2,]
  }
  fit <- lm(dose ~ y, data.frame(tmp))
  dose <- as.numeric(predict(fit, list(y=target_design$value)))

  if(check_boundaries) { # if at upper or lower boundary, then take a different range
    if(dose == grid[1] && dose > min_dose) {
      grid <- grid / 4
    } else {
      if(dose == tail(grid,1) && dose < max_dose) {
        grid <- grid * 4
      }
    }
    if(dose == grid[1] || dose == tail(grid,1)) {
      dose <- dose_grid_search(
        est_model = est_model,
        regimen = regimen,
        target_design = target_design,
        auc_comp = auc_comp,
        pta = pta,
        omega = omega, ruv = ruv,
        dose_update = dose_update,
        grid = grid,
        grid_type = "dose",
        dose_resolution = NULL,
        refine = refine,
        check_boundaries = FALSE, # !!
        md = md,
        parameters = parameters,
        covariates = covariates,
        ...
      )
    }
  }
  if(refine) { # do a finer grid around the first one
    dose_grid <- seq(
      dose * refine_range[1],
      dose * refine_range[2],
      length.out = 10
    )
    dose <- dose_grid_search(
      est_model = est_model,
      regimen = regimen,
      target_design = target_design,
      auc_comp = auc_comp,
      pta = pta,
      omega = omega,
      ruv = ruv,
      dose_update = dose_update,
      grid = dose_grid, # updated under refined range
      grid_type = "dose",
      dose_resolution = NULL, # will round again below
      refine = FALSE, # !!
      check_boundaries = FALSE,
      md = md,
      parameters = parameters,
      covariates = covariates,
      ...
    )
  }
  if(dose < 0) dose <- 0
  if(!is.null(dose_resolution)) {
    dose <- round_to_multiple(dose, dose_resolution)
  }
  dose_over_max <- !is.null(max_dose) && (dose > max_dose)
  dose_under_min <- !is.null(min_dose) && (dose < min_dose)
  if (dose_over_max || dose_under_min) {
    if (dose_over_max) dose <- max_dose
    if (dose_under_min)  dose <- min_dose
  }

  return(dose)
}

#' Simulate different doses/intervals in a dose/interval grid
#'
#' @inheritParams dose_grid_search
#' @param model model for simulating dose (estimation model)
#' @param parameters list of parameters
#' @param value element of the dose/interval grid
#' @param grid_type either `dose` grid or `interval` grid
#' @param obs Value of `obs` as determined by [dose_grid_search()] (i.e. either "obs" or AUC compartment)
#' @md
simulate_dose_interval <- function(
    value,
    grid_type = "dose",
    dose_update,
    regimen,
    md,
    pta,
    target_design,
    model,
    parameters,
    covariates,
    omega,
    obs,
    ruv,
    ...
) {
  reg <- regimen
  if(grid_type == "dose") {
    reg <- update_regimen(
      regimen = reg,
      new_dose = value,
      dose_update_number = dose_update
    )
  } else if(grid_type == "interval") {
    reg <- update_regimen(
      regimen = reg,
      new_interval = value,
      dose_update_number = dose_update
    )
  }

  ## Calculate time at which target needs to be checked
  ## This is constant for dose-optimization, but differs for interval-based
  ## optimization.
  t_obs <- get_sampling_times_from_scheme(
    scheme = target_design$scheme,
    regimen = reg
  )
  if(length(t_obs) > 1 && !target_design$type %in% c("auc", target_types_time)) {
    t_obs <- t_obs[1]
  }
  if(target_design$type %in% c("auc", target_types_time)) {
    if(length(t_obs) != 2) {
      cli::cli_abort("Need a vector of length 2 for observation times when target type is `auc`.")
    }
  }

  if (target_design$type %in% target_types_time || target_design$type == "auc") {
    # need two time points for time-based targets
    t_obs <- c(t_obs - regimen$interval, t_obs)
  } else if (target_design$type == "auc24") {
    # need 24 hours of dosing
    t_obs <- c(t_obs - 24, t_obs)
  } else if (target_design$type == "auc12") {
    # need 12 hours of dosing
    t_obs <- c(t_obs - 12, t_obs)
  }

  tmp <- PKPDsim::sim(
    model,
    regimen = reg,
    parameters = parameters,
    covariates = covariates,
    t_obs = t_obs,
    output_include = list(variables=TRUE),
    only_obs = FALSE,
    checks = FALSE,
    ...
  )
  if (is.null(pta)) {
    if (target_design$type %in% c("auc", "auc24", "auc12")) {
      if(!is.null(target_design$variable)) {
        return(diff(tmp[[target_design$variable]][tmp$comp == obs]))
      } else {
        return(diff(tmp[tmp$comp == obs, ]$y))
      }
    } else if (target_design$type %in% target_types_time) {
      return(
        tail(
          get_quantity_from_variable(
            var = target_design$type,
            sim = tmp,
            md = md,
            times = t_obs,
            comp = "obs"
          ),
          1
        )
      )
    } else {
      if(!is.null(target_design$variable)) {
        return(tmp[[target_design$variable]][tmp$comp == obs])
      } else {
        return(tmp[["y"]][tmp$comp == obs])
      }
    }
  } else {
    sd <- PKPDsim::get_var_y(model,
                             parameters = parameters,
                             covariates = covariates,
                             regimen = reg,
                             t_obs = t_obs,
                             omega = omega,
                             obs_comp = obs,
                             ruv = ruv,
                             ...
    )$regular
    if (is.null(pta$type)) {
      pta$type <- "gt"
    }
    if (pta$type == "gt") {
      conc_bnd <- qnorm(1 - pta$prob, tmp[tmp$comp == obs, ]$y, sd) # target level
    } else {
      conc_bnd <- qnorm(pta$prob, tmp[tmp$comp == obs, ]$y, sd) # target level
    }
    return(conc_bnd)
  }
}
