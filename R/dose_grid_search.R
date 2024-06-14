#' Perform a grid search for a particular target by simulating a grid of doses
#'
#' Set refine = TRUE if the model is nonlinear so that the grid search happens
#' iteratively.
#'
#' @param est_model model used for estimation ("clinician facing")
#' @param regimen PKPDsim regimen object
#' @param target list with arguments `type`, and `value`. `type` = observation
#'   type, either "conc" or auc".
#' @param target_time vector of observation times at which to evaluate the
#'   target exposure.
#' @param obs_comp auc compartment (starting from 1, R-style not C-style!)
#' @param pta probability of target attainment, list with arguments `type` and
#'   `value`, also requires `omega` if non-NULL. If `NULL`, will just aim for
#'   specific conc or auc.
#' @param omega IIV matrix, for estimation model, for probability of target
#'   attainment target types.
#' @param ruv list specifying residual error for estimation model:
#'   `list(prop = 0.1, add = 1.5)`, for probability of target attainment target
#'   types.
#' @param dose_update update dose from which dose?
#' @param dose_grid vector specifying doses to use as test grid, Example:
#'   seq(from = 50, to = 500, by = (500 - 50) / 10 )
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
#' @param covariates covariates object
#' @param ... passed on to PKPDsim function
#' @returns A numeric value indicating the recommended dose
#' @export
#'
dose_grid_search <- function(
    est_model = NULL,
    regimen,
    target_time = 24,
    target = list(
      type = "conc",
      value = 10
    ),
    obs_comp = NULL,
    pta = NULL,
    omega = NULL,
    ruv = NULL,
    dose_update = 1,
    dose_grid = seq(1, 6000, by = 10),
    dose_resolution = 1,
    refine = NULL,
    refine_range = c(0.7, 1.4),
    check_boundaries = TRUE,
    max_dose = NULL,
    min_dose = NULL,
    n_cores = 1,
    md = list(),
    covariates = NULL,
    ...
) {

  if(target$type %in% target_types_time) { #
    if(min(target$range) >= 100) {
      target$variable <- ifelse(
        grepl("_free", target$type),
        "CONCF",
        "CONC"
      )
      target$type <- "cmin"
      target$value <- min(target$range)/100 * tail(covariates$MIC$value, 1)
      target$range <- rep(target$value, 2)
    }
  }

  if(length(target_time) > 1 && !target$type %in% c("auc", target_types_time)) {
    target_time <- target_time[1]
  }

  if(target$type %in% c(target_types_conc, target_types_time)) {
    obs <- "obs"
  } else if(target$type %in% target_types_auc) {
    if(is.null(obs_comp)) {
      stop("AUC compartment not specified")
    }
    obs <- obs_comp
  } else {
    stop("Target type not recognized!")
  }

  if(target$type %in% c("auc", target_types_time)) {
    if(length(target_time) != 2) {
      stop("Need start and end of observation interval as vector target_time.")
    }
  }
  if (target$type == "auc" & !is.null(pta)) {
    stop("PTA method for AUC currently not supported.")
  }
  if (!is.null(pta)) {
    if(is.null(omega)) {
      stop("PTA method requires specification of omega!")
    }
  }
  if (is.null(dose_grid) || any(is.na(dose_grid)) || length(dose_grid) < 2) {
    stop("Must supply grid search space in `dose_grid`")
  }
  if(is.null(target$value) || length(target$value) == 0) {
    stop("Target not specified!")
  }
  if (is.null(refine)){
    # unless specified otherwise, do not refine if model is linear
    refine <- !isTRUE(attr(est_model, "misc")$linearity == "linear")
    # time-based target methods also need to be refined since this target
    # is non-linear
    refine <- target$type %in% target_types_time || refine
  }

  y <- mclapply(
    dose_grid,
    simulate_dose,
    dose_update = dose_update,
    regimen = regimen,
    md = md,
    pta = pta,
    target = target,
    model = est_model,
    t_obs = target_time,
    omega = omega,
    obs = obs,
    ruv = ruv,
    mc.cores = n_cores,
    covariates = covariates,
    ...
  )
  tab <- data.frame(dose = dose_grid, y = unlist(y))

  ## get best dose:
  if (target$type %in% target_types_time) {
    tab <- filter_rows_0_100(tab)
  }

  # Get two closest doses above and below target if possible, otherwise get
  # two closest doses (even if both are above or below)
  if (any(tab$y < target$value) && any(tab$y >= target$value)) {
    closest_below <- max(order(tab$y - target$value)[tab$y - target$value < 0])
    closest_above <- min(order(tab$y - target$value)[tab$y - target$value >= 0])
    tmp <- tab[c(closest_below, closest_above),]
  } else {
    tmp <- tab[order(abs(tab$y - target$value)),][1:2,]
  }

  fit <- lm(dose ~ y, data.frame(tmp))
  dose <- as.numeric(predict(fit, list(y=target$value)))
  fit_reverse <- lm(y ~ dose, data.frame(tmp))

  if(check_boundaries) { # if at upper or lower boundary, then take a different range
    if(dose == dose_grid[1] && dose > min_dose) {
      dose_grid <- dose_grid / 4
    } else {
      if(dose == tail(dose_grid,1) && dose < max_dose) {
        dose_grid <- dose_grid * 4
      }
    }
    if(dose == dose_grid[1] || dose == tail(dose_grid,1)) {
      dose <- dose_grid_search(
        est_model = est_model,
        regimen = regimen,
        target_time = target_time,
        target = target,
        obs_comp = obs_comp,
        pta = pta,
        omega = omega, ruv = ruv,
        dose_update = dose_update,
        dose_grid = dose_grid,
        dose_resolution = NULL,
        refine = refine,
        check_boundaries = FALSE, # !!
        md = md,
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
      target_time = target_time,
      target = target,
      obs_comp = obs_comp,
      pta = pta,
      omega = omega,
      ruv = ruv,
      dose_update = dose_update,
      dose_grid = dose_grid, # updated under refined range
      dose_resolution = NULL, # will round again below
      refine = FALSE, # !!
      check_boundaries = FALSE,
      md = md,
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

#' Simulate dose
#'
#' Simulate a dose in the dose grid
#'
#' @inheritParams dose_grid_search
#' @param model model for simulating dose (estimation model)
#' @param dose_grid element of the dose grid
#' @param t_obs time at which observation should be calculated
#' @param obs Value of `obs` as determined by [dose_grid_search()] (i.e. either "obs" or AUC compartment)
#' @md
simulate_dose <- function(dose_grid,
                          dose_update,
                          regimen,
                          md,
                          pta,
                          target,
                          model,
                          t_obs,
                          omega,
                          obs,
                          ruv,
                          ...) {
  reg <- regimen
  reg$dose_amts[dose_update:length(reg$dose_amts)] <- rep(
    dose_grid,
    length(reg$dose_amts[dose_update:length(reg$dose_amts)])
  )

  if (target$type %in% target_types_time || target$type == "auc") {
    # need two time points for time-based targets
    t_obs <- c(t_obs - regimen$interval, t_obs)
  } else if (target$type == "auc24") {
    # need 24 hours of dosing
    t_obs <- c(t_obs - 24, t_obs)
  }

  tmp <- PKPDsim::sim(
    model,
    regimen = reg,
    t_obs = t_obs,
    output_include = list(variables=TRUE),
    only_obs = FALSE,
    checks = FALSE,
    ...
  )
  if (is.null(pta)) {
    if (target$type %in% c("auc", "auc24")) {
      if(!is.null(target$variable)) {
        return(diff(tmp[[target$variable]][tmp$comp == obs]))
      } else {
        return(diff(tmp[tmp$comp == obs, ]$y))
      }
    } else if (target$type %in% target_types_time) {
      return(
        tail(
          get_quantity_from_variable(
            var = target$type,
            sim = tmp,
            md = md,
            times = t_obs,
            comp = "obs"
          ),
          1
        )
      )
    } else {
      if(!is.null(target$variable)) {
        return(tmp[[target$variable]][tmp$comp == obs])
      } else {
        return(tmp[["y"]][tmp$comp == obs])
      }
    }
  } else {
    sd <- PKPDsim::get_var_y(model,
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


#' Model-based starting dose
#'
#' A light wrapper for [dose_grid_search] for finding model-based starting doses
#' and returning an appropriate regimen object.
#'
#' @param n number of doses
#' @param interval dosing interval
#' @param type dose type, e.g.: "infusion", "oral".
#'   (see [PKPDsim::new_regimen()])
#' @param t_inf infusion time
#' @param ... arguments passed on to `dose_grid_search`
#' @export
#' @returns Returns a PKPDsim regimen populated with the model-predicted dose

model_based_starting_dose <- function(
  n,
  interval,
  type = "infusion",
  t_inf = 1,
  ...
) {
  reg <- PKPDsim::new_regimen(
    amt = 1,
    n = n,
    type = type,
    t_inf = t_inf,
    interval = interval
  )
  starting_dose <- dose_grid_search(
    regimen = reg,
    ...
  )
  reg$dose_amts <- rep(starting_dose, length(reg$dose_amts))
  reg
}
