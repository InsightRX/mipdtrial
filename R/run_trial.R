#' Run an MIPD trial
#'
#' This function is a wrapper function to make it easier to run a basic MIPD
#' trial with minimal function calls. For fine-tune control over logic, it may
#' be easier to compose each step of the precision-dosing workflow yourself. For
#' an example, see the "Busulfan MIPD" vignette.
#'
#' This function performs the following steps:
#'
#' 1. Dose the patient using the function specified in `initial_dose_method`.
#' 2. Collect levels at the specified times (`tdm_times`).
#' 3. Adjust dosing at each specified dose (`adjust_doses_at`) using the logic
#'    specified in `dose_optimization_method`
#' 4. Return an object with data collected during and at the end of the MIPD
#'    trial.
#'
#' @param data data set to use for simulation, a data frame. Should contain
#'   covariates in columns, corresponding to values in `cov_mapping` and a
#'   column for `ID`, as a patient identifier.
#' @param design trial design, created using `create_trial_design()`
#' @param cov_mapping a named vector indicating correspondence between model
#'   covariates and data frame columns. Vector names should reflect all
#'   covariates defined by the model, and vector values should represent the
#'   associated column in the provided data set.
#' @param seed argument passed to `set.seed()` to set the seed for all random
#'   variability generation.
#' @param progress show progress bar? (default is `TRUE`)
#' @param n_ids number of subjects to use in simulated trial. If not specified,
#' will use all subjects in `data`.
#' @param verbose verbose output?
#' @param threads number of threads to run the simulations on. By default the
#' simulations will run on 4 cores, or less if less are available (one core will
#' always be reserved). The user can override by specifying manually.

#' @export
#'
run_trial <- function(
    data,
    design,
    cov_mapping,
    n_ids = NULL,
    seed = 0,
    verbose = FALSE,
    progress = TRUE,
    threads = 1
) {

  ## Determine number of threads
  n_cores <- max(c(1, parallel::detectCores() - 1))
  if(threads > (n_cores-1)) {
    cli::cli_alert_warning("Number of threads ({threads}) cannot be equal to or higher than the number of cores available ({n_cores}). Reducing to {n_cores-1}")
    threads <- n_cores - 1
  }
  cli::cli_alert_info("Starting simulations in {threads} threads")

  ## Set up data collectors
  tdms <- data.frame()
  dose_updates <- data.frame()
  additional_info <- list() # keep generic for other methods
  gof <- data.frame()
  final_exposure <- data.frame()
  eval_exposure <- data.frame()

  ## Set up number of individuals to simulate
  if(is.null(n_ids)) {
    n_ids <- nrow(data)
  } else {
    if(n_ids <= nrow(data)) {
      data <- data[1:n_ids,]
    } else {
      cli::cli_abort("`n_ids` cannot be larger than number of subjects in dataset.")
    }
  }

  ## Draw individual parameters up front
  if("ID" %in% names(data)) { # ensure we have a lower case `id`
    data <- data |>
      dplyr::rename(id = ID)
  }
  if(any(duplicated(data$id)))
    cli::cli_abort("Input dataset cannot have duplicate `id`.")
  sim_ids <- data$id
  all_pars <- generate_iiv(
    sim_model  = design$sim$model,
    omega      = design$sim$omega_matrix,
    parameters = design$sim$parameters,
    n_iter = length(sim_ids)
  ) |>
    dplyr::mutate(id = sim_ids)

  ## Set up progress bars
  ## Need to loop this through the progressr package
  ## because furrr currently doesn't support progressbars with cli.
  if(progress) {
    progressr::handlers(global = TRUE)
    progressr::handlers("cli")
    p <- progressr::progressor(along = sim_ids)
  } else {
    p <- function() { }
  }

  ## Main loop
  f <- function(i) {
    p()
    sim_subject(
      data = data[i, ],
      cov_mapping = cov_mapping,
      pars_true_i = all_pars[i, ] |>
        dplyr::select(-id, -iteration) |>
        as.list(),
      design = design,
      verbose = verbose
    )
  }
  if(threads > 1) {
    future::plan(future::multisession, workers = threads)
    res <- furrr:::future_map(
      sim_ids,
      .f = f,
      .options = furrr::furrr_options(seed = seed)
    )
  } else {
    res <- purrr::map(
      sim_ids,
      .f = f
    )
  }

  ## Return simulation results as list of data.frames
  cli::cli_alert_info("Post-processing results")
  bind_sim_output(res)

}
