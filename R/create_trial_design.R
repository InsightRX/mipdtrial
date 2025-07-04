#' Combine all sub-designs into the overall trial design object
#'
#' @param file a YAML file with all subdesigns
#' @param sampling_design Design for sampling, from `create_sampling_design()`
#' @param target_design Design for target attainment, from
#' `create_target_design()`
#' @param initial_regimen_design Design for initial regimen, from
#' `create_initial_regimen_design()`
#' @param regimen_update_design Design for dose/regimen optimization, from
#' `create_regimen_update_design()`
#' @param sim_design Design for simulation model, from `create_model_design()`
#' @param est_design Design for estimation, from `create_model_design()`
#' @param eval_design Design for evaluation metrics, from `create_eval_design()`
#'
#' @export
#' 
create_trial_design <- function(
    file = NULL,
    sampling_design = NULL,
    target_design = NULL,
    initial_regimen_design = NULL,
    regimen_update_design = NULL,
    sim_design = NULL,
    est_design = NULL,
    eval_design = NULL
) {
  if(!is.null(file)) {
    design <- parse_spec_file_to_trial_design(file)
  } else {
    design <- list(
      sampling = sampling_design,
      target = target_design,
      initial_regimen = initial_regimen_design,
      regimen_update = regimen_update_design,
      sim = sim_design,
      est = est_design,
      evaluation = eval_design
    )
  }

  ## Design checks / parsing before returning
  check_trial_design(design)
}

#' Parse YAML spec file to trial design
#'
#' @param file full filepath to yaml file
parse_spec_file_to_trial_design <- function(file) {
  md <- yaml::read_yaml(file)
  design <- list()
  for(idx in names(md$designs)) {
    if(idx %in% c("sim", "est")) {
      f <- paste0("create_model_design")
    } else {
      f <- paste0("create_", idx, "_design")
    }
    design[[idx]] <- do.call(f, args = md$designs[[idx]])
  }
  missing <- setdiff(c("target", "sampling", "regimen_update", "sim", "est"), names(design))
  if(length(missing) > 0) {
    cli::cli_warn(
      paste(
        "Subdesigns are missing from design specification file: ",
        paste0(missing, collapse = ", ")
      )
    )
  }
  design
}

