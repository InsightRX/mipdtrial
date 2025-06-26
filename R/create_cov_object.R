#' Create a list of PKPDsim covariates for modeling
#'
#' Data sets typically come in rectangular formats while for MIPD trials we
#' model each individual separately. PKPDsim expects covariates to be provided
#' as a list of PKPDsim covariates. This function helps convert rectangular
#' (wide) data to model-ready covariates.
#'
#' The data is assumed to already be numeric (e.g., categorical covariates like
#' smoking status have already been mapped to 1 or 0).
#'
#' The function returns NULL if required columns are missing or NA.
#'
#' @param dat_i a rectangular data set, containing only rows for an individual
#' @param mapping named vector indicating which columns to extract, and what
#'   the covariate names are expected in the model. Names should be model
#'   covariates and values should be data frame column names.
#' @param implementation named vector indicating covariate implementation, one
#'   of `c("interpolate", "locf")`. If NULL, assumes "interpolate" for all
#'   covariates. See `PKPDsim::new_covariate` for details. Names should be model
#'   covariates and values should be implementation method.
#' @param time_column if covariates are time-varying, supply a column for time.
#'   Otherwise, initial value only is taken for all covariates.
#' @returns Returns a named list of PKPDsim covariates.
#' @export
#' @examples
#' dat <- data.frame(ID = 1, weight = 70, CRCL = 4)
#' mapping <- c(WT = "weight", CRCL = "CRCL")
#' create_cov_object(dat, mapping)

create_cov_object <- function(
  dat_i,
  mapping,
  implementation = NULL,
  time_column = NULL
) {
  if(inherits(mapping, "list")) {
    mapping <- unlist(mapping)
  }
  if (!all(mapping %in% colnames(dat_i)) || any(is.na(dat_i[mapping]))) {
    missing_cov <- setdiff(mapping, colnames(dat_i))
    non_missing <- setdiff(
      names(mapping),
      names(mapping[which(mapping %in% missing_cov)])
    )
    mapping <- mapping[non_missing]
    missing_cov <- c(missing_cov, mapping[is.na(dat_i[mapping])])
    cli::cli_alert_warning(paste0(
      "Missing covariates: ",
      paste0(missing_cov, collapse = ", ")
    ))
    return(NULL)
  }

  if (is.null(time_column)) {
    dat_i <- dat_i[1,]
    dat_i$t <- 0
  } else {
    if (!time_column %in% colnames(dat_i)) {
      cli::cli_alert_warning(
        paste0("Missing time column: ", time_column))
      return(NULL)
    }
    dat_i$t <- dat_i[[time_column]]
  }

  if (is.null(implementation)) {
    implementation <- setNames(
      rep("interpolate", length(mapping)), names(mapping)
    )
  }

  covs <- list()
  for (key in names(mapping)) {
    covs[[key]] <- PKPDsim::new_covariate(
      value = dat_i[[mapping[[key]]]],
      times = dat_i$t,
      implementation = implementation[[key]],
      verbose = FALSE
    )
  }
  covs
}
