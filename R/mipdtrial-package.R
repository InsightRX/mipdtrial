#' MIPDtrial package
#'
#' Simulation model-informed precision dosing trials
#'
#' @docType package
#' @name mipdtrial-package
#' @author Jasmine Hughes \email{jasmine@@insight-rx.com}
#' @importFrom parallel mclapply detectCores
#' @importFrom PKPDsim new_regimen sim join_regimen new_covariate get_var_y
#' @importFrom PKPDmap get_map_estimates create_iov_object
#' @importFrom stats lm predict qnorm setNames rnorm runif
#' @importFrom utils tail txtProgressBar setTxtProgressBar
#' @importFrom yaml read_yaml

"_PACKAGE"
NULL
