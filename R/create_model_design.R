#' Create a design for the model, including population and random-effects
#' parameters.
#'
#' @export
create_model_design <- function(
  lib = NULL,
  model = NULL,
  parameters = NULL,
  omega_matrix = NULL,
  ruv = NULL
) {
  design <- list()
  if(!is.null(lib)) {
    suppressMessages( ## avoid message "the following objects are masked from ..."
      require(lib, character.only = TRUE)
    )
    load_lib <- c("model", "parameters", "omega_matrix", "ruv")
    for(i in seq(load_lib)) {
      design[[load_lib[i]]] <- get(load_lib[i], asNamespace(lib))()
    }
  } else {
    design <- list(
      model = model,
      parameters = parameters,
      omega_matrix = omega_matrix,
      ruv = ruv
    )
  }
  design
}
