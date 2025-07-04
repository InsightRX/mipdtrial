#' Bind simulated subject-level output together
#'
#' Binds data from list containing the individual simulations into a
#' data.frames that summarises the trial results).
#'
#' @param res temporary result object (list) in `run_trial()`
#'
#' @returns list with various data.frames with summaries of the trial
#'
bind_sim_output <- function(res) {
  out <- list()
  elements <- names(res[[1]])
  for(el in elements) {
    if(!is.null(res[[1]][[el]])) {
      if(inherits(res[[1]][[el]], "data.frame")) {
        out[[el]] <- dplyr::bind_rows(lapply(res, function(x) x[[el]]))
      } else {
        out[[el]] <- c(lapply(res, function(x) x[[el]]))
      }
    }
  }
  class(out) <- c("mipdtrial_results", "list")
  out
}
