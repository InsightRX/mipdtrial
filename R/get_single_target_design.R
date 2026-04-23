#' Get a single target from a potentially time-varying target design
#' 
#' In most cases, the target design will just specify a single target.
#' But in some scenarios, a time-varying target may make sense. In that 
#' case, for each regimen update instance, a different target can be 
#' specified. 
#' This function just grabs the right target values from the overall
#' target design. If just a single target is specified, it will just
#' return the target design as-is.
#' 
#' @param target_design target design, created using `create_target_design`.
#' @param idx number of regimen update. By default it will take the last
#' instance.
#' 
get_single_target_design <- function(
  target_design,
  idx = nrow(target_design$scheme)
) {
  if(nrow(target_design$scheme) == 1) { ## most common case, just a single target
    return(target_design)
  }
  if(idx > nrow(target_design$scheme) | idx > length(target_design$value) ) {
    cli::cli_abort("Not enough target values specified to support regimen updates. Please check target and regimen update designs.")
  }
  tmp <- target_design
  tmp$scheme <- tmp$scheme[idx,]
  tmp$value <- tmp$value[idx]
  tmp$min <- tmp$min[idx]
  tmp$max <- tmp$max[idx]
  tmp
  
}