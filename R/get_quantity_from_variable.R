#' Get quantities from variables in sim results
#'
#' Used to add quantities to rows in advice table, exposure time table.
#'
#' @param var Name of variable
#' @param sim Output of PKPDsim::sim_ode()
#' @param md Metadata object
#' @param times Times at which to calculate
#' @param comp if not NULL, filter by `sim$comp`
get_quantity_from_variable <- function(var, sim, md, times, comp = NULL) {

  info <- md$prediction$quantity_from_variable[[var]]
  if (!is.null(comp)) {
    sim <- sim[sim$comp == comp, ]
  }
  obs <- sim[round(sim$t,3) %in% round(times, 3),]
  dt <- diff(obs$t)

  if (is.null(obs[[info$variable]])) {
    return(NULL)
  }
  if(!is.null(obs[[info$variable]])) {
    if(info$per_interval) {
      if(info$percentage_time) {
        val <- 100 * diff(obs[[info$variable]]) / diff(obs$t)
        val <- c(rep(0, nrow(obs) - length(val)), val)
      } else {
        if(is.null(info$diff) || info$diff == FALSE) {
          val <- obs[[info$variable]]
        } else {
          val <- c(diff(obs[[info$variable]]))
          val <- c(rep(0, nrow(obs) - length(val)), val)
        }
      }
    } else {
      if(!is.null(info$func)) { # apply a function
        f <- get(info$func)
        val <- f(obs[[info$variable]])
      } else { # or read out at last t_obs
        val <- tail(obs[[info$variable]],1)
      }
    }
    if(!is.null(info$correct_interval)) {
      val <- val * (info$correct_interval / c(rep(1, nrow(obs) - length(dt)), dt))
    }
    if(!is.null(info$factor)) {
      val <- val * info$factor
    }
    if(!is.null(info$rounding)) {
      val <- round(val, info$rounding)
    }
  }

  val
}
