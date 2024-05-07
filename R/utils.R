#' Round to a multiple of any number (e.g. round to the nearest 5, 10, 100)
#' @param x value to be rounded
#' @param multiple accuracy to round to. If NULL, x will be returned unrounded.
#' @export
#' @examples
#' # Uses "round-to-even" strategy
#' round_to_multiple(12.5, 5)
#' round_to_multiple(17.5, 5)
round_to_multiple <- function(x, multiple) {
  if (is.null(multiple)) {
    return(x)
  }
  round(x / multiple) * multiple
}
