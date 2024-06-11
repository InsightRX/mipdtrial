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

#' Filter rows with values 0 or 100
#'
#' Remove all rows of 0 or 1 if there are at least 2 non-(0,1) rows, and
#' otherwise keep all but the highest 0 row and the lowest 1 row.
#'
#' @param tab data frame with columns `dose` and `y`
#' @details
#' Taken from a proprietary package, written by Kara Woo.
#'
#' @md
filter_rows_0_100 <- function(tab) {
  if (sum(!tab$y %in% c(0, 100)) >= 2) {
    return(tab[!tab$y %in% c(0, 100), ])
  }

  keep <- c(
    suppressWarnings(max(which(tab$y == 0))),
    which(!tab$y %in% c(0, 100)),
    suppressWarnings(min(which(tab$y == 100)))
  )
  tab[keep[!keep %in% c(-Inf, Inf)], ]
}

#' Checks that an object represents a single finite number
#' @param n an object to check
#' @examples
#' mipdtrial:::is_single_valid_number(9)
#' mipdtrial:::is_single_valid_number(mtcars)
#' mipdtrial:::is_single_valid_number(c(1, 2))

is_single_valid_number <- function(n) {
  length(n) == 1 && is.atomic(n) && is.numeric(n) && !is.na(n) && is.finite(n)
}
