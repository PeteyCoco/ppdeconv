#' Get midpoint
#'
#' A helper function for computing the midpoints for a given a vector defining a partition of an interval.
#'
#' @param x_breaks a (n+1) vector defining a partition of an interval
#'
#' @return an n vector of the midpoints of `x_breaks`
#' @export
#'
#' @examples
#' x_breaks <- c(0, 2, 3, 5, 8)
#' x_mid <- get_midpoints(x_breaks)
get_midpoints <- function(x_breaks){

  assertthat::assert_that(
    length(x_breaks) >= 2,
    is.numeric(x_breaks),
    assertthat::noNA(x_breaks)
  )

  x_breaks <- sort(x_breaks)
  mid <- (x_breaks[-1] + x_breaks[-length(x_breaks)])/2
  return(mid)
}
