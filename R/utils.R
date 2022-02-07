#' Get midpoint
#'
#' A helper function that returns a vector of q equally spaced
#' points within the intervals specified by x_breaks
#'
#' @param x_breaks a (n+1) vector defining a partition of an interval
#' @param q an integer specifying the number of points within interval
#'
#' @return an n vector of the midpoints of `x_breaks`
#' @export
#'
#' @examples
#' x_breaks <- c(0, 2, 3, 5, 8)
#' x_mid <- get_midpoints(x_breaks)
get_midpoints <- function(x_breaks, q = 1) {
  assertthat::assert_that(length(x_breaks) >= 2,
                          is.numeric(x_breaks),
                          assertthat::noNA(x_breaks))

  x_breaks <- sort(x_breaks)
  x_diff <- diff(x_breaks)
  mid <-
    lapply(
      1:q,
      FUN = function(x)
        x_breaks[-length(x_breaks)] + as.numeric(x) * x_diff / (q + 1)
    )
  mid <- sort(unlist(mid))
  return(mid)
}


configure_idx <- function(x, mode) {
  # Change default parameter idx's to reflect separate models
  a_idx <- rep(1:length(x), times = unlist(lapply(x, function(y) length(y$a))))
  par_idx <- 1:length(a_idx)
  b_idx <- length(par_idx) + 1

  for (i in 1:length(x)) {
    x[[i]]$a_idx <- par_idx[i == a_idx]
    if (mode == "variable") {
      x[[i]]$b_idx <- b_idx
    }
  }

  x
}
