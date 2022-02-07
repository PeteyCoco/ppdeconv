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

#' Put data into block diagonal format
#'
#' @param data list of data for the ppdeconv function
#'
#' @return list of data in block diagonal format
#' @export
#'
#' @importFrom Matrix bdiag
#'
#' @examples
#' # TODO
to_bdiag.ppdeconvFix <- function(data) {
  # Put data into block diagonal format
  N_bdiag <- unlist(lapply(data, function(x)
    x$N))
  Q_bdiag <- bdiag(lapply(data, function(x)
    x$Q))
  P_bdiag <- bdiag(lapply(data, function(x)
    x$P))
  S_bdiag <- bdiag(lapply(data, function(x)
    x$S))
  c0_bdiag <- rep(unlist(lapply(data, function(x)
    x$c0)),
    times = unlist(lapply(data, function(x)
      nrow(x$S))))
  l_grid_bdiag <- unlist(lapply(data, function(x)
    x$l_grid))
  r_grid_bdiag <- unlist(lapply(data, function(x)
    x$r_grid))
  a_bdiag <- unlist(lapply(data, function(x)
    x$a))

  data_bdiag <-
    new_ppdeconvFix(
      N = N_bdiag,
      Q = Q_bdiag,
      P = P_bdiag,
      S = S_bdiag,
      c0 = c0_bdiag,
      l_grid = l_grid_bdiag,
      r_grid = r_grid_bdiag,
      a = a_bdiag
    )
  return(data_bdiag)
}
