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

#' Put data into block diagonal format
#'
#' @param data list of data for the ppdeconv function
#'
#' @return list of data in block diagonal format
#' @export
#'
#' @examples
#' # TODO
to_bdiag <- function(data){

    # Put data into block diagonal format
    y_bdiag <- unlist(lapply(data, function(x) x$y))
    Q_bdiag <- bdiag(lapply(data, function(x) x$Q))
    P_bdiag <- bdiag(lapply(data, function(x) x$P))
    S_bdiag <- bdiag(lapply(data, function(x) x$S))
    c0_bdiag <- rep(unlist(lapply(data, function(x) x$c0)),
                   times = unlist(lapply(data, function(x) nrow(x$S))))

    data_bdiag <- list(y = y_bdiag, Q = Q_bdiag, P = P_bdiag, S = S_bdiag, c0 = c0_bdiag)
    return(data_bdiag)
}
