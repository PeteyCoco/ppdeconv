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

#' Title
#'
#' @param x
#'
#' @return
#' @export
#' @importFrom Matrix bdiag
#'
#' @examples
format_bdiag <- function(x){

  # Combine each ppdeconv into a bdiag format
  args <- list(
    N = unlist(lapply(x, function(y) y$N)),
    Q = do.call(bdiag, lapply(x, function(y) y$Q)),
    S = do.call(bdiag, lapply(x, function(y) y$S)),
    P_fn = function(b) do.call(bdiag, lapply(x, function(y) y$P_fn(b))),
    c0 = rep(vapply(x, function(y) y$c0, numeric(1)),
                    times = vapply(x, function(y) ncol(y$S), numeric(1))),
    l_breaks = unlist(lapply(x, function(y) y$l_breaks)),
    l_grid = unlist(lapply(x, function(y) y$l_grid)),
    r_breaks = unlist(lapply(x, function(y) y$r_breaks)),
    r_grid = unlist(lapply(x, function(y) y$r_grid)),
    p = unlist(lapply(x, function(y) y$p)),
    M = unlist(lapply(x, function(y) y$M))
  )

  do.call(new_ppdeconvObj, args)
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

