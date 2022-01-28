#' ppdeconv Object
#'
#' @param N a vector of length n of observed counts
#' @param u a data.frame of covariates with n ro
#' @param Q a m x p structure matrix
#' @param P_fn a function on `ppdeconvObj` objects that returns an
#' n x m matrix of transition probabilities
#' @param S a p x p penalty matrix
#' @param c0 a numeric smoothness parameter
#' @param l_breaks an (m + 1)-vector of latent space break points
#' @param r_breaks an (n + 1)-vector of observed space breaks points
#' @param l_grid an m-vector of points between the breaks of `l_breaks`
#' @param r_grid an n-vector of points between the breaks of `r_breaks`
#' @param a a p-dimensional parameter vector corresponding to `Q`
#' @param b a q-dimensional parameter vector corresponding to `P`
#'
#' @return a `ppdeconvObj` object
#' @export
#'
#' @examples #TODO
new_ppdeconvObj <- function(N = numeric(),
                            u = data.frame(),
                            Q = matrix(nrow = 0, ncol = 0),
                            P_fn,
                            S = matrix(nrow = 0, ncol = 0),
                            c0 = numeric(),
                            l_breaks = numeric(),
                            r_breaks = numeric(),
                            l_grid = numeric(),
                            r_grid = numeric(),
                            a = numeric(),
                            b = numeric()
                            ){
  stopifnot(is.numeric(N))
  stopifnot(is.data.frame(u))
  stopifnot(is.matrix(Q))
  stopifnot(is.function(P_fn))
  stopifnot(is.matrix(S))
  stopifnot(is.numeric(c0))
  stopifnot(is.numeric(l_breaks))
  stopifnot(is.numeric(r_breaks))
  stopifnot(is.numeric(l_grid))
  stopifnot(is.numeric(r_grid))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(b))


  obj <- list()
  obj$N <- N
  obj$u <- u
  obj$Q <- Q
  obj$P_fn <- P_fn
  obj$S <- S
  obj$c0 <- c0
  obj$l_breaks <- l_breaks
  obj$r_breaks <- r_breaks
  obj$l_grid <- l_grid
  obj$r_grid <- r_grid
  obj$a <- a
  obj$b <- b

  class(obj) <- "ppdeconvObj"

  obj
}
