#' ppdeconv Object
#'
#' @param N a vector of length n of observed counts
#' @param Q a m x p structure matrix
#' @param P_fn a function on `ppdeconvFix` objects that returns an
#' n x m matrix of transition probabilities
#' @param S a p x p penalty matrix
#' @param c0 a numeric smoothness parameter
#' @param l_breaks an (m + 1)-vector of latent space break points
#' @param r_breaks an (n + 1)-vector of observed space breaks points
#' @param l_grid an m-vector of points between the breaks of `l_breaks`
#' @param r_grid an n-vector of points between the breaks of `r_breaks`
#' @param a a p-dimensional parameter vector corresponding to `Q`
#'
#' @return a `ppdeconvFix` object
#' @export
#'
#' @examples #TODO
new_ppdeconvFix <- function(N = numeric(),
                            Q = matrix(nrow = 0, ncol = 0),
                            P_fn,
                            S = matrix(nrow = 0, ncol = 0),
                            c0 = numeric(),
                            l_breaks = numeric(),
                            r_breaks = numeric(),
                            l_grid = numeric(),
                            r_grid = numeric(),
                            a = NULL
                            ){

  stopifnot(is.numeric(N))
  stopifnot(is.matrix(Q))
  stopifnot(is.function(P_fn))
  stopifnot(is.matrix(S))
  stopifnot(is.numeric(c0))
  stopifnot(is.numeric(l_breaks))
  stopifnot(is.numeric(r_breaks))
  stopifnot(is.numeric(l_grid))
  stopifnot(is.numeric(r_grid))

  if(is.null(a)){
    a <- rep(0,length.out = ncol(Q))
  }

  stopifnot(is.numeric(a))


  obj <- list()
  obj$N <- N
  obj$Q <- Q
  obj$P_fn <- P_fn
  obj$S <- S
  obj$c0 <- c0
  obj$l_breaks <- l_breaks
  obj$r_breaks <- r_breaks
  obj$l_grid <- l_grid
  obj$r_grid <- r_grid
  obj$a <- a

  class(obj) <- "ppdeconvFix"

  obj
}
