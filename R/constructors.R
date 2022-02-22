#' ppdeconv Object
#'
#' @param N a vector of length n of observed counts
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
#'
#' @return a `ppdeconvObj` object
#' @export
#' @importFrom Matrix Matrix
#'
#' @examples #TODO
new_ppdeconvObj <- function(N = numeric(),
                            Q = matrix(nrow = 0, ncol = 0),
                            P_fn,
                            S = matrix(nrow = 0, ncol = 0),
                            c0 = numeric(),
                            l_breaks = numeric(),
                            r_breaks = numeric(),
                            l_grid = numeric(),
                            r_grid = numeric(),
                            p = NULL,
                            a_idx = NULL,
                            b_idx = integer(0),
                            M = NULL) {
  stopifnot(is.numeric(N))
  stopifnot(is.matrix(Q) | class(Q) == "dgCMatrix")
  stopifnot(is.matrix(S) | class(S) == "dgCMatrix")
  stopifnot(is.numeric(c0))
  stopifnot(is.numeric(l_breaks))
  stopifnot(is.numeric(r_breaks))
  stopifnot(is.numeric(l_grid))
  stopifnot(is.numeric(r_grid))

  a_idx <- 1:ncol(Q)
  b_idx <- max(a_idx) + b_idx

  stopifnot(is.numeric(a_idx))

  if (is.null(p)) {
    p <- rep(0, times = length(a_idx) + length(b_idx))
  }
  stopifnot(length(p) == length(a_idx) + length(b_idx))

  if (is.null(M)) {
    M <- rep(NA, times = length(l_grid))
  }
  stopifnot(length(M) == length(l_grid))

  P <- P_fn(p[b_idx])

  obj <- list(
    N = N,
    Q = Matrix(Q, sparse = T),
    P = P,
    P_fn = P_fn,
    S = S,
    c0 = c0,
    l_breaks = l_breaks,
    r_breaks = r_breaks,
    l_grid = l_grid,
    r_grid = r_grid,
    p = p,
    a_idx = a_idx,
    b_idx = b_idx,
    M = M
  )

  class(obj) <- "ppdeconvObj"

  obj
}
