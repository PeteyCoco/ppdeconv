#' The Hessian of the unpenalized log-likelihood
#'
#' @param a the p-dimensional parameter vector
#' @param y the n-dimensional data vector of counts
#' @param Q the m x p structure matrix
#' @param P the n x m matrix of transition probabilities
#' @param S the p x p penalty matrix
#' @param c0 the smoothness parameter
#'
#' @return the p x p Hessian of the unpenalized log-likelihood with respect to the parameter `a`
#' @export
lddot <- function(a, y, Q, P, S, c0){

  la <- as.vector(exp(Q %*% a))

  ra <- as.vector(P %*% la)

  Pt <- P/ra

  W <- la * t(Pt)

  yra_W <- (y - ra) * t(W)

  WyW <- W %*% (y*t(W))

  WyW_diag <- diag(colSums(yra_W)) - WyW

  result <- t(Q) %*% (WyW_diag %*% Q)

  return(result)
}
