#' The gradient of the unpenalized log-likelihood
#'
#' @param a the p-dimensional parameter vector
#' @param y the n-dimensional data vector of counts
#' @param Q the m x p structure matrix
#' @param P the n x m matrix of transition probabilities
#' @param S the p x p penalty matrix
#' @param c0 the smoothness parameter
#'
#' @return the p-dimensional gradient of the unpenalized log-likelihood with respect to the parameter `a`
#' @import Matrix
#' @export
ldot <- function(a, y, Q, P, S, c0){

  la <- as.vector(exp(Q %*% a))

  ra <- as.vector(P %*% la)

  Pt <- P/ra

  W <- la * t(Pt)

  yra_W <- t((y - ra) * t(W))

  ldot_i <- t(Q) %*% yra_W

  return(rowSums(ldot_i))
}

#' Title
#'
#' @param a the p-dimensional parameter vector
#' @param y the n-dimensional data vector of counts
#' @param Q the m x p structure matrix
#' @param P the n x m matrix of transition probabilities
#' @param S the p x p penalty matrix
#' @param c0 the smoothness parameter
#'
#' @return the p-dimensional gradient vector
#' @export
#'
#' @examples #TODO
sdot <- function(a, y, Q, P, S, c0){
  return(c0 * (S %*% a))
}

#' Compute gradient of the penalized log-likelihood
#'
#' @param a the p-dimensional parameter vector
#' @param y the n-dimensional data vector of counts
#' @param Q the m x p structure matrix
#' @param P the n x m matrix of transition probabilities
#' @param S the p x p penalty matrix
#' @param c0 the smoothness parameter
#'
#' @return the p-dimesional gradient vector
#' @export
#'
#' @examples #TODO
gradient <- function(a, y, Q, P, S, c0){

  result <- ldot(a, y, Q, P, S, c0) - sdot(a, y, Q, P, S, c0)

  return(result)
}
