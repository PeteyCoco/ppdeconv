#' Title
#'
#' Computes the penalized log-likelihood for the mis-measured point process
#' with transition matrix P and latent structure matrix Q as described in
#' equations (24) and (25)
#'
#' @param a the p-vector initial guess for mle
#' @param y the n observed cell counts
#' @param Q the m x p structure matrix
#' @param P the n x m transition matrix#'
#' @param S the p x p penalty matrix
#' @param c0 the scalar penalty
#'
#' @return the penalized log-likelihood
#' @export
#'
#' @examples
#' # TODO
loglik <- function(a, y, Q, P, S, c0 ){

  la <- exp(Q %*% a)
  ra <- as.vector(P %*% la)

  li <- y * log(ra) - ra
  Ja <- 0.5* t(a) %*% (c0 * S) %*% a
  ll <- sum(li) - Ja

  return(as.numeric(ll))
}
