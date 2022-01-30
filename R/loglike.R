#' log-likelihood for `ppdeconv` object
#'
#' @param x a `ppdeconvFix` object
#'
#' @return the log-likelihood of the object
#' @export
#'
#' @examples #TODO
loglik <- function(x) {
  la <- exp(x$Q %*% x$a)
  ra <- as.vector(x$P %*% la)

  li <- x$N * log(ra) - ra
  Ja <- 0.5 * t(x$a) %*% (x$c0 * x$S) %*% x$a
  ll <- sum(li) - Ja

  return(as.numeric(ll))
}
