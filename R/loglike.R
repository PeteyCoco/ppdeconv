#' log-likelihood for `ppdeconv` object
#'
#' @param x a `ppdeconvFix` object
#'
#' @return the log-likelihood of the object
#' @export
#'
#' @examples #TODO
loglik <- function(x, a) {
  la <- exp(x$Q %*% a)
  ra <- as.vector(x$P %*% la)

  li <- x$N * log(ra) - ra
  Ja <- 0.5 * t(a) %*% (x$c0 * x$S) %*% a
  ll <- sum(li) - Ja

  return(as.numeric(ll))
}

get_loglik <- function(x, p){
  loglik(x, p[x$a_idx])
}
