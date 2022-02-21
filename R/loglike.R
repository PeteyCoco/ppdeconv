#' log-likelihood for `ppdeconv` object
#'
#' @param x a `ppdeconvFix` object
#'
#' @return the log-likelihood of the object
#' @export
#'
#' @examples #TODO
loglik <- function(x, a) {
  return(lfn(x, a) + sfn(x, a))
}

get_loglik <- function(x, p) {
  loglik(x, p[x$a_idx])
}

lfn <- function(x, a) {
  la <- exp(x$Q %*% a)
  ra <- as.vector(x$P %*% la)
  li <- x$N * log(ra) - ra
  ll <- sum(li)
  return(as.double(ll))
}

sfn <- function(x, a) {
  Ja <- -0.5 * t(a) %*% (x$c0 * x$S) %*% a
  la <- exp(x$Q %*% a)
  priors <- 0
  # priors <- sum(x$M * log(la) - la, na.rm = TRUE)
  return(as.double(Ja + priors))
}
