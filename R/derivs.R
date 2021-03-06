#' gradient of log-likelihood
#'
#' See documentation of `gradient` for details
#'
#' @param x a `ppdeconvObj` or `ppdeconvVar` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the gradient of the log-likelihood
#' @export
#'
#' @examples #TODO
ldot <- function(x, a) {
  UseMethod("ldot")
}

#' Title
#'
#' @param x any object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
ldot.default <- function(x, a) {
  stop(paste0("There is no method ldot for objects of the class '", class(x), "'"))
}

#' `ldot` for fixed P ppdeconv model
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the gradient of the log-likelihood
#' @export
#' @importFrom Matrix Diagonal
#'
#' @examples #TODO
ldot.ppdeconvObj <- function(x, a) {
  la <- as.vector(exp(x$Q %*% a))

  ra <- as.vector(x$P %*% la)

  ldot_i <- Diagonal(x = x$N/ra - 1) %*% x$P %*% Diagonal(x = la) %*% x$Q

  return(Matrix::colSums(ldot_i))
}

#' gradient of the penalty term
#'
#' See documentation of `gradient` for details
#'
#' @param x a `ppdeconvObj` or `ppdeconvVar` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the gradient of the penalty term
#' @export
#'
#' @examples #TODO
sdot <- function(x, a) {
  UseMethod("sdot")
}

#' Title
#'
#' @param x any object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
sdot.default <- function(x, a) {
  stop(paste0("There is no method sdot for objects of the class '", class(x), "'"))
}

#' `sdot` for fixed P ppdeconv model
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#' @importFrom Matrix colSums
#'
#' @examples #TODO
sdot.ppdeconvObj <- function(x, a) {
  la <- as.vector(exp(x$Q %*% a))
  prior <- as.vector(t(colSums((x$M - la) * x$Q,na.rm = TRUE)))
  return(as.vector(-(x$c0 * x$S) %*% a) + prior )
}

#' gradient for ppdeconv models
#'
#' TODO: Add more detail
#'
#' @param x a `ppdeconvObj` or `ppdeconvVar` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the gradient of the penalized log-likelihood
#' @export
#'
#' @examples #TODO
gradient <- function(x, a) {
  UseMethod("gradient")
}

#' Title
#'
#' @param x any object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
gradient.default <- function(x, a) {
  stop(paste0(
    "there is no method gradient for objects of the class '",
    class(x),
    "'"
  ))
}

#' gradient for fixed P ppdeconv models
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the gradient of the penalized log-likelihood
#' @export
#'
#' @examples #TODO
gradient.ppdeconvObj <- function(x, a) {
  result <- ldot(x, a) + sdot(x, a)

  return(as.vector(result))
}

#' Title
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
lddot <- function(x, a) {
  UseMethod("lddot")
}

#' Title
#'
#' @param x any object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
lddot.default <- function(x, a) {
  stop(paste0("There is no method lddot for objects of the class '", class(x), "'"))
}

#' hessian of log-likelihood for `ppdeconvObj`
#'
#' @param x, a a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the hessian of the log-likelihood
#' @export
#' @importFrom Matrix t
#'
#' @examples #TODO
lddot.ppdeconvObj <- function(x, a) {
  la <- as.vector(exp(x$Q %*% a))

  ra <- as.vector(x$P %*% la)

  Pt <- x$P / ra

  W <- la * t(Pt)

  yra_W <- (x$N - ra) * t(W)

  WyW <- W %*% (x$N * t(W))

  WyW_diag <- diag(Matrix::colSums(yra_W)) - WyW

  result <- t(x$Q) %*% (WyW_diag %*% x$Q)

  return(result)
}
#' Title
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
sddot <- function(x, a) {
  UseMethod("sddot")
}

#' Title
#'
#' @param x any object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
sddot.default <- function(x, a) {
  stop(paste0("There is no method sddot for objects of the class '", class(x), "'"))
}

#' hessian for the penalty term of `ppdeconvObj` objects
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the hessian of the penalty term
#' @export
#'
#' @examples #TODO
sddot <- function(x, a) {
  return(-x$c0 * x$S)
}

#' title
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
hessian <- function(x, a) {
  UseMethod("hessian")
}

#' Title
#'
#' @param x any object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return
#' @export
#'
#' @examples #TODO
hessian.default <- function(x, a) {
  stop(paste0(
    "There is no method hessian for objects of the class '",
    class(x),
    "'"
  ))
}


#' hessian for the penalized log-likelihood
#'
#' @param x a `ppdeconvObj` object
#' @param a a parameter vector with length(a) = ncol(x$Q)
#'
#' @return the hessian of the penalized log-likelihood
#' @export
#'
#' @examples #TODO
hessian.ppdeconvObj <- function(x, a) {
  result <- lddot(x, a) + sddot(x, a)

  return(result)
}

get_gradient <- function(x, p) {
  gradient(x, p[x$a_idx])
}
