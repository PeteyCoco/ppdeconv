#' gradient of log-likelihood
#'
#' See documentation of `gradient` for details
#'
#' @param x a `ppdeconvFix` or `ppdeconvVar` object
#'
#' @return the gradient of the log-likelihood
#' @export
#'
#' @examples #TODO
ldot <- function(x){
  UseMethod("ldot")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
ldot.default <- function(x){
  stop(paste0("There is no method ldot for objects of the class '", class(x),"'"))
}

#' `ldot` for fixed P ppdeconv model
#'
#' @param x a `ppdeconvFix` object
#'
#' @return the gradient of the log-likelihood
#' @export
#'
#' @examples #TODO
ldot.ppdeconvFix <- function(x){

  la <- as.vector(exp(x$Q %*% x$a))

  ra <- as.vector(x$P %*% la)

  Pt <- x$P/ra

  W <- la * t(Pt)

  yra_W <- t((x$N - ra) * t(W))

  ldot_i <- t(x$Q) %*% yra_W

  return(rowSums(ldot_i))
}

#' gradient of the penalty term
#'
#' See documentation of `gradient` for details
#'
#' @param x a `ppdeconvFix` or `ppdeconvVar` object
#'
#' @return the gradient of the penalty term
#' @export
#'
#' @examples #TODO
sdot <- function(x){
  UseMethod("sdot")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
sdot.default <- function(x){
  stop(paste0("There is no method sdot for objects of the class '", class(x),"'"))
}

#' `sdot` for fixed P ppdeconv model
#'
#' @param x a `ppdeconvFix` object
#'
#' @return
#' @export
#'
#' @examples #TODO
sdot.ppdeconvFix <- function(x){
  return((x$c0 * x$S) %*% x$a)
}

#' gradient for ppdeconv models
#'
#' TODO: Add more detail
#'
#' @param x a `ppdeconvFix` or `ppdeconvVar` object
#'
#' @return the gradient of the penalized log-likelihood
#' @export
#'
#' @examples #TODO
gradient <- function(x){
  UseMethod("gradient")
}

gradient.default <- function(x){
  stop(paste0("there is no method gradient for objects of the class '", class(x),"'"))
}

#' gradient for fixed P ppdeconv models
#'
#' @param obj a `ppdeconvFix` object
#'
#' @return the gradient of the penalized log-likelihood
#' @export
#'
#' @examples #TODO
gradient.ppdeconvFix <- function(obj){

  result <- ldot(obj) - sdot(obj)

  return(as.numeric(result))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
lddot <- function(x){
  UseMethod("lddot")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
lddot.default <- function(x){
  stop(paste0("There is no method lddot for objects of the class '", class(x),"'"))
}

#' hessian of log-likelihood for `ppdeconvFix`
#'
#' @param x a `ppdeconvFix` object
#'
#' @return the hessian of the log-likelihood
#' @export
#'
#' @examples #TODO
lddot.ppdeconvFix <- function(x){

  la <- as.vector(exp(x$Q %*% x$a))

  ra <- as.vector(x$P %*% la)

  Pt <- x$P/ra

  W <- la * t(Pt)

  yra_W <- (x$N - ra) * t(W)

  WyW <- W %*% (x$N*t(W))

  WyW_diag <- diag(colSums(yra_W)) - WyW

  result <- t(x$Q) %*% (WyW_diag %*% x$Q)

  return(result)
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
sddot <- function(x){
  UseMethod("sddot")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
sddot.default <- function(x){
  stop(paste0("There is no method sddot for objects of the class '", class(x),"'"))
}

#' hessian for the penalty term of `ppdeconvFix` objects
#'
#' @param x a `ppdeconvFix` object
#'
#' @return the hessian of the penalty term
#' @export
#'
#' @examples #TODO
sddot <- function(x){
  return(x$c0 * x$S)
}

#' title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
hessian <- function(x){
  UseMethod("hessian")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples #TODO
hessian.default <- function(x){
  stop(paste0("There is no method hessian for objects of the class '", class(x),"'"))
}

#' hessian for the penalized log-likelihood
#'
#' @param x a `ppdeconvFix` object
#'
#' @return the hessian of the penalized log-likelihood
#' @export
#'
#' @examples #TODO
hessian.ppdeconvFix <- function(x){

  result <- lddot(x) - sddot(x)

  return(result)
}
