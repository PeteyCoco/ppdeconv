#' Title
#'
#' @param x
#' @param p
#'
#' @return
#' @export
#'
#' @examples
set_par <- function(x, p){
  UseMethod("set_par")
}

#' Title
#'
#' @param x
#' @param p
#'
#' @return
#' @export
#'
#' @examples
set_par.default <- function(x, p){
  stop(paste0("no method 'set_par' for class", class(x)))
}

#' Title
#'
#' @param x
#' @param p
#'
#' @return
#' @export
#'
#' @examples
set_par.ppdeconvFix <- function(x, p){
  x$a <- p[x$a_idx]
  x
}

#' Title
#'
#' @param x
#' @param p
#'
#' @return
#' @export
#'
#' @examples
set_par.ppdeconvVar <- function(x, p){
  x$a <- p[x$a_idx]
  x$b <- p[x$b_idx]
  x$P <- x$P_fn(x)
  x
}

