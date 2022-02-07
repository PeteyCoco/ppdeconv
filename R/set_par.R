set_par <- function(x, p){
  UseMethod("set_par")
}

set_par.default <- function(x, p){
  stop(paste0("no method 'set_par' for class", class(x)))
}

set_par.ppdeconvFix <- function(x, p){
  x$a <- p[x$a_idx]
  x
}

set_par.ppdeconvVar <- function(x, p){
  x$a <- p[x$a_idx]
  x$b <- p[x$b_idx]
  x$P <- x$P_fn(x)
  x
}

