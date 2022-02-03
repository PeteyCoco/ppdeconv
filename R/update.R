update_Fix <- function(x, a){
  x$a <- a
  x
}

update_Var <- function(x, a, b){
  x$a <- a
  x$b <- b
  x$P <- x$P_fn(x)
  x
}

