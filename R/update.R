update_a <- function(x, a){
  x$a <- a
  x
}

update_b <- function(x, b){
  x$b <- b
  x$P <- x$P_fn(x)
  x
}

