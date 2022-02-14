#' ppdeconv Object
#'
#' @param N a vector of length n of observed counts
#' @param Q a m x p structure matrix
#' @param P_fn a function on `ppdeconvFix` objects that returns an
#' n x m matrix of transition probabilities
#' @param S a p x p penalty matrix
#' @param c0 a numeric smoothness parameter
#' @param l_breaks an (m + 1)-vector of latent space break points
#' @param r_breaks an (n + 1)-vector of observed space breaks points
#' @param l_grid an m-vector of points between the breaks of `l_breaks`
#' @param r_grid an n-vector of points between the breaks of `r_breaks`
#' @param a a p-dimensional parameter vector corresponding to `Q`
#'
#' @return a `ppdeconvFix` object
#' @export
#'
#' @examples #TODO
new_ppdeconvFix <- function(N = numeric(),
                            Q = matrix(nrow = 0, ncol = 0),
                            P = matrix(nrow = 0, ncol = 0),
                            S = matrix(nrow = 0, ncol = 0),
                            c0 = numeric(),
                            l_breaks = numeric(),
                            r_breaks = numeric(),
                            l_grid = numeric(),
                            r_grid = numeric(),
                            a = NULL,
                            a_idx = NULL,
                            M = NULL) {
  stopifnot(is.numeric(N))
  stopifnot(is.matrix(Q) | class(Q) == "dgCMatrix")
  stopifnot(is.matrix(P) | class(Q) == "dgCMatrix")
  stopifnot(is.matrix(S) | class(Q) == "dgCMatrix")
  stopifnot(is.numeric(c0))
  stopifnot(is.numeric(l_breaks))
  stopifnot(is.numeric(r_breaks))
  stopifnot(is.numeric(l_grid))
  stopifnot(is.numeric(r_grid))

  if (is.null(a)) {
    a <- rep(0, length.out = ncol(Q))
  }
  a_idx <- 1:length(a)

  stopifnot(is.numeric(a))
  stopifnot(is.numeric(a_idx))

  if (is.null(M)) {
    M <- rep(NA, times = length(l_grid))
  }
  stopifnot(length(M) == length(l_grid))

  obj <- list()
  obj$N <- N
  obj$Q <- Q
  obj$P <- P
  obj$S <- S
  obj$c0 <- c0
  obj$l_breaks <- l_breaks
  obj$r_breaks <- r_breaks
  obj$l_grid <- l_grid
  obj$r_grid <- r_grid
  obj$a <- a
  obj$a_idx <- a_idx
  obj$M <- M

  class(obj) <- "ppdeconvFix"

  obj
}

#' Title
#'
#' @param N
#' @param Q
#' @param P
#' @param S
#' @param c0
#' @param l_breaks
#' @param r_breaks
#' @param l_grid
#' @param r_grid
#' @param a
#' @param a_idx
#' @param b
#' @param b_idx
#' @param M
#' @param P_fn
#'
#' @return
#' @export
#'
#' @examples
new_ppdeconvVar <- function(N = numeric(),
                            Q = matrix(nrow = 0, ncol = 0),
                            P = matrix(nrow = 0, ncol = 0),
                            S = matrix(nrow = 0, ncol = 0),
                            c0 = numeric(),
                            l_breaks = numeric(),
                            r_breaks = numeric(),
                            l_grid = numeric(),
                            r_grid = numeric(),
                            a = NULL,
                            a_idx = NULL,
                            b,
                            b_idx = NULL,
                            M = NULL,
                            P_fn) {
  stopifnot(is.numeric(N))
  stopifnot(is.matrix(Q) | class(Q) == "dgCMatrix")
  stopifnot(is.matrix(P) | class(Q) == "dgCMatrix")
  stopifnot(is.matrix(S) | class(Q) == "dgCMatrix")
  stopifnot(is.numeric(c0))
  stopifnot(is.numeric(l_breaks))
  stopifnot(is.numeric(r_breaks))
  stopifnot(is.numeric(l_grid))
  stopifnot(is.numeric(r_grid))

  if (is.null(a)) {
    a <- rep(0, length.out = ncol(Q))
  }
  a_idx <- 1:length(a)
  b_idx <- max(a_idx) + 1:length(b)

  stopifnot(is.numeric(a))
  stopifnot(is.numeric(a_idx))
  stopifnot(is.numeric(b))
  stopifnot(is.numeric(b_idx))

  if (is.null(M)) {
    M <- rep(NA, times = length(l_grid))
  }
  stopifnot(length(M) == length(l_grid))

  stopifnot(is.function(P_fn))

  obj <- list()
  obj$N <- N
  obj$Q <- Q
  obj$P <- P
  obj$S <- S
  obj$c0 <- c0
  obj$l_breaks <- l_breaks
  obj$r_breaks <- r_breaks
  obj$l_grid <- l_grid
  obj$r_grid <- r_grid
  obj$a <- a
  obj$a_idx <- a_idx
  obj$b <- b
  obj$b_idx <- b_idx
  obj$M <- M
  obj$P_fn <- P_fn

  class(obj) <- "ppdeconvVar"

  obj
}

new_ppdeconvList <- function(x){
  if (!all(vapply(x, function(y)
    class(y) %in% c("ppdeconvFix", "ppdeconvVar"),
    logical(1)))) {
    stop("List elements must be of type `ppdeconvFix` or `ppdeconvVar`.")
  }
  obj <- x
  class(obj) <- "ppdeconvList"
  obj
}
