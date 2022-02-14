#' Title
#'
#' @param x
#' @param type
#'
#' @return
#' @export
#'
#' @examples
fitted.ppdeconvFix <- function(x, type = "latent") {
  if (type == "latent") {
    data.frame(l_grid = x$l_grid,
               lambda = as.vector(exp(x$Q %*% x$a)))
  } else if (type == "observed") {
    data.frame(r_grid = x$r_grid,
               rho = as.vector(x$P %*% exp(x$Q %*% x$a)),
               N = x$N)
  } else {
    stop("`type` must be either `observed` or `latent`.")
  }
}

#' Title
#'
#' @param x
#' @param type
#'
#' @return
#' @export
#'
#' @examples
fitted.ppdeconvList <- function(x, type = "latent", unlist = TRUE) {
  if (type == "latent") {
    result <-
      lapply(x, function(y)
        fitted.ppdeconvFix(y, type = "latent"))
  } else if (type == "observed") {
    result <-
      lapply(x, function(y)
        fitted.ppdeconvFix(y, type = "observed"))
  } else {
    stop("`type` must be either `observed` or `latent`.")
  }

  if (unlist) {
    # Stack the dataframes and add a label column
    for (i in seq_along(result)) {
      result[[i]]$label <- names(result)[[i]]
    }
    result <-
      do.call(function(...)
        rbind(..., make.row.names = FALSE), result)
  }
  result
}
