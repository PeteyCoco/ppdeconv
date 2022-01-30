#' Title
#'
#' @param a0 an initial estimate for the parameter
#' @param data a list containing the data for `pdeconv`. See details.
#' @param mode a string indicating what type of inference to perform.
#'
#' @return a model fit object
#' @export
#'
#' @examples
#' # TODO
ppdeconv <- function(a0 = NULL, data, mode = "mle") {
  # Default fit NULL is returned in case of error
  fit <- NULL

  if (mode == "mle") {
    data_bdiag <- to_bdiag(data)

    # Define the objective function and its gradient
    fn <-
      function(p)
        loglik(
          p,
          y = data_bdiag$y,
          Q = data_bdiag$Q,
          P = data_bdiag$P,
          S = data_bdiag$S,
          c0 = data_bdiag$c0
        )
    gr <-
      function(p)
        gradient(
          p,
          y = data_bdiag$y,
          Q = data_bdiag$Q,
          P = data_bdiag$P,
          S = data_bdiag$S,
          c0 = data_bdiag$c0
        )

    # Maximize
    if (is.null(a0)) {
      a0 <- rep(0, times = ncol(data_bdiag$Q))
    }

    fit <- stats::optim(
      par = a0,
      fn = fn,
      gr = gr,
      method = "BFGS",
      control = list(fnscale = -1)
    )
  }
  else if (mode == "rmle") {

  }
  else{
    stop("mode not recognized")
  }

  return(fit)
}
