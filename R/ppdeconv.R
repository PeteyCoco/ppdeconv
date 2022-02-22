#' Title
#'
#' @param a0 an initial estimate for the parameter
#' @param x a list containing the data for `pdeconv`. See details.
#' @param mode a string indicating what type of inference to perform.
#'
#' @return a model fit object
#' @export
#'
#' @examples
#' # TODO
ppdeconv <-
  function(x,
           a0 = NULL,
           method = "mle",
           control = list()) {
    # If program doesn't work, return NULL
    fit <- NULL

    # Combine data into a single ppdeconvObj
    x <- format_bdiag(x)

    if (method == "mle") {
      # Define the objective function and its gradient
      # (I'm updating the parameters within each function to make sure
      # that the most recent parameters are used in both functions)
      fn <- function(p) {
        get_loglik(x, p)
      }
      gr <- function(p) {
        get_gradient(x, p)
      }

      # Set the initial parameter guess
      if (is.null(a0)) {
        a0 <- rep(0, length(x$p))
      }

      fit <- stats::optim(
        par = a0,
        fn = fn,
        gr = gr,
        method = "BFGS",
        control = c(list(fnscale = -1), control)
      )

      # Apply final estimate to the data
      x$p <- fit$par
    } else if (method == "rmle") {
      stop("mode 'reml' has not been implemented")
    } else{
      stop("mode must be either 'mle' or 'reml'")
    }

    result <- list(data = x, fit = fit)
    class(result) <- "ppdeconvFit"
    return(result)
  }
