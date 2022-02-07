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
ppdeconv <-
  function(a0 = NULL,
           data,
           mode = "fixed",
           method = "mle") {

    # Convert list of data to a list of ppdeconvFix or ppdeconvVar objects
    if (mode == "fixed") {
      data <- lapply(
        data,
        FUN = function(x) {
          do.call(new_ppdeconvFix, x)
        }
      )

      # Change default parameter idx's to reflect separate models
      a_idx <-
        rep(1:length(data), times = unlist(lapply(data, function(x) {
          ncol(x$Q)
        })))
      par_idx <- 1:length(a_idx)
      for (i in 1:length(data)) {
        data[[i]]$a_idx <- par_idx[i == a_idx]

      }
    }
    else if (mode == "variable") {
      stop("mode 'variable' has not been implemented")
    }
    else{
      stop("mode must be either 'fixed' or 'variable'")
    }
    if (method == "mle") {

      # Define the objective function and its gradient
      # (I'm updating the parameters within each function to make sure
      # that the most recent parameters are used in both functions)
      fn <- function(p) {
        data <- lapply(data, FUN = function(x) set_par(x, p))
        result <- lapply(data, FUN = function(x) get_loglik(x, p))
        sum(unlist(result))
      }
      gr <- function(p) {
        data <- lapply(data, FUN = function(x) set_par(x, p))
        result <- lapply(data, FUN = function(x) get_gradient(x, p))
        rowSums(matrix(unlist(result), ncol = length(data)))
      }

      # Set the initial parameter guess
      if (is.null(a0)) {
        a0 <- unlist(lapply(data, function(x) rep(0, ncol(x$Q))))
      }

      fit <- stats::optim(
        par = a0,
        fn = fn,
        # gr = gr,
        method = "BFGS",
        control = list(fnscale = -1)
      )
    }
    else if (method == "rmle") {
      stop("mode 'reml' has not been implemented")
    }
    else{
      stop("mode must be either 'mle' or 'reml'")
    }

    result <- list(data = data, fit = fit)
    class(result) <- "ppdeconvFit"
    return(fit)
  }
