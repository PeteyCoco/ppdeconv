#' Simulate non-homogeneous Poisson process on the real line
#'
#' @param lambda non-negative intensity function. The first argument is the coordinate on the real line.
#' @param min lower limit of the observation interval
#' @param max upper limit of the observation interval
#' @param M an upper bound for the function lambda. If M = `NULL`, an upper bound is obtained using `optimize`.
#'
#' @return a vector containing the points of the realization
#' @export
#'
#' @examples
#' # Using known bound M over interval [0,10]
#' lambda <- function(x) abs(sin(x))
#' y <- sim_nhpp(lambda = lambda, min = 0, max = 10, M = 1)
#' # Using unknown bound M over interval [0,10]
#' y <- sim_nhpp(lambda = lambda, min = 0, max = 10, M = NULL)
sim_nhpp <- function(lambda, min, max, M = NULL){

  assertthat::assert_that(assertthat::is.number(min),
                          assertthat::is.number(max),
                          max > min)

  if(is.null(M)){
    M <- stats::optimize(f = lambda, lower = min, upper = max, maximum = TRUE)[["objective"]]
    M <- M *(1.01) # Ensure that M is an upper bound of lambda
  }
  assertthat::assert_that(M > 0,
                          msg = "M must be a positive number. If M = NULL, check that lambda is a nonnegative function.")

  # Simulate from HPP with rate M
  width <- max - min
  hpp <- ppdeconv::sim_hpp(rate=M, min = min, max = max)

  assertthat::assert_that(all(lambda(hpp) >= 0),
                          msg = "Some values of lambda are negative. Check that lambda is a nonnegative function.")

  # Importance sampling
  p <- lambda(hpp)/M
  keep <- stats::rbinom(n = length(hpp), size = 1, p = p)

  return(hpp[as.logical(keep)])
}
