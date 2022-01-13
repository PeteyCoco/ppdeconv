#' Simulate realization from a homogeneous Poisson process
#'
#' @param rate non-negative number giving the rate parameter of the process
#' @param min lower limit of the observation interval
#' @param max upper limit of the observation interval
#'
#' @return a vector containing the points of the realization within the given observation interval
#' @export
#'
#' @examples
#' # A realization from a Poisson process with rate 10 observed over the unit interval [0,1]
#' y <- sim_hpp(rate = 10, min = 0, max = 1)
sim_hpp <- function(rate, min, max){

  assertthat::assert_that(assertthat::is.number(rate))
  assertthat::assert_that(rate >= 0)
  assertthat::assert_that(assertthat::is.number(min))
  assertthat::assert_that(assertthat::is.number(max))
  assertthat::assert_that(max > min)

  width = max - min
  y <- stats::rpois(1, lambda = rate*width)
  result <- stats::runif(y, min=min, max = max)

  return(result)
}
