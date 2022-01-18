#' Intensity function for example NHPP
#'
#' @param x numeric n-vector giving coordinates to evaluate the intensity
#' @param height scaling parameter equal to expected number of observations over \[0,100\]
#'
#' @return n-vector giving the value of the intensity
#' @export
#'
#' @examples
#' # Evaluate example_lambda over a uniform grid
#' l_grid <- seq(0,100, length.out = 20)
#' height <- 25000
#' example_lambda(l_grid, height)
example_lambda <- function(x, height){

  assertthat::assert_that(is.vector(x, mode = "numeric"),
                          assertthat::is.number(height),
                          height >= 0)

    height*(0.6*stats::dnorm(x, mean = 30, sd = 10) +
              0.4*stats::dnorm(x, mean = 70, sd = 10))
  }
#
# example_sim <- function(obs_min = 0, obs_max = 100, sd = 10, M = 600, seed = NULL){
#
#   if(!is.null(seed)){
#     set.seed(seed)
#   }
#
#   sample <- sim_nhpp(function(x) example_lambda(x, height=25000), M = M, from = obs_min, to = obs_max)
#
#   noise <- abs(rnorm(length(sample), mean = 0, sd = sd))
#
#   obs <- sample + noise
#
#   obs <- obs[(obs > obs_min) & (obs <= obs_max)]
#
#   set.seed(Sys.time())
#
#   l_breaks <- seq(from = obs_min, to = obs_max, by = 1)
#   r_breaks <- seq(from = obs_min, to = obs_max, by = 1)
#
#   l_grid <- get_midpoints(l_breaks)
#   r_grid <- get_midpoints(r_breaks)
#
#   y <- table(cut(obs, breaks = r_breaks, include.lowest = TRUE))
#   y <- as.integer(y)
#
#   P <- example_P(l_breaks, r_breaks, width = 0.1, sd = sd)
#
#   return(list(y = y,
#               P = P,
#               l_breaks = l_breaks,
#               r_breaks = r_breaks,
#               l_grid = l_grid,
#               r_grid = r_grid,
#               truth = example_lambda(l_grid, height=25000)))
# }
