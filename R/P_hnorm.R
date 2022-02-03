#' P matrix for half-normal example
#'
#' @param l_breaks a (m+1) vector defining a partition of the interval L into m sets
#' @param r_breaks a (n+1) vector defining a partition of the interval R into n sets
#' @param sd the standard deviation of the half-normal distribution
#'
#' @return the n x m matrix P of transition probabilities
#' @export
#'
#' @examples
#' # Compute P for half-normal measurement error with sd = 2
#' # The latent interval L = [0,10] is divided into 20 intervals of length 0.5
#' # The observed interval R = [0,15] is divided into 15 intervals of length 1.
#' l_breaks <- seq(0,10, length.out = 21)
#' r_breaks <- seq(0,15, length.out = 16)
#' P <- P_hnorm(l_breaks, r_breaks, sd = 2)
P_hnorm <- function(l_breaks, r_breaks, sd) {
  assertthat::assert_that(
    is.vector(l_breaks, mode = "numeric"),
    is.vector(r_breaks, mode = "numeric"),
    assertthat::is.number(sd),
    length(l_breaks) > 0,
    length(r_breaks) > 0
  )

  # Define the quadrature grid along the latent space defined by l_breaks
  l_grid <- get_midpoints(l_breaks)
  l_wd <- diff(l_breaks)

  # Perform Quadrature for the latent space integration
  result <-
    lapply(l_grid, function(l)
      cond_hnorm(l = l, r_breaks = r_breaks, sd = sd))
  P <- matrix(unlist(result), nrow = length(l_grid), byrow = TRUE)
  P <- l_wd * P

  P <- t(P)

  # Add interval labels to the rows (the observed space)
  rownames(P) <-
    paste0("(", r_breaks[-length(r_breaks)], ",", r_breaks[-1] , "]")
  colnames(P) <-
    paste0("(", l_breaks[-length(l_breaks)], ",", l_breaks[-1] , "]")

  return(P)

}


#' CDF of Half-normal distribution
#'
#' @param q vector of quantiles
#' @param mean scalar of mean
#' @param sd the standard deviation of the half-normal distribution
#'
#' @return the CDF for the given vector of quantiles `q`
phnorm <- Vectorize(function(q, mean = 0, sd) {
  if (q < mean) {
    0
  }
  else{
    2 * (stats::pnorm(q, mean = mean, sd = sd) - 0.5)
  }
})


#' `cond_hnorm` helper function
#'
#' 'cond_hnorm' computes the inner integral in () wrt the observed coordinate `r` for a given the latent coordinate `l`.
#' This is a helper function for `P_hnorm` and should not be used independently.
#'
#' @param l the given value of the latent variable
#' @param r_breaks a (n+1) vector defining a partition of the interval R into n sets
#' @param sd the standard deviation of the half-normal distribution
#'
#' @return an n vector whose i^th entry is P_{ij} as defined above.
cond_hnorm <- function(l, r_breaks , sd) {
  a <- phnorm(r_breaks, mean = l, sd = sd)[-length(r_breaks)]
  b <- phnorm(r_breaks, mean = l, sd = sd)[-1]
  return(b - a)
}
