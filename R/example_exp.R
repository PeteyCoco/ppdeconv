#' Title
#'
#' @param l_min
#' @param l_max
#' @param l_wd
#' @param r_min
#' @param r_max
#' @param r_wd
#' @param rate
#' @param df
#' @param c0
#' @param height
#' @param M
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
example_exp <-
  function(l_min,
           l_max,
           l_wd,
           r_min,
           r_max,
           r_wd,
           rate,
           df,
           c0,
           height,
           M = NULL,
           seed = NULL) {
    assertthat::assert_that(l_min < l_max,
                            r_min < r_max,
                            rate >= 0,
                            height >= 0,
                            l_wd > 0,
                            r_wd > 0)
    if (!is.null(seed)) {
      set.seed(seed)
    }

    sample <-
      sim_nhpp(
        function(x)
          example_lambda(x, height),
        M = M,
        min = l_min,
        max = l_max
      )

    noise <- rexp(n = length(sample), rate = rate)

    obs <- sample + noise

    obs <- obs[(obs > r_min) & (obs <= r_max)]

    set.seed(Sys.time())

    l_breaks <- seq(from = l_min, to = l_max, by = l_wd)
    r_breaks <- seq(from = r_min, to = r_max, by = r_wd)

    l_grid <- get_midpoints(l_breaks)
    r_grid <- get_midpoints(r_breaks)

    N <- table(cut(obs, breaks = r_breaks, include.lowest = TRUE))
    N <- as.integer(N)

    # Setup model
    smooth <-
      mgcv::smoothCon(mgcv::s(
        l_grid,
        k = df,
        bs = "ps",
        m = c(2, 3)
      ),
      data = data.frame(l_grid = l_grid))
    Q <- smooth[[1]]$X
    S <- smooth[[1]]$S[[1]]

    # Define the function to compute new P
    P_fn <- function(b){
      P_exp(l_breaks = l_breaks,
            r_breaks = r_breaks,
            rate = exp(b))
    }

    return(
      new_ppdeconvObj(
        N = N,
        Q = Q,
        P_fn = P_fn,
        S = S,
        c0 = c0,
        l_breaks = l_breaks,
        r_breaks = r_breaks,
        l_grid = l_grid,
        r_grid = r_grid,
        b_idx = 1,
        p = c(rep(0, ncol(Q)), rate)
      )
    )
  }

#' Title
#'
#' @param l_breaks a (m+1) vector defining a partition of the interval L into m sets
#' @param r_breaks a (n+1) vector defining a partition of the interval R into n sets
#' @param rate the rate of the exponential distribution
#'
#' @return the n x m matrix P of transition probabilities
#' @export
#'
#' @examples
#' # Compute P for exponential measurement error with rate = 1
#' # The latent interval L = [0,10] is divided into 20 intervals of length 0.5
#' # The observed interval R = [0,15] is divided into 15 intervals of length 1.
#' l_breaks <- seq(0,10, length.out = 21)
#' r_breaks <- seq(0,15, length.out = 16)
#' P <- P_exp(l_breaks, r_breaks, rate = 1)
P_exp <- function(l_breaks, r_breaks, rate, n_quad = 1) {
  assertthat::assert_that(
    is.vector(l_breaks, mode = "numeric"),
    is.vector(r_breaks, mode = "numeric"),
    assertthat::is.number(rate),
    length(l_breaks) > 0,
    length(r_breaks) > 0
  )

  # Define the quadrature grid along the latent space defined by l_breaks
  l_quad <- sort(c(l_breaks, get_midpoints(l_breaks, q = n_quad)))
  l_wd <- diff(l_quad)
  l_quad <- l_quad[-length(l_quad)]

  # Perform Quadrature for the latent space integration
  result <-
    lapply(l_quad, function(l)
      cond_exp(
        l = l,
        r_breaks = r_breaks,
        rate = rate
      ))
  P <- matrix(unlist(result), nrow = length(l_quad), byrow = TRUE)
  P <- l_wd * P
  P <-
    rowsum(P,
           cut(
             l_quad,
             breaks = l_breaks,
             include.lowest = TRUE,
             right = FALSE
           ))

  P <- t(P)

  # Add interval labels to the rows (the observed space)
  rownames(P) <-
    paste0("(", r_breaks[-length(r_breaks)], ",", r_breaks[-1] , "]")
  colnames(P) <-
    paste0("(", l_breaks[-length(l_breaks)], ",", l_breaks[-1] , "]")

  return(P)

}

#' `cond_exp` helper function
#'
#' 'cond_exp' computes the inner integral in () wrt the observed coordinate `r` for a given the latent coordinate `l`.
#' This is a helper function for `P_exp` and should not be used independently.
#'
#' @param l the given value of the latent variable
#' @param r_breaks a (n+1) vector defining a partition of the interval R into n sets
#' @param rate the rate of the exponential distribution
#'
#' @return an n vector whose i^th entry is P_{ij} as defined above.
cond_exp <- function(l, r_breaks , rate) {
  a <- pexp(r_breaks - l, rate = rate)[-length(r_breaks)]
  b <- pexp(r_breaks - l, rate = rate)[-1]
  return(b - a)
}
