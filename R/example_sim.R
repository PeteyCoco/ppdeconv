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
example_lambda <- function(x, height) {
  assertthat::assert_that(is.vector(x, mode = "numeric"),
                          assertthat::is.number(height),
                          height >= 0)

  height * (0.6 * stats::dnorm(x, mean = 30, sd = 10) +
              0.4 * stats::dnorm(x, mean = 70, sd = 10))
}

example_hnorm <-
  function(l_min,
           l_max,
           l_wd,
           r_min,
           r_max,
           r_wd,
           sd,
           df,
           c0,
           height,
           M = NULL,
           seed = NULL) {
    assertthat::assert_that(l_min < l_max,
                            r_min < r_max,
                            sd >= 0,
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

    noise <- abs(stats::rnorm(length(sample), mean = 0, sd = sd))

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

    P <- example_P(l_breaks = l_breaks,
                   r_breaks = r_breaks,
                   sd = sd)

    return(
      new_ppdeconvFix(
        N = N,
        Q = Q,
        P = P,
        S = S,
        c0 = c0,
        l_breaks = l_breaks,
        r_breaks = r_breaks,
        l_grid = l_grid,
        r_grid = r_grid,
      )
    )
  }

example_bernvar <-
  function(l_min,
           l_max,
           l_wd,
           r_min,
           r_max,
           r_wd,
           df,
           c0,
           b,
           height,
           M = NULL,
           seed = NULL) {
    assertthat::assert_that(l_min < l_max,
                            r_min < r_max,
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

  }
