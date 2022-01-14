example_P <- function(l_breaks, r_breaks, sd, n_quad = 100){

  assertthat::assert_that(is.vector(l_breaks, mode = "numeric"),
                          is.vector(r_breaks, mode = "numeric"),
                          assertthat::is.number(sd),
                          n_quad%%1 == 0,
                          length(l_breaks) > 0,
                          length(r_breaks) > 0,
                          length(n_quad) == 1,
                          n_quad > 0)

  # Halfnorm CDF
  phnorm <- Vectorize(function(q, mean = 0, sd){
    if(q < mean){
      0
    }
    else{
      2*(stats::pnorm(q, mean = mean, sd = sd)-0.5)
    }
  })

  # Define the quadrature grid along the latent space defined by l_breaks
  quad_grid <- seq(from = min(l_breaks), to = max(l_breaks), length.out = n_quad)

  # 'cond' computes the inner integral in () wrt the observed coordinate r for a given l
  cond <- function(l, r_breaks , sd){
    a <- phnorm(r_breaks, mean = l, sd = sd)[-length(r_breaks)]
    b <- phnorm(r_breaks, mean = l, sd = sd)[-1]
    return(b-a)
  }

  # Quadrature
  width <- (max(l_breaks) - min(l_breaks))/n_quad
  result <- lapply(quad_grid, function(l) cond(l = l, r_breaks = r_breaks, sd = sd)*width)
  result <- matrix(unlist(result), nrow = n_quad, byrow = TRUE)

  P <- rowsum(result, cut(quad_grid, breaks = l_breaks, include.lowest = TRUE))

  P <- t(P)

  # Add interval labels to the rows (the observed space)
  rownames(P) <- paste0("(", r_breaks[-length(r_breaks)], ",", r_breaks[-1] , "]")

  return(P)

}
