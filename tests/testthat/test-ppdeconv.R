test_that("MLE fit works with block diagonal matrices", {

  # Simulate one realization from the example intensity
  example <- example_sim(l_min = 0, l_max = 100, l_wd = 0.5,
                         r_min = 0, r_max = 100, r_wd = 1,
                         sd = 10, height = 1000, n_quad = 5, M = NULL, seed = 235)

  # Define model
  df <- 10

  smooth <- mgcv::smoothCon(mgcv::s(l_grid, k = df, bs = "ps", m = c(2,3)),
                            data = data.frame(l_grid = example$l_grid))
  Q <- smooth[[1]]$X
  S <- smooth[[1]]$S[[1]]

  c0 <- 1

  # Stack data into lists
  obs <- list( y = example$y, Q = Q, P = example$P, S = S, c0 = c0)
  data <- list(obs, obs)

  fit <- ppdeconv(a0 = NULL, data = data, mode = "mle")

  expect_equal(fit$convergence, 0)
})
