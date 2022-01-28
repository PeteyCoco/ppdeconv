test_that("gradient functions works in bdiag case", {

  # Simulate data
  example <- example_sim(l_min = 0, l_max = 100, l_wd = 0.5,
                         r_min = 0, r_max = 100, r_wd = 1,
                         sd = 10, df = 10, height = 1000, M = NULL, seed = 235)

  # Stack data into lists
  obs <- list( y = example$y, Q = Q, P = example$P, S = S, c0 = c0)
  data <- list(obs, obs)

  # create the bdiag matrices
  Q_bdiag <- Matrix::bdiag(lapply(data, function(x) x$Q))
  P_bdiag <- Matrix::bdiag(lapply(data, function(x) x$P))
  S_bdiag <- Matrix::bdiag(lapply(data, function(x) x$S))

  y_bdiag <- unlist(lapply(data, function(x) x$y))
  a <- rep(0, times = ncol(Q_bdiag))

  ldot <- ldot(a = a, y = y_bdiag, Q = Q_bdiag, P = P_bdiag, S = S_bdiag, c0 = c0)
  sdot <- sdot(a = a, y = y_bdiag, Q = Q_bdiag, P = P_bdiag, S = S_bdiag, c0 = c0)
  grad <- gradient(a = a, y = y_bdiag, Q = Q_bdiag, P = P_bdiag, S = S_bdiag, c0 = c0)

  # Check the length of the gradients
  expect_length(ldot, 20)
  expect_length(sdot, 20)
  expect_length(grad, 20)

  # The gradients should be symettric
  expect_equal(ldot[1:10], ldot[11:20])
  expect_equal(sdot[1:10], sdot[11:20])
  expect_equal(grad[1:10], grad[11:20])
})
