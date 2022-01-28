test_that("dimension of gradient functions are correct", {

  example <- example_sim(l_min = 0, l_max = 100, l_wd = 0.5,
                         r_min = 0, r_max = 100, r_wd = 1,
                         sd = 10, height = 1000, M = NULL, seed = 235)

  df <- 20

  a <- rep(1, times = df)

  smooth <- mgcv::smoothCon(mgcv::s(l_grid, k = df, bs = "ps", m = c(2,3)),
                            data = data.frame(l_grid = example$l_grid))
  Q <- smooth[[1]]$X
  S <- smooth[[1]]$S[[1]]

  c0 <- 1

  expect_length(ldot(a = a, y = example$y, Q = Q, P = example$P, S = S, c0 = c0), df)
  expect_length(sdot(a = a, y = example$y, Q = Q, P = example$P, S = S, c0 = c0), df)
  expect_length(gradient(a = a, y = example$y, Q = Q, P = example$P, S = S, c0 = c0), df)
})

test_that("dimension of Hessian functions are correct", {

  example <- example_sim(l_min = 0, l_max = 100, l_wd = 0.5,
                         r_min = 0, r_max = 100, r_wd = 1,
                         sd = 10, height = 1000, M = NULL, seed = 235)

  df <- 20

  a <- rep(1, times = df)

  smooth <- mgcv::smoothCon(mgcv::s(l_grid, k = df, bs = "ps", m = c(2,3)),
                            data = data.frame(l_grid = example$l_grid))
  Q <- smooth[[1]]$X
  S <- smooth[[1]]$S[[1]]

  c0 <- 1

  expect_equal(dim(lddot(a = a, y = example$y, Q = Q, P = example$P, S = S, c0 = c0)), c(df,df))
  expect_equal(dim(sddot(a = a, y = example$y, Q = Q, P = example$P, S = S, c0 = c0)), c(df,df))
  expect_equal(dim(hessian(a = a, y = example$y, Q = Q, P = example$P, S = S, c0 = c0)), c(df,df))
})
