test_that("output dimensions are correct", {
  l_min <- 0
  l_max <- 100
  l_wd <- 0.5
  r_min <- 0
  r_max <- 100
  r_wd <- 1
  height <- 1000
  rate <- 1
  df <- 10
  c0 <- 1
  M <- 50

  result <- example_exp(
    l_min = l_min,
    l_max = l_max,
    l_wd = l_wd,
    r_min = r_min,
    r_max = r_max,
    r_wd = r_wd,
    height = height,
    rate = rate,
    df = df,
    c0 = c0,
    M = M,
  )

  expect_true("ppdeconvVar" %in% class(result))
  expect_length(result$N, 100)
  expect_equal(dim(result$P), c(100, 200))
  expect_equal(dim(result$Q), c(200, 10))
  expect_equal(dim(result$S), c(10, 10))
  expect_length(result$l_breaks, 201)
  expect_length(result$r_breaks, 101)
  expect_length(result$l_grid, 200)
  expect_length(result$r_grid, 100)
  expect_length(result$a, 10)
})

test_that("example_P works when M = NULL", {
  l_min <- 0
  l_max <- 100
  l_wd <- 0.5
  r_min <- 0
  r_max <- 100
  r_wd <- 1
  height <- 1000
  rate <- 1
  df <- 10
  c0 <- 1
  M <- NULL

  result <- example_exp(
    l_min = l_min,
    l_max = l_max,
    l_wd = l_wd,
    r_min = r_min,
    r_max = r_max,
    r_wd = r_wd,
    height = height,
    rate = rate,
    df = df,
    c0 = c0,
    M = M
  )

  expect_true("ppdeconvVar" %in% class(result))
  expect_length(result$N, 100)
  expect_equal(dim(result$P), c(100, 200))
  expect_equal(dim(result$Q), c(200, 10))
  expect_equal(dim(result$S), c(10, 10))
  expect_length(result$l_breaks, 201)
  expect_length(result$r_breaks, 101)
  expect_length(result$l_grid, 200)
  expect_length(result$r_grid, 100)
  expect_length(result$a, 10)
})
