test_that("output dimensions are correct", {

  l_min <- 0
  l_max <- 100
  l_wd <- 0.5
  r_min <- 0
  r_max <- 100
  r_wd <- 1
  height <- 1000
  sd <- 10
  M <- 50

  result <- example_sim(l_min = l_min, l_max = l_max, l_wd = l_wd,
                        r_min = r_min, r_max = r_max, r_wd = r_wd,
                        height = height, sd = sd, M = M)

  expect_equal( length(result$y), 100)
  expect_equal( dim(result$P), c(100, 200))
  expect_equal( length(result$l_breaks), 201)
  expect_equal( length(result$r_breaks), 101)
  expect_equal( length(result$l_grid), 200)
  expect_equal( length(result$r_grid), 100)
  expect_equal( length(result$truth), 200)
})

test_that("example_P works when M = NULL", {

  l_min <- 0
  l_max <- 100
  l_wd <- 0.5
  r_min <- 0
  r_max <- 100
  r_wd <- 1
  height <- 1000
  sd <- 10
  M <- NULL

  result <- example_sim(l_min = l_min, l_max = l_max, l_wd = l_wd,
                        r_min = r_min, r_max = r_max, r_wd = r_wd,
                        height = height, sd = sd, M = M)

  expect_equal( length(result$y), 100)
  expect_equal( dim(result$P), c(100, 200))
  expect_equal( length(result$l_breaks), 201)
  expect_equal( length(result$r_breaks), 101)
  expect_equal( length(result$l_grid), 200)
  expect_equal( length(result$r_grid), 100)
  expect_equal( length(result$truth), 200)
})
