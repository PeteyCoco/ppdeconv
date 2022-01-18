test_that("sum of result does not exceed 1", {

  l <- 0
  r_breaks <- seq(-5, 5, length.out = 100)
  result <- cond(l, r_breaks, sd = 1)

  expect_lte( sum(result), 1)
})
