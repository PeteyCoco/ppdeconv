test_that("sum of result does not exceed 1", {
  l <- 0
  r_breaks <- seq(-5, 5, length.out = 100)
  r_grid <- get_midpoints(r_breaks)
  r_wd <- diff(r_breaks)
  result <- cond(l, r_grid, sd = 1)

  expect_lte(sum(result), 1)
})
