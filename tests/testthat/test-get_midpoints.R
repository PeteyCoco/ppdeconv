test_that("get_midpoints works", {
  expect_equal(get_midpoints(c(2, 0, 8, 5, 3)), c(1, 2.5, 4, 6.5))
})
