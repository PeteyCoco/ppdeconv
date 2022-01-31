test_that("dimension of Hessian functions are correct", {
  example <- example_sim(
    l_min = 0,
    l_max = 100,
    l_wd = 0.5,
    r_min = 0,
    r_max = 100,
    r_wd = 1,
    sd = 10,
    df = 10,
    c0 = 1,
    height = 1000,
    M = NULL,
    seed = 235
  )

  a <- rep(0, 10)

  expect_equal(dim(lddot(example, a)), c(10, 10))
  expect_equal(dim(sddot(example, a)), c(10, 10))
  expect_equal(dim(hessian(example, a)), c(10, 10))
})
