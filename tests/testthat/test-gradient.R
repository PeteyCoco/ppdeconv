test_that("gradient function outputs have proper dimension.", {
  # Simulate data
  example <- example_hnorm(
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

  # Parameter
  a <- rep(0,10)

  ldot <- ldot(example, a)
  sdot <- sdot(example, a)
  grad <- gradient(example, a)

  # Check the length of the gradients
  expect_length(ldot, 10)
  expect_length(sdot, 10)
  expect_length(grad, 10)

})
