test_that("ppdeconv converges for a fixed mle example", {
  # Simulate one realization from the example intensity
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

  # Stack data into lists
  data <- list(example, example)

  result <- ppdeconv(a0 = NULL, data = data, mode = "fixed", method = "mle")

  expect_equal(result$fit$convergence, 0)
})

test_that("ppdeconv converges for a variable mle example", {
  # Simulate one realization from the example intensity
  example <- example_exp(
    l_min = 0,
    l_max = 100,
    l_wd = 0.5,
    r_min = 0,
    r_max = 100,
    r_wd = 1,
    height = 1000,
    rate = 1/5,
    df = 10,
    c0 = 1,
    M = NULL,
  )

  # Stack data into lists
  data <- list(example, example)

  result <- ppdeconv(a0 = NULL, data = data, mode = "variable", method = "mle")

  expect_equal(result$fit$convergence, 0)
})
