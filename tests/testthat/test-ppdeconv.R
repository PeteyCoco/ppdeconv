test_that("MLE fit works with block diagonal matrices", {
  # Simulate one realization from the example intensity
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

  # Stack data into lists
  data <- list(example, example)

  fit <- ppdeconv(a0 = NULL, data = data, mode = "fixed", method = "mle")

  expect_equal(fit$convergence, 0)
})
