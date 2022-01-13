test_that("output is numeric vector", {
  expect_vector(sim_hpp(rate = 10, min = 0, max = 1), ptype = numeric())
})
