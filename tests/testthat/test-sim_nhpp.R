test_that("output is numeric vector", {
  expect_vector(sim_nhpp(lambda = function(x) abs(sin(x)), min = 0, max = 10, M = 1), ptype = numeric())
})

