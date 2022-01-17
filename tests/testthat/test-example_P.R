test_that("the dimension of P is correct", {
  expect_equal(dim(example_P(l_breaks = seq(0,10, length.out = 11),
                             r_breaks = seq(0,20, length.out = 21),
                             sd = 1,
                             n_quad = 100)),
               c(20,10))
})

test_that("the column sums of P are <= 1 (within tol = 1e-8)", {
  expect(all(colSums(example_P(l_breaks = seq(0,10, length.out = 11),
                               r_breaks = seq(0,20, length.out = 21),
                               sd = 1,
                               n_quad = 100)) <= 1+ 1e-8),
         failure_message = "The column sums of P exceed 1")
})

test_that("the entries of P are positive", {
  expect(all(example_P(l_breaks = seq(0,10, length.out = 11),
                       r_breaks = seq(0,20, length.out = 21),
                       sd = 1,
                       n_quad = 100) >= 0),
         failure_message = "Some entries of P are negative")
})
