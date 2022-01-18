test_that("the dimension of P is correct", {
  # Test using the length.out argument
  l_breaks = seq(0,100, length.out = 201)
  r_breaks = seq(0,100, length.out = 101)

  P <- example_P(l_breaks = l_breaks,
                 r_breaks = r_breaks,
                 sd = 10,
                 n_quad = 5)

  expect_equal(dim(P), c( 100, 200))

  # Test using the by argument
  l_breaks <- seq(0, 100, by = 0.5)
  r_breaks <- seq(0, 100, by = 1)

  P <- example_P(l_breaks = l_breaks,
                 r_breaks = r_breaks,
                 sd = 10,
                 n_quad = 5)

  expect_equal(dim(P), c( 100, 200))
})

# test_that("the column sums of P are <= 1 (within tol = 1e-8)", {
#
#   l_breaks = seq(0,10, length.out = 11)
#   r_breaks = seq(0,20, length.out = 21)
#
#   P <- example_P(l_breaks = seq(0,10, length.out = 11),
#                  r_breaks = seq(0,20, length.out = 21),
#                  sd = 1,
#                  n_quad = 5)
#
#   expect( all( rowSums(P) <= 1+ 1e-8),
#          failure_message = "The rowsums of P exceed 1")
# })

test_that("the entries of P are positive", {
  expect(all(example_P(l_breaks = seq(0,10, length.out = 11),
                       r_breaks = seq(0,20, length.out = 21),
                       sd = 1,
                       n_quad = 5) >= 0),
         failure_message = "Some entries of P are negative")
})
