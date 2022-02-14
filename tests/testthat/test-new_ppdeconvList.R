test_that("The constructor works for mixed types", {

  # Make a list of one ppdeconvFix and ppdeconvVar object
  fixed <- example_hnorm(
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
  variable <- example_exp(
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
    seed = 32
  )

  data <- list(fixed, variable)
  expect_equal(class(new_ppdeconvList(data)), "ppdeconvList")
})

test_that("The constructor throws error when the wrong object is passed",{

  fixed <- example_hnorm(
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

  # A list containing the wrong objects
  data <- list(sin, fixed)
  expect_error(new_ppdeconvList(data))
})
