test_that("input and output length match", {
  expect_length(example_lambda(seq(0,100, length.out = 20), height = 100), 20)
})

test_that("output length is 0 when input is numeric(0)", {
  expect_length(example_lambda(numeric(0), height = 100) , 0)
})

test_that("output is NA when input is NA", {
  expect_true(is.na(example_lambda(c(NA, 1), height = 100)[1]))
})

test_that("result is non-negative", {
  expect_true(all(example_lambda(seq(0,100, length.out = 20), height = 100) >= 0))
})
