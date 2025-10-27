vec_num <- c(1, 0.4, 0.001, 0.23, 0.170)

test_that("precision is detected correctly", {
  expect_equal(get_precision(vec_num), c(0, 1, 3, 2, 2))
})

test_that("rounding to precision works", {
  expect_equal(
    ceiling_precision(vec_num, precision = 1),
    c(1.0, 0.4, 0.1, 0.3, 0.2)
  )
  expect_equal(
    floor_precision(vec_num, precision = 1),
    c(1.0, 0.4, 0.00, 0.2, 0.1)
  )
})

test_that("precision is treated as integer", {
  expect_equal(
    floor_precision(0.25, precision = 2L),
    floor_precision(0.25, precision = 2.0)
  )
  expect_equal(
    ceiling_precision(0.25, precision = 2L),
    ceiling_precision(0.25, precision = 2.0)
  )
  expect_equal(
    floor_precision(0.25, precision = 2L),
    floor_precision(0.25, precision = 2.2)
  )
  expect_equal(
    ceiling_precision(0.25, precision = 2L),
    ceiling_precision(0.25, precision = 2.2)
  )
})

test_that("non-numeric input is rejected", {
  expect_error(get_precision(x = "0.25"))
  expect_error(floor_precision(x = "0.25"))
  expect_error(ceiling_precision(x = "0.25"))
  expect_error(floor_precision(x = 0.25, precision = "2L"))
  expect_error(ceiling_precision(x = 0.25, precision = "2L"))
})
