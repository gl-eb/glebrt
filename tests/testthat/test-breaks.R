x <- seq(1, 21)

test_that("Breaks are computed correctly", {
  expect_equal(breaks_limits(x), c(1, 5, 10, 15, 21))
  expect_equal(breaks_limits(x = x, n = 3), c(1, 10, 21))
  expect_equal(breaks_limits(x = x, r = 0), c(0, 5, 10, 15, 20))
  expect_equal(breaks_interval(x, i = 10), c(0, 10, 20))
  expect_equal(breaks_interval(x = x, i = 5), c(0, 5, 10, 15, 20))
  expect_equal(breaks_interval(x = x, i = 3), c(0, 3, 6, 9, 12, 15, 18, 21))
})

test_that("Non-numeric arguments are rejected", {
  expect_error(
    breaks_limits(x = c("a", "b", "c")),
    regexp = "`x` must be numeric"
  )
  expect_error(
    breaks_limits(x = x, n = "3"),
    regexp = "`n` must be numeric"
  )
  expect_error(
    breaks_limits(x = x, r = "0"),
    regexp = "`r` must be numeric"
  )
  expect_error(
    breaks_limits(x = x, r = 10),
    regexp = "`r` must be between 0 and 1"
  )
  expect_error(
    breaks_interval(x = c("a", "b", "c")),
    regexp = "`x` must be numeric"
  )
  expect_error(
    breaks_interval(x = x, i = "5"),
    regexp = "`i` must be numeric"
  )
})
