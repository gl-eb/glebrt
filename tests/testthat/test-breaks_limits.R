x <- seq(1, 21)

test_that("breaks are computed correctly", {
  expect_equal(breaks_limits(x), c(1, 5, 10, 15, 21))
  expect_equal(breaks_limits(x = x, n = 3), c(1, 10, 21))
  expect_equal(breaks_limits(x = x, r = 0), c(0, 5, 10, 15, 20))
})

test_that("Arguments are checked", {
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
})
