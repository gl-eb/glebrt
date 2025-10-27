dat <- tibble::tibble(var = c("a", "b", "c", "b"))

test_that("enumeration works", {
  expect_equal(enumerate(dat, var), 3)
})

test_that("data masking", {
  expect_equal(enumerate(dat, "var"), 3)
})

test_that("Non-conforming arguments are rejected", {
  expect_error(enumerate(dat = c("a", "b", "c", "b")))
  expect_error(enumerate(dat = dat, column = dat))
  expect_error(enumerate(dat = dat, column = "column"))
})
