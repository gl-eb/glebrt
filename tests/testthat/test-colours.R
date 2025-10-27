test_that("greys are returned", {
  expect_equal(get_greys(), c("grey95", "grey75", "grey50", "grey25", "grey5"))
  expect_equal(
    get_greys(NULL),
    c("grey95", "grey75", "grey50", "grey25", "grey5")
  )
  expect_equal(get_greys(1:3), c("grey95", "grey75", "grey50"))
  expect_equal(get_greys(c(1, 5)), c("grey95", "grey5"))
})

test_that("non-numeric argument is refused", {
  expect_error(get_greys("1"))
  expect_error(get_greys(TRUE))
})
