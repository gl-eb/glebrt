test_that("tests of logical vectors work", {
  expect_equal(some(c(TRUE, FALSE, FALSE)), TRUE)
  expect_equal(some(c(TRUE, TRUE, TRUE)), FALSE)
  expect_equal(some(c(FALSE, FALSE, FALSE)), FALSE)
  expect_equal(same(c(TRUE, FALSE, FALSE)), FALSE)
  expect_equal(same(c(TRUE, TRUE, TRUE)), TRUE)
  expect_equal(same(c(FALSE, FALSE, FALSE)), TRUE)
})

test_that("non-logical objects are rejected", {
  expect_error(some(c(1, 0, 0)), regexp = "must be logical")
  expect_error(some("TRUE"), regexp = "must be logical")
  expect_error(same(c(1, 0, 0)), regexp = "must be logical")
  expect_error(same("TRUE"), regexp = "must be logical")
})
