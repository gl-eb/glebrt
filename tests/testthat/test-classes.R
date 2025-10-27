dat_2 <- tibble::tibble(
  var = c("a", "b", "c"),
  value = c(1, 2, 3)
)
dat_3 <- tibble::tibble(
  var = c("a", "b", "c"),
  value1 = c(1, 2, 3),
  value2 = c(20, 30, 10)
)

test_that("Class is set correctly", {
  expect_equal(
    set_class(x = c(1, 2, 3), class = "integer"),
    as.integer(c(1, 2, 3))
  )
})

test_that("Deframing with class works", {
  expect_equal(
    deframe_with_class(dat = dat_2, class = "integer"),
    list("a" = 1, "b" = 2, "c" = 3) |> purrr::map(as.integer)
  )
})

test_that("Non-conforming arguments are rejected", {
  expect_error(deframe_with_class(dat = c(1, 2, 3), class = "integer"))
  expect_error(deframe_with_class(dat = dat_3, class = "integer"))
})
