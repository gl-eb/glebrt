dat <- tibble::tibble(
  var = letters[1:5],
  value1 = 1:5,
  value2 = rep(NA, 5),
  value3 = rep(NA, 5)
)

test_that("NA columns are deselected while keeping some", {
  expect_equal(deselect_na(dat), dat[c("var", "value1")])
  expect_equal(
    deselect_na(dat, keep = "value2"),
    dat[c("var", "value1", "value2")]
  )
  expect_equal(deselect_na(dat, keep = colnames(dat)), dat)
})

test_that("Non-conforming arguments are rejected", {
  expect_error(deselect_na(
    list(dat$var, dat$value1, dat$value2),
    keep = "value5"
  ))
  expect_error(deselect_na(dat, keep = "value5"))
})
