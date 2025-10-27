test_that("Concatenating vector works", {
  expect_equal(paste_conjunction(c("a", "b", "c")), "a, b, and c")
  expect_equal(
    paste_conjunction(c("a", "b", "c"), conjunction = "as well as"),
    "a, b, as well as c"
  )
  expect_equal(paste_conjunction(c(1, 2, 3)), "1, 2, and 3")
  expect_equal(paste_conjunction(c(TRUE, TRUE, FALSE)), "TRUE, TRUE, and FALSE")
  expect_equal(
    paste_conjunction(c("a", "b", "c"), conjunction = 2),
    "a, b, 2 c"
  )
})
