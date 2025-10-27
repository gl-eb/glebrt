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

test_that("Spelling out numbers works", {
  expect_equal(spell_numbers(2), "two")
  expect_equal(spell_numbers(20), "20")
  expect_equal(spell_numbers(c(1, 2, 3)), "one, two, three")
  expect_equal(spell_numbers(c(1, 2, 30)), "1, 2, 30")
  expect_equal(
    spell_numbers(c(1, 2, 3), conjunction = "and"),
    "one, two, and three"
  )
  expect_equal(spell_numbers(c(1, 2, 30), conjunction = "and"), "1, 2, and 30")
})

test_that("Number spelling rejects non-numeric argument", {
  expect_error(spell_numbers("a"), regexp = "`x` must be numeric.")
  expect_error(spell_numbers(c("a", "b", "c")), regexp = "`x` must be numeric.")
})
