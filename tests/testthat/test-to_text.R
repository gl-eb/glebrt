x_chr <- letters[1:3]
x_num <- 1:3

test_that("Concatenating vector works", {
  expect_equal(paste_conjunction(x_chr), "a, b, and c")
  expect_equal(
    paste_conjunction(x_chr, conjunction = "as well as"),
    "a, b, as well as c"
  )
  expect_equal(paste_conjunction(x_num), "1, 2, and 3")
  expect_equal(paste_conjunction(c(TRUE, TRUE, FALSE)), "TRUE, TRUE, and FALSE")
  expect_equal(
    paste_conjunction(x_chr, conjunction = 2),
    "a, b, 2 c"
  )
})

test_that("Spelling out numbers works", {
  expect_equal(spell_numbers(2), "two")
  expect_equal(spell_numbers(20), "20")
  expect_equal(spell_numbers(x_num), "one, two, three")
  expect_equal(spell_numbers(c(1, 2, 30)), "1, 2, 30")
  expect_equal(
    spell_numbers(x_num, conjunction = "and"),
    "one, two, and three"
  )
  expect_equal(spell_numbers(c(1, 2, 30), conjunction = "and"), "1, 2, and 30")
})

test_that("Number spelling rejects non-numeric argument", {
  expect_error(spell_numbers("a"), regexp = "`x` must be numeric.")
  expect_error(spell_numbers(x_chr), regexp = "`x` must be numeric.")
})

test_that("Spelling out ranges works", {
  expect_equal(spell_range(c(1, 2)), "one to two")
  expect_equal(spell_range(x_num), "one to three")
  expect_equal(
    spell_range(c(1, 2, 3, 30)),
    stringi::stri_join("1", "\u2013", "30")
  )
  expect_equal(
    spell_range(c(20, 25, 30)),
    stringi::stri_join("20", "\u2013", "30")
  )
})

test_that("Range spelling rejects non-conforming argument", {
  expect_error(spell_range(c("a", "b")), regexp = "`x` must be numeric.")
  expect_error(spell_range(1), regexp = "`x` must be of length \u2265 2.")
})
