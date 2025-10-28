#' Collapse vector with conjunction
#'
#' Paste together vector with final conjunction like "and", as well as an oxford comma
#'
#' @param x Usually a character vector, but can be any object that can be converted to a character vector and is subsettable
#' @param conjunction Conjunction to be used behind the oxford comma. Usually a character vector vector of length one, but can be any object that can be converted to a character vector
#' @returns Character vector of length 1
#' @export
#' @examples
#' paste_conjunction(c("a", "b", "c"))
#' paste_conjunction(c("a", "b", "c"), conjunction = "as well as")
paste_conjunction <- function(x, conjunction = "and") {
  x[length(x)] <- stringi::stri_join(conjunction, " ", x[length(x)])
  c <- stringi::stri_join(x, collapse = ", ")
  if (length(x) < 3) {
    c <- stringr::str_replace_all(
      c,
      stringi::stri_join(", ", conjunction),
      stringi::stri_join(" ", conjunction)
    )
  }
  return(c)
}

#' Spell out numbers
#'
#' Spell out and concatenate a numeric vector if any of the numbers are >10.
#' Vectors of length >1 are concatenated with an optional conjunction.
#' `spell_numbers()` wraps `xfun::numbers_to_words()` and adds some additional logic.
#'
#' @param x Numeric vector to be converted to text
#' @param conjunction Conjunction to be used behind the oxford comma. Usually a character vector vector of length one, but can be any object that can be converted to a character vector
#' @returns Character vector of length 1
#' @export
#' @examples
#' spell_numbers(2)
#' spell_numbers(20)
#' spell_numbers(c(1, 2, 3))
#' spell_numbers(c(1, 2, 30))
#' spell_numbers(c(1, 2, 3), conjunction = "and")
#' spell_numbers(c(1, 2, 30), conjunction = "and")
spell_numbers <- function(x, conjunction = NULL) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "!" = "{.arg x} must be numeric.",
        "x" = "You supplied {.type {x}}"
      )
    )
  }

  if (length(x) == 1) {
    x <- ifelse(
      x < 10,
      xfun::numbers_to_words(x, cap = FALSE, hyphen = TRUE, and = FALSE),
      as.character(x)
    )
  } else {
    if (all(x < 10)) {
      x <- purrr::map(x, spell_numbers) |>
        stringi::stri_join_list()
    }
    if (!is.null(conjunction)) {
      x <- paste_conjunction(x = x, conjunction = conjunction)
    } else {
      x <- stringi::stri_join(x, collapse = ", ")
    }
  }

  return(x)
}

#' Spell out range
#'
#' Spell out the range of a numeric vector if any of the numbers are >10.
#' `spell_range()` uses `xfun::numbers_to_words()` for number to text conversion.
#'
#' @param x Numeric vector to be converted to text
#' @returns Character vector of length 1
#' @export
#' @examples
#' spell_range(c(1, 2))
#' spell_range(c(1, 2, 3))
#' spell_range(c(1, 2, 3, 30))
#' spell_range(c(20, 25, 30))
spell_range <- function(x) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "!" = "{.arg x} must be numeric.",
        "x" = "You supplied {.type {x}}."
      )
    )
  }
  if (length(x) == 1) {
    cli::cli_abort(
      c(
        "!" = "{.arg x} must be of length \u2265 2.",
        "x" = "You supplied a vector of length {.val {1}}."
      )
    )
  }

  r <- range(x)
  if (any(r >= 10)) {
    r <- stringi::stri_join(r, collapse = "\u2013")
  } else {
    r <- purrr::map(r, spell_numbers) |>
      stringi::stri_join_list() |>
      stringi::stri_join(collapse = " to ")
  }

  return(r)
}
