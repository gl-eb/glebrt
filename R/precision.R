#' Get Precision of Number
#'
#' Get the numeric precision of numbers (i.e. the number of significant digits)
#'
#' @param x (numeric) Vector to get precision of
#' @export
#' @examples
#' x <- c(1, 0.4, 0.001, 0.23, 0.170)
#' get_precision(x)

get_precision <- function(x) {
  # check if input is numeric
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "!" = "{.arg x} must be numeric.",
        "x" = "You supplied {.type {x}}"
      )
    )
  }

  # get indexes of integers in vector
  i_int <- !stringr::str_detect(x, "\\.")

  # add decimal point to integers
  for (i in which(i_int %in% TRUE)) {
    x[i] <- paste0(x[i], ".")
  }

  # separate numbers into decimal digits
  x |>
    stringr::str_replace_all("\\d+\\.", "") |>
    purrr::map(\(c) stringr::str_split_1(c, "(?<=\\d)(?=\\d)")) |>
    purrr::map(\(n) ifelse(n == "", 0, length(n))) |>
    purrr::map(unique) |>
    unlist()
}

#' Rounding of Numbers With Precision
#'
#' `ceiling_precision` rounds up the nearest number of specified decimal places.
#' `floor_precision` rounds down the nearest number of specified decimal places.
#'
#' @param x (numeric) Vector to round
#' @param precision (numeric) Integer indicating the precision to which to round to. Non-integer numerics will be passed to `as.integer()`.
#' @return A numeric vector of the same length as `x`
#' @examples
#' x <- c(0.001, 0.23, 0.170)
#' ceiling_precision(x, precision = 2)
#' floor_precision(x, precision = 2)

#' @rdname round_precision
#' @export
ceiling_precision <- function(x, precision = 2L) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "!" = "{.arg x} must be numeric.",
        "x" = "You supplied {.type {x}}."
      )
    )
  }
  if (!is.numeric(precision)) {
    cli::cli_abort(
      c(
        "!" = "{.arg precision} must be numeric.",
        "x" = "You supplied {.type {x}}."
      )
    )
  } else if (is.numeric(precision) & !is.integer(precision)) {
    precision <- as.integer(precision)
  }

  ceiling(x * (10^precision)) / (10^precision)
}

#' @rdname round_precision
#' @export
floor_precision <- function(x, precision = 2L) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "!" = "{.arg x} must be numeric.",
        "x" = "You supplied {.type {x}}."
      )
    )
  }
  if (!is.numeric(precision)) {
    cli::cli_abort(
      c(
        "!" = "{.arg precision} must be numeric.",
        "x" = "You supplied {.type {x}}."
      )
    )
  } else if (is.numeric(precision) & !is.integer(precision)) {
    precision <- as.integer(precision)
  }

  floor(x * (10^precision)) / (10^precision)
}
