#' Tests for logical vectors
#'
#' @description
#' `some_of()` checks if some but not all elements of a logical vector are TRUE.
#'
#' `same()` checks if all elements of a logical vector are the same.
#'
#' @param x (logical) Vector to be tested
#' @export
#' @examples
#' some_of(c(TRUE, FALSE, FALSE))
#' some_of(c(TRUE, TRUE, TRUE))
#' some_of(c(FALSE, FALSE, FALSE))
#'
#' same(c(TRUE, FALSE, FALSE))
#' same(c(TRUE, TRUE, TRUE))
#' same(c(FALSE, FALSE, FALSE))
some_of <- function(x) {
  if (!is.logical(x)) {
    cli::cli_abort(c(
      "!" = "{.arg x} must be logical",
      "x" = "You supplied {.type {x}}"
    ))
  }
  if (any(x) & !all(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @rdname some_of
#' @export
same <- function(x) {
  if (!is.logical(x)) {
    cli::cli_abort(c(
      "!" = "{.arg x} must be logical",
      "x" = "You supplied {.type {x}}"
    ))
  }
  if (all(x) | !any(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
