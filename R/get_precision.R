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
      c("x" = "{.arg x} must be numeric.")
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
