#' Get Precision of Number
#'
#' Get the numeric precision of numbers (i.e. the number of significant digits)
#'
#' @param x (numeric) Vector to get precision of
#' @export
#' @examples
#' x <- c(0.001, 0.23, 0.170)
#' get_precision(x)

get_precision <- function(x) {
  x |>
    stringr::str_replace_all("\\d+\\.", "") |>
    stringr::str_replace_all("0+$", "") |>
    purrr::map(\(x) stringr::str_split_1(x, "(?<=\\d)(?=\\d)")) |>
    purrr::map(length) |>
    unlist()
}
