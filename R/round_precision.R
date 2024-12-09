#' Rounding of Numbers With Precision
#'
#' `ceiling_precision` rounds up the nearest number of specified decimal places.
#' `floor_precision` rounds down the nearest number of specified decimal places.
#'
#' @param x (numeric) Vector to round
#' @param precision (numeric) Integer indicating the precision to which to round to
#' @examples
#' x <- c(0.001, 0.23, 0.170)
#' ceiling_precision(x, precision = 2)
#' floor_precision(x, precision = 2)

#' @rdname round_precision
#' @export
ceiling_precision <- function(x, precision = 2L) {
  ceiling(x * (10^precision)) / (10^precision)
}

#' @rdname round_precision
#' @export
floor_precision <- function(x, precision = 2L) {
  floor(x * (10^precision)) / (10^precision)
}
