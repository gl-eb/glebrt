#' Deframe and set class
#'
#' Deframe tibble into named vector and set class of each element
#'
#' @param dat (data.frame) Table with one or two columns
#' @param class (character) Class to which to set to elements of resulting vector
#' @returns A vector
#' @seealso [glebrt::set_class()] [tibble::deframe()]
#' @export
#' @examples
#' dat <- tibble::tibble(var = c("a", "b", "c"), value = c(1, 2, 3))
#' deframe_with_class(dat, class = "integer")
deframe_with_class <- function(dat, class) {
  vec <- dat |>
    tibble::deframe() |>
    purrr::map(\(x) set_class(x, class))
}

#' Set class
#'
#' A function for setting classes inside of pipes
#'
#' @param x Object to which to apply the class to
#' @param class (character) Class to which to set to elements of resulting vector
#' @export
#' @examples
#' set_class(1, class = "integer")
set_class <- function(x, class) {
  class(x) <- class
  return(x)
}
