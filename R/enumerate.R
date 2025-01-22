#' Count unique values in column
#'
#' Get the number of unique values from a column of a data.frame
#'
#' @param dat (data.frame)
#' @param column (character) Name of column to count unique values in
#' @returns An integer
#' @export
#' @examples
#' dat <- tibble::tibble(var = c("a", "b", "c", "b"))
#' enumerate(dat, var)
enumerate <- function(dat, column) {
  dat |> dplyr::pull({{ column }}) |> unique() |> length()
}
