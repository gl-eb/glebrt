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
  if (!is.data.frame(dat)) {
    cli::cli_abort(c(
      "!" = "{.arg dat} must be a data frame.",
      "x" = "You supplied {.type dat}"
    ))
  }

  column <- rlang::as_name(enquo(column))
  if (!(column %in% colnames(dat))) {
    cli::cli_abort(c(
      "!" = "Can't extract columns that don't exist.",
      "x" = "Column {.var {column}} doesn't exist."
    ))
  }

  dat |> dplyr::pull({{ column }}) |> dplyr::n_distinct()
}
