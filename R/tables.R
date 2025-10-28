#' Deselect NA columns
#'
#' Remove NA columns from a data frame while allowing to retain specific columns
#' in the same position
#'
#' @param dat Data frame
#' @param keep Names of columns to keep despite them being entirely `NA`
#' @export
#' @examples
#' dat <- tibble::tibble(
#'   var = letters[1:5],
#'   value1 = 1:5,
#'   value2 = rep(NA, 5)
#' )
deselect_na <- function(dat, keep = NULL) {
  if (!is.data.frame(dat)) {
    cli::cli_abort(c(
      "!" = "{.arg dat} must be a data frame.",
      "x" = "You supplied {.type dat}."
    ))
  }
  if (!is.null(keep) & length(intersect(keep, colnames(dat))) == 0) {
    cli::cli_abort(c(
      "!" = "Can't extract columns that don't exist.",
      "x" = "{.var {keep}} can't be found."
    ))
  }

  dat_columns <- dat |>
    dplyr::select(
      tidyselect::where(~ !all(is.na(.x))),
      tidyselect::all_of(keep)
    )
  dat_order <- dat_columns |>
    dplyr::select(intersect(colnames(dat), colnames(dat_columns)))
  return(dat_order)
}
