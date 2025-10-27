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
  x[length(x)] <- paste0(conjunction, " ", x[length(x)])
  c <- paste(x, collapse = ", ")
  if (length(x) < 3) {
    c <- stringr::str_replace_all(
      c,
      paste0(", ", conjunction),
      paste0(" ", conjunction)
    )
  }
  return(c)
}
