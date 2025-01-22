#' Collapse vector with conjunction
#'
#' Paste together vector with final conjunction, "and" for example, as well as an oxford comma
#' @param x Object that both [paste0()] and [length()] can be called on
#' @param conjunction (character) The conjunction to be used behind the oxford comma
#' @returns String
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
