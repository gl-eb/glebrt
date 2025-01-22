#' Return a Scale of Greys
#'
#' Returns up to five different shades of grey
#'
#' @param i (numeric) Vector of indices of greys to return
#' @returns A character vector of colour names
#' @export
#' @examples
#' get_greys(1)
#' get_greys(1:3)
#' get_greys(c(1, 3, 5))

get_greys <- function(i = NULL) {
  greys <- c("grey95", "grey75", "grey50", "grey25", "grey5")

  if (is.null(i)) {
    return(greys)
  } else {
    return(greys[i])
  }
}
