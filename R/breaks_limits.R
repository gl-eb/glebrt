#' Breaks Including Limits
#'
#' Return a vector of breaks for a numeric axis.
#' Based on `scales::breaks_extended()`, but will include the (approximate) axis limits as breaks
#'
#' @param x (numeric) Data mapped onto numeric axis
#' @param n (numeric) Desired number of breaks. Passed onto
#' @param ... Other arguments passed on to `labeling::extended()`
#' @export
#' @examples
#' x <- c(0.001, 0.23, 0.170)
#' get_precision(x)

breaks_limits <- function(x, n = 5, ...) {
  # get breaks as computed by labeling::extended()
  breaks <- scales::breaks_extended(n = n, ...)(x)
  n <- length(breaks)
  interval <- breaks[2] - breaks[1]

  # determine precision of breaks
  precision <- get_precision(interval)

  # get upper and lower limit of data
  min <- min(x, na.rm = TRUE) |> floor_precision(precision = precision)
  max <- max(x, na.rm = TRUE) |> ceiling_precision(precision = precision)

  # determine whether min replaces highest breaks or is added
  if (breaks[1] < min) {
    if (min - breaks[1] >= 0.5 * interval) {
      breaks <- c(min, breaks)
    } else {
      breaks[1] <- min
    }
  } else if (breaks[1] > min) {
    if (breaks[1] - min <= 0.5 * interval) {
      breaks[1] <- min
    } else {
      breaks <- c(breaks[1], min, breaks[3:n])
    }
  }

  # determine whether min replaces highest breaks or is added
  if (breaks[n] > max) {
    if (breaks[n] - max >= 0.5 * interval) {
      breaks <- c(breaks, max)
    } else {
      breaks[n] <- max
    }
  } else if (breaks[n] < max) {
    if (max - breaks[n] <= 0.5 * interval) {
      breaks[n] <- max
    } else {
      breaks <- c(breaks, max)
    }
  }

  return(breaks)
}
