#' Breaks Including Limits
#'
#' Return a vector of breaks for a numeric axis.
#' Based on `scales::breaks_extended()`, but will include the (approximate) axis limits as breaks
#'
#' @param x (numeric) Data mapped onto continuous axis
#' @param n (numeric) Desired number of breaks.
#'   Passed onto `labeling::extended()`
#' @param r (numeric) (0, 1] Factor determining how close axis limits are kept
#'   as discrete breaks before replacing the closest break. A value of 1
#'   results in the most aggressive replacement of precomputed breaks.
#'
#'   Specifically, the interval between breaks as returned by
#'  `labeling::extended()` is multiplied by r to get the minor interval.
#'   Then the limits of x are rounded to the nearest multiple of the minor
#'   interval, the minor break. If not already a break, the minor break is
#'   added to the vector of breaks. In a final step the minor break is replaced
#'   with the actual limit
#' @param ... Other arguments passed on to `labeling::extended()`
#' @export
#' @examples
#' x <- seq(1, 21)
#' breaks_limits(x)

breaks_limits <- function(x, n = 5, r = 1, ...) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "!" = "{.arg x} must be numeric.",
        "x" = "You supplied {.type {x}}"
      )
    )
  }
  if (!is.numeric(n)) {
    cli::cli_abort(
      c(
        "!" = "{.arg n} must be numeric.",
        "x" = "You supplied {.type {n}}"
      )
    )
  }
  if (!is.numeric(r)) {
    cli::cli_abort(
      c(
        "!" = "{.arg r} must be numeric.",
        "x" = "You supplied {.type {r}}"
      )
    )
  } else if (!dplyr::between(r, 0, 1)) {
    cli::cli_abort(
      c(
        "!" = "{.arg r} must be between {.val {0}} and {.val {1}}",
        "x" = "You supplied {.val {r}}"
      )
    )
  }

  # get breaks as computed by labeling::extended()
  breaks <- scales::breaks_extended(n = n, ...)(x)
  n <- length(breaks)
  interval <- breaks[2] - breaks[1]

  # determine precision of breaks
  precision <- get_precision(interval)

  # get upper and lower limit of data
  min <- min(x, na.rm = TRUE) |> floor_precision(precision = precision)
  max <- max(x, na.rm = TRUE) |> ceiling_precision(precision = precision)

  # round min and max to the closest (r * interval)
  interval_minor <- interval * r
  min_round <- round(min / interval_minor) * interval_minor
  max_round <- round(max / interval_minor) * interval_minor

  # add rounded limits to breaks, then clean breaks up
  breaks <- c(min_round, breaks, max_round) |> unique() |> sort()

  # replace rounded limits with actual limits
  breaks[breaks == min_round] <- min
  breaks[breaks == max_round] <- max

  return(breaks)
}
