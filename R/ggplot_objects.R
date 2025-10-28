#' Reset ggplot2 legend and guides to defaults
#'
#' `unset_legend()` takes a ggplot2 object, unsets legend components of its
#' theme, and resets the guides to default values. This can be useful when
#' combining multiple plots into a figure with patchwork, which requires guides
#' and legends to be exactly the same when collecting them across panels.
#'
#' @param gg ggplot2 object as returned by `ggplot()`
#' @export
#' @examples
#' dat <- tibble::tibble(
#'   var = seq(1:10),
#'   val = rev(seq(1:10))
#' )
#' plot <- dat |>
#'   ggplot2::ggplot(ggplot2::aes(x = var, y = val, colour = val)) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_colour_continuous(breaks = dat$val) +
#'   ggplot2::guides(
#'     colour = ggplot2::guide_legend()
#'   ) +
#'   ggplot2::theme(
#'     legend.position = "bottom"
#'   )
#'
#' # unmodified plot
#' plot
#'
#' # plot with legend and guide modifications restored to defaults
#' unset_legend(plot)
unset_legend <- function(gg) {
  if (!ggplot2::is_ggplot(gg)) {
    cli::cli_abort(c(
      "!" = "{.arg gg} must be a ggplot object",
      "x" = "You supplied a {.cls {class(gg)}}"
    ))
  }

  # default theme element and guide values
  theme_defaults <- ggplot2::get_theme()[stringi::stri_detect(
    str = names(ggplot2::get_theme()),
    regex = "legend"
  )]

  gg <- gg + ggplot2::theme(!!!theme_defaults)
  gg@guides <- ggplot2::ggplot()@guides

  return(gg)
}
