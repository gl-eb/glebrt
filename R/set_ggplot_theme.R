#' Set a global ggplot2 theme
#'
#' This function sets a global ggplot2 theme using ggplot2::set_theme().
#' In case you want to use non-default wont weights, you need to register them
#' as a font variant first (see `systemfonts::register_variant()`).
#'
#' @param theme (character) The name of a ggplot2 complete theme (default: "theme_light")
#' @param base_size (numeric) Base font size argument (base_size) passed onto ggplot theme
#' @param font_family (character) Font family name
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' set_ggplot_theme()
#' set_ggplot_theme(theme = "theme_light")
#' set_ggplot_theme(
#'   theme = "theme_light",
#'   base_size = 15,
#'   font_family = "Fira Sans",
#' )
#' }

set_ggplot_theme <- function(
  theme = "theme_light",
  base_size = 15,
  font_family = "Fira Sans"
) {
  if (!exists(theme)) {
    cli::cli_abort(
      c(
        "!" = "{.arg theme} must be a complete ggplot2 theme.",
        "x" = "You supplied {.fn {theme}}."
      )
    )
  }
  if (!is.numeric(base_size)) {
    cli::cli_abort(
      c(
        "!" = "{.arg base_size} must be numeric.",
        "x" = "You supplied {.type {base_size}}."
      )
    )
  }
  if (!is.character(font_family)) {
    cli::cli_abort(
      c(
        "!" = "{.arg font_family} must be a string.",
        "x" = "You supplied {.type {font_family}}."
      )
    )
  }
  if (length(font_family) > 1) {
    cli::cli_abort(
      c(
        "!" = "{.arg font_family} must be a character vector of length 1.",
        "x" = "Your vector has length {.val {length(font_family)}}."
      )
    )
  }
  if (stringi::stri_isempty(font_family)) {
    cli::cli_abort(
      c(
        "x" = "{.arg font_family} cannot be an empty string."
      )
    )
  }

  # get font from online repository if not available locally
  systemfonts::require_font(
    family = font_family,
    repositories = c("Font Squirrel", "Font Library", "Google Fonts"),
    verbose = FALSE
  )

  # set theme as requested
  theme_args <- list(
    base_size = base_size,
    base_family = font_family
  )
  do.call(theme, theme_args) |> eval() |> ggplot2::set_theme()

  # make facet strips less noticeable
  ggplot2::update_theme(
    strip.background = ggplot2::element_rect(color = "grey70", fill = "white"),
    strip.text = ggplot2::element_text(color = "black")
  )

  # also set font as default for geom_text and geom_label
  ggplot2::update_geom_defaults(
    geom = "text",
    ggplot2::aes(family = font_family)
  )
  ggplot2::update_geom_defaults(
    geom = "label",
    ggplot2::aes(family = font_family)
  )
}
