#' Set a global ggplot2 theme
#'
#' This function sets a global ggplot2 theme using ggplot2::theme_set()
#'
#' @param theme (character) The name of a ggplot complete theme (default: "theme_light")
#' @param base_size (numeric) Base font size argument (base_size) passed onto ggplot theme
#' @param font_family (character or list) Font family name or font family list. See examples
#' @importFrom ggplot2 theme_light
#' @export
#' @examples
#' set_ggplot_theme()
#' set_ggplot_theme(theme = "theme_light")
#' \dontrun{
#' set_ggplot_theme(
#'   theme = "theme_light",
#'   base_size = 15,
#'   font_family = list(
#'     font_name = "Fira Sans",
#'     font_filename = "FiraSans",
#'     font_format = "otf",
#'     font_cut_roman = "Regular",
#'     font_cut_italic = "Italic",
#'     font_weight_bold = "Medium"
#'   )
#' )
#' }

set_ggplot_theme <- function(
  theme = "theme_light",
  base_size = 15,
  font_family = list(
    font_name = "Fira Sans",
    font_filename = "FiraSans",
    font_format = "otf",
    font_cut_roman = "Regular",
    font_cut_italic = "Italic",
    font_weight_bold = "Medium"
  )
) {
  if (rlang::is_list(font_family)) {
    # ensure list has necessary elements
    font_family_elements <- c(
      "font_name", "font_filename", "font_format",
      "font_cut_roman", "font_cut_italic", "font_weight_bold"
    )
    if (!all(names(font_family) %in% font_family_elements)) {
      cli::cli_abort(
        message = c(
          "x" = "The list {.var font_family} does not contain all of the necessary elements: {.val {font_family_elements}}"
        )
      )
    }

    font_name <- font_family$font_name
    font_filename <- font_family$font_filename
    font_format <- font_family$font_format
    font_cut_roman <- font_family$font_cut_roman
    font_cut_italic <- font_family$font_cut_italic
    font_weight_bold <- font_family$font_weight_bold

    t <- try(
      sysfonts::font_add(
        family = font_name,
        regular = stringr::str_glue(
          "{font_filename}-{font_cut_roman}.{font_format}"
        ),
        bold = stringr::str_glue(
          "{font_filename}-{font_weight_bold}.{font_format}"
        ),
        italic = stringr::str_glue(
          "{font_filename}-{font_cut_italic}.{font_format}"
        ),
        bolditalic = stringr::str_glue(
          "{font_filename}-{font_weight_bold}{font_cut_italic}.{font_format}"
        )
      )
    )

    # fall back to google fonts if font not found locally
    if(inherits(t, "try-error") & curl::has_internet()) {
      sysfonts::font_add_google(font_name, font_filename)
    }

    # use light theme with custom font
    theme_args <- list(
      base_size = base_size,
      base_family = font_name
    )
  } else {
    if (is.null(font_family) | !is.character(font_family)) {
      font_family <- ""
    }

    theme_args <- list(
      base_size = base_size,
      base_family = font_family
    )
  }

  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto()

  do.call(theme, theme_args) |> eval() |> ggplot2::theme_set()

  # make facet strips less noticeable
  ggplot2::theme_update(
    strip.background = ggplot2::element_rect(color = "grey70", fill = "white"),
    strip.text = ggplot2::element_text(color = "black")
  )
}
