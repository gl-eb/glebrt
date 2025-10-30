dat <- tibble::tibble(
  var = seq(1:10),
  val = rev(seq(1:10))
)
plot_label <- dat |>
  ggplot2::ggplot(ggplot2::aes(x = var, y = val, label = val, colour = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_label()

test_that("Check that theme is set successfully", {
  set_ggplot_theme()
  withr::defer(ggplot2::theme_set(ggplot2::theme_gray()))
  expect_snapshot(ggplot2::theme_get())
})

test_that("All text is set in new font", {
  set_ggplot_theme()
  withr::defer(ggplot2::theme_set(ggplot2::theme_gray()))
  vdiffr::expect_doppelganger(
    title = "Plot in Fira Sans",
    fig = plot_label,
    writer = write_svglite
  )
})

test_that("Non-ggplot objects are rejected", {
  expect_error(
    set_ggplot_theme(theme = "light"),
    regexp = "`theme` must be a complete ggplot2 theme."
  )
  expect_error(
    set_ggplot_theme(base_size = "15"),
    regexp = "`base_size` must be numeric."
  )
  expect_error(
    set_ggplot_theme(font_family = ""),
    regexp = "`font_family` cannot be an empty string."
  )
  expect_error(
    set_ggplot_theme(font_family = c("sans", "serif")),
    regexp = "`font_family` must be a character vector of length 1."
  )
})
