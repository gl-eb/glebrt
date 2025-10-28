dat <- tibble::tibble(
  var = seq(1:10),
  val = rev(seq(1:10))
)
plot <- dat |>
  ggplot2::ggplot(ggplot2::aes(x = var, y = val, colour = val)) +
  ggplot2::geom_point() +
  ggplot2::scale_colour_continuous(breaks = dat$val) +
  ggplot2::guides(
    colour = ggplot2::guide_legend()
  ) +
  ggplot2::theme(
    legend.position = "bottom"
  )

test_that("Unsetting legend modification works", {
  vdiffr::expect_doppelganger(
    title = "Plot with custom legend",
    fig = plot
  )
  vdiffr::expect_doppelganger(
    title = "Plot with legend reverted to defaults",
    fig = unset_legend(plot)
  )
})

test_that("Non-ggplot objects are rejected", {
  expect_error(
    unset_legend(ggplot2::ggplot_build(plot)),
    regexp = "`gg` must be a ggplot object"
  )
})
