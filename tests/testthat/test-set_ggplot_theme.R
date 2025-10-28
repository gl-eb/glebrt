# test_that("Check that theme is set successfully", {
#   set_ggplot_theme(theme = "theme_light")
#   withr::defer(ggplot2::set_theme())
#   expect_snapshot(ggplot2::theme_get())
# })
