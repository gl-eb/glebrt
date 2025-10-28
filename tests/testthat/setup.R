# writer for vdiffr::expect_doppelganger() using svglite::svglite()
write_svglite <- function(plot, file, title = "") {
  svglite::svglite(file)
  withr::defer(grDevices::dev.off())

  print(plot + ggtitle(title))
}
