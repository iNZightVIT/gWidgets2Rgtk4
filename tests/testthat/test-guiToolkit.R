test_that("guiToolkit discovers Rgtk4 backend", {
  skip_if_not_installed("gWidgets2")
  skip_if_not(interactive() || nzchar(Sys.getenv("DISPLAY")),
              "GTK requires a display")

  options(guiToolkit = "Rgtk4")
  tk <- gWidgets2::guiToolkit("Rgtk4")
  expect_s4_class(tk, "guiWidgetsToolkitRgtk4")
  expect_identical(tk@toolkit, "Rgtk4")
})
