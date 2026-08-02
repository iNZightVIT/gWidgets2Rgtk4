test_that("guiWidgetsToolkitRgtk4 is registered", {
  expect_true(isClass("guiWidgetsToolkitRgtk4"))
  tk <- new("guiWidgetsToolkitRgtk4", toolkit = "Rgtk4")
  expect_s4_class(tk, "guiWidgetsToolkitRgtk4")
  expect_identical(tk@toolkit, "Rgtk4")
})
