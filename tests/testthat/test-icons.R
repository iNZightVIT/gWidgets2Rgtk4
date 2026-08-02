test_that("getStockIconByName resolves stock names", {
  tk <- new("guiWidgetsToolkitRgtk4", toolkit = "Rgtk4")
  expect_equal(as.character(.getStockIconByName.guiWidgetsToolkitRgtk4(tk, "ok")),
               "emblem-ok")
  expect_equal(as.character(.getStockIconByName.guiWidgetsToolkitRgtk4(tk, "gtk-save")),
               "document-save")
})

test_that("getStockIcons returns a named list", {
  tk <- new("guiWidgetsToolkitRgtk4", toolkit = "Rgtk4")
  icons <- .getStockIcons.guiWidgetsToolkitRgtk4(tk)
  expect_true(is.list(icons))
  expect_true("ok" %in% names(icons) || "gtk-ok" %in% names(icons))
})
