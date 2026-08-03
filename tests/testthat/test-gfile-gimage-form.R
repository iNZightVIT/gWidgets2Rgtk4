skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("gfilebrowse get/set value", {
  w <- gwindow("fb", visible = FALSE)
  fb <- gfilebrowse(initial.filename = "/tmp/a.txt", container = w)
  expect_true(is(fb, "GFileBrowse"))
  expect_equal(svalue(fb), "/tmp/a.txt")
  svalue(fb) <- "/tmp/b.txt"
  expect_equal(svalue(fb), "/tmp/b.txt")
  dispose(w)
})

test_that("gfile open/save/selectdir via mocked chooser", {
  seen_parent <- NULL
  local_mocked_bindings(
    gtkFileChooserDialogRun = function(parent = NULL, title = "Choose File", action = 0L) {
      seen_parent <<- parent
      list(response = -5L, file = sprintf("/tmp/chosen-%s", action))
    }
  )
  tk <- guiToolkit("Rgtk4")
  expect_equal(.gfile.guiWidgetsToolkitRgtk4(tk, type = "open"), "/tmp/chosen-0")
  expect_true(inherits(seen_parent, "GtkWindow"))
  expect_equal(.gfile.guiWidgetsToolkitRgtk4(tk, type = "save", text = "Save as"),
               "/tmp/chosen-1")
  expect_equal(.gfile.guiWidgetsToolkitRgtk4(tk, type = "selectdir"), "/tmp/chosen-2")
})

test_that("gfile passes explicit parent window", {
  w <- gwindow("file-parent", visible = FALSE)
  seen_parent <- NULL
  local_mocked_bindings(
    gtkFileChooserDialogRun = function(parent = NULL, title = "Choose File", action = 0L) {
      seen_parent <<- parent
      list(response = -6L, file = NULL)
    }
  )
  tk <- guiToolkit("Rgtk4")
  expect_equal(
    .gfile.guiWidgetsToolkitRgtk4(tk, type = "open", parent = w),
    character(0)
  )
  expect_identical(seen_parent, getWidget(w))
  dispose(w)
})

test_that("gfile cancel returns character(0)", {
  local_mocked_bindings(
    gtkFileChooserDialogRun = function(...) list(response = -6L, file = "/tmp/x")
  )
  tk <- guiToolkit("Rgtk4")
  expect_equal(.gfile.guiWidgetsToolkitRgtk4(tk, type = "open"), character(0))
})

test_that("gformlayout adds labeled rows", {
  w <- gwindow("form", visible = FALSE)
  f <- gformlayout(align = "left", container = w)
  expect_true(is(f, "GFormLayout"))
  gedit("x", container = f, label = "Name")
  gcheckbox("y", container = f, label = "Ok")
  expect_equal(length(f), 2L)
  dispose(w)
})

test_that("gimage from stock and file", {
  w <- gwindow("img", visible = FALSE)
  img <- gimage(stock.id = "ok", container = w)
  expect_true(is(img, "GImage"))
  expect_equal(svalue(img), "ok")
  ## non-existent path falls back to icon name mapping
  svalue(img) <- "dialog-information"
  expect_equal(svalue(img), "dialog-information")
  ## real file if available from gWidgets2 images
  gif <- list.files(system.file("images", package = "gWidgets2"),
                    pattern = "\\.gif$", full.names = TRUE)
  if (length(gif)) {
    img2 <- gimage(filename = basename(gif[1]), dirname = dirname(gif[1]), container = w)
    expect_true(file.exists(svalue(img2)))
  }
  dispose(w)
})
