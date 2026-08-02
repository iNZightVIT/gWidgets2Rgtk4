skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")
tk <- guiToolkit("Rgtk4")

test_that(".dialog_icon_type maps icons", {
  expect_equal(.dialog_icon_type("info"), 0L)
  expect_equal(.dialog_icon_type("warning"), 1L)
  expect_equal(.dialog_icon_type("question"), 2L)
  expect_equal(.dialog_icon_type("error"), 3L)
  expect_error(.dialog_icon_type("nope"))
})

test_that(".dialog_parent_window resolves parents", {
  expect_null(.dialog_parent_window(NULL))
  w <- gwindow("parent", visible = FALSE)
  expect_true(inherits(.dialog_parent_window(w), "GtkWindow"))
  expect_null(.dialog_parent_window(list()))
  dispose(w)
})

test_that("gmessage constructs and runs with OK response", {
  dlg <- GMessage$new(tk, msg = c("hello", "world"), title = "t", icon = "info")
  expect_true(is(dlg, "GMessage"))
  expect_equal(dlg$get_buttons(), 1L)
  respond_dialog(dlg$widget, -5L)
  expect_null(dlg$run())
})

test_that("gconfirm returns TRUE/FALSE from responses", {
  dlg <- GConfirm$new(tk, msg = "ok?", title = "c", icon = "question")
  expect_true(dlg$ok_response())
  expect_false(dlg$cancel_response())
  respond_dialog(dlg$widget, -5L)
  expect_true(dlg$run())

  dlg2 <- GConfirm$new(tk, msg = "ok?", title = "c", icon = "warning")
  respond_dialog(dlg2$widget, -6L)
  expect_false(dlg2$run())
})

test_that("ginput dialog set_text and responses", {
  dlg <- GInput$new(tk, msg = "name?", title = "i", icon = "info")
  dlg$set_text("abc")
  expect_equal(as.character(dlg$ok_response()), "abc")
  expect_equal(dlg$cancel_response(), character(0))
  respond_dialog(dlg$widget, -5L)
  expect_equal(as.character(dlg$run()), "abc")
})

test_that("gbasicdialog add child and OK via set_visible", {
  handled <- FALSE
  dlg <- gbasicdialog(
    title = "bd",
    handler = function(h, ...) handled <<- TRUE
  )
  expect_true(is(dlg, "GBasicDialog"))
  gbutton("inside", container = dlg)
  respond_dialog(dlg$block, -5L)
  expect_true(dlg$set_visible(TRUE))
  expect_true(handled)
})

test_that("gbasicdialog cancel returns FALSE", {
  dlg <- gbasicdialog(title = "bd2", do.buttons = TRUE)
  respond_dialog(dlg$block, -6L)
  expect_false(dlg$set_visible(TRUE))
})

test_that("galert with GWindow parent uses infobar path", {
  w <- gwindow("alert-parent", visible = FALSE)
  expect_null(galert("ping", parent = w, delay = 0.05))
  dispose(w)
})

test_that("galert without parent opens transient window", {
  expect_null(galert("hi", title = "alert", delay = 0.05))
  Sys.sleep(0.15)
})

test_that("public gmessage/gconfirm/ginput wrappers run", {
  local_mocked_bindings(
    gtkDialogRun = function(dialog) -5L
  )
  expect_null(gmessage("hello", title = "m", icon = "info"))
  expect_true(gconfirm("ok?", title = "c", icon = "question"))
  expect_type(ginput("name?", text = "z", title = "i"), "character")
})

test_that("gconfirm cancel via mocked dialog run", {
  local_mocked_bindings(
    gtkDialogRun = function(dialog) -6L
  )
  expect_false(gconfirm("no?", icon = "warning"))
})
