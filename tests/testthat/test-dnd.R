skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("dnd helpers: text provider and object env resolve", {
  expect_true(is.numeric(.dnd_string_gtype()))
  expect_true(.dnd_string_gtype() > 0)

  prov <- .dnd_text_provider("hello")
  expect_true(inherits(prov, "RGtkObject") || !is.null(prov))

  .dnd_set_active(list(a = 1L))
  expect_equal(.dnd_resolve_dropdata(), list(a = 1L))
  .dnd_clear_active()

  key <- .dnd_store_object(list(a = 1L))
  expect_match(key, "^gwdnd-")
  expect_equal(.dnd_resolve_dropdata(key), list(a = 1L))
  expect_equal(.dnd_resolve_dropdata("plain text"), "plain text")
  .dnd_clear_key(key)
  expect_equal(.dnd_resolve_dropdata(key), key)
})

test_that("add_drop_source / target / motion attach without warning", {
  w <- gwindow("dnd-api", visible = FALSE)
  src <- glabel("src", container = w)
  tgt <- gedit("", container = w)

  expect_silent(addDropSource(src, handler = function(h, ...) "x"))
  expect_silent(addDropTarget(tgt, handler = function(h, ...) {
    svalue(h$obj) <- as.character(h$dropdata)[1]
  }))
  expect_silent(addDragMotion(tgt, handler = function(h, ...) NULL))

  expect_false(is.null(src$get_attr(".dnd_drag_source")))
  expect_false(is.null(tgt$get_attr(".dnd_drop_target")))
  dispose(w)
})

test_that("object dropdata round-trip via resolve helpers", {
  w <- gwindow("dnd-obj", visible = FALSE)
  src <- glabel("obj", container = w)
  dropped <- NULL
  tgt <- gedit("", container = w)

  addDropSource(src, data.type = "object", handler = function(h, ...) {
    list(name = "foo", n = 42L)
  })
  addDropTarget(tgt, handler = function(h, ...) {
    dropped <<- h$dropdata
  })

  ## Simulate what prepare + drop do without a real pointer drag
  src_ctrl <- src$get_attr(".dnd_drag_source")
  expect_false(is.null(src_ctrl))
  key <- .dnd_store_object(list(name = "foo", n = 42L))
  resolved <- .dnd_resolve_dropdata(key)
  expect_equal(resolved$name, "foo")
  expect_equal(resolved$n, 42L)
  .dnd_clear_key(key)
  dispose(w)
})

test_that("gvarbrowser registers object drop source", {
  assign(".dnd_test_var", 1:3, envir = .GlobalEnv)
  on.exit(rm(list = ".dnd_test_var", envir = .GlobalEnv), add = TRUE)

  w <- gwindow("dnd-vb", visible = FALSE)
  expect_silent(vb <- gvarbrowser(container = w, expand = TRUE))
  expect_false(is.null(vb$get_attr(".dnd_drag_source")))
  try(vb$stop_timer(), silent = TRUE)
  dispose(w)
})
