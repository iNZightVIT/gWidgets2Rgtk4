skip_if_not_installed("gWidgets2")
skip_if_not(interactive() || nzchar(Sys.getenv("DISPLAY")), "GTK requires a display")

options(guiToolkit = "Rgtk4")

test_that("gwindow + ggroup + basic controls construct", {
  w <- gwindow("test", visible = FALSE, width = 200, height = 150)
  expect_true(is(w, "GWindow"))
  g <- ggroup(horizontal = FALSE, container = w)
  expect_true(is(g, "GGroup"))
  b <- gbutton("OK", container = g)
  expect_equal(svalue(b), "OK")
  svalue(b) <- "Done"
  expect_equal(svalue(b), "Done")
  lab <- glabel("hi", container = g)
  expect_equal(svalue(lab), "hi")
  ed <- gedit("x", container = g)
  expect_equal(svalue(ed), "x")
  svalue(ed) <- "y"
  expect_equal(svalue(ed), "y")
  cb <- gcheckbox("c", checked = FALSE, container = g)
  expect_false(svalue(cb))
  svalue(cb) <- TRUE
  expect_true(svalue(cb))
  dispose(w)
})

test_that("gvbox is a vertical ggroup", {
  w <- gwindow("vbox", visible = FALSE)
  g <- gvbox(container = w)
  expect_true(is(g, "GGroup"))
  expect_false(g$horizontal)
  dispose(w)
})

test_that("gbutton accepts a constructor handler", {
  w <- gwindow("h", visible = FALSE)
  clicked <- FALSE
  b <- gbutton("Go", container = w, handler = function(h, ...) {
    clicked <<- TRUE
  })
  expect_true(is(b, "GButton"))
  ## toolkit signal → observers
  b$notify_observers(signal = "clicked")
  expect_true(clicked)
  dispose(w)
})
