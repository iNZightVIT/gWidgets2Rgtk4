skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("glabel markup, editable toggle, angle", {
  w <- gwindow("lab", visible = FALSE)
  lab <- glabel("<b>hi</b>", markup = TRUE, container = w)
  expect_equal(svalue(lab), "hi")
  ed <- glabel("edit-me", editable = TRUE, container = w)
  expect_equal(ed$state, "label")
  ed$show_edit_widget()
  expect_equal(ed$state, "edit")
  ed$show_label_widget()
  expect_equal(ed$state, "label")
  ed$set_angle(90)
  dispose(w)
})

test_that("gnotebook names, insert, dispose_child", {
  w <- gwindow("nb2", visible = FALSE)
  nb <- gnotebook(container = w, tab.pos = 1)
  p1 <- glabel("one")
  p2 <- glabel("two")
  p3 <- glabel("three")
  add(nb, p1, label = "One")
  add(nb, p2, label = "Two")
  add(nb, p3, label = "Zero", index = 0) ## prepend
  expect_equal(length(nb), 3)
  expect_equal(names(nb)[1], "Zero")
  names(nb) <- c("A", "B", "C")
  expect_equal(names(nb), c("A", "B", "C"))
  expect_true(is(nb[1], "GLabel"))
  nb$dispose_child(p2)
  expect_equal(length(nb), 2)
  dispose(w)
})

test_that("gstackwidget page selection and names", {
  w <- gwindow("st2", visible = FALSE)
  st <- gstackwidget(container = w)
  add(st, glabel("a"))
  add(st, glabel("b"))
  add(st, glabel("c"))
  expect_equal(length(st), 3)
  svalue(st) <- 2
  expect_equal(as.integer(svalue(st)), 2L)
  expect_equal(svalue(st, index = TRUE), 2L)
  expect_true(length(names(st)) == 3)
  expect_true(is(st[1], "GLabel"))
  names(st) <- c("x", "y", "z") ## no-op setter
  dispose(w)
})

test_that("GWidgetWithItems enabled and getWidget", {
  w <- gwindow("wi", visible = FALSE)
  r <- gradio(c("a", "b"), container = w)
  enabled(r) <- FALSE
  expect_false(enabled(r))
  enabled(r) <- TRUE
  expect_true(enabled(r))
  expect_true(inherits(getWidget(r), "RGtkObject"))
  dispose(w)
})

test_that("gedit coerce.with and gtext insert", {
  w <- gwindow("ed2", visible = FALSE)
  e <- gedit("12", coerce.with = as.integer, container = w)
  expect_equal(svalue(e), 12L)
  t <- gtext(container = w, height = 80)
  t$insert_text("line", do.newline = TRUE)
  expect_true(grepl("line", svalue(t)))
  dispose(w)
})

test_that("gprogressbar pulse and gbutton stock icon / remove_border", {
  w <- gwindow("misc2", visible = FALSE)
  pb <- gprogressbar(container = w)
  svalue(pb) <- NULL ## pulse
  expect_true(is(pb, "GProgressBar"))
  b <- gbutton("ok", container = w) ## stock id path
  expect_true(is(b, "GButton"))
  expect_null(b$remove_border())
  dispose(w)
})

test_that("gwindow close-request handler path", {
  w <- gwindow("cr", visible = FALSE)
  w$add_handler_unrealize(function(h, ...) TRUE)
  expect_true(is(w, "GWindow"))
  dispose(w)
})

test_that("expandgroup visibility and names", {
  w <- gwindow("eg2", visible = FALSE)
  eg <- gexpandgroup("Title", container = w)
  names(eg) <- "New"
  expect_equal(names(eg), "New")
  visible(eg) <- TRUE
  expect_true(visible(eg))
  visible(eg) <- FALSE
  expect_false(visible(eg))
  dispose(w)
})
