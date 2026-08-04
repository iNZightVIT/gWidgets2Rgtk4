skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("GComponent enabled/visible/tooltip/size/tag/invalid", {
  w <- gwindow("comp", visible = FALSE)
  b <- gbutton("X", container = w)

  enabled(b) <- FALSE
  expect_false(enabled(b))
  enabled(b) <- TRUE
  expect_true(enabled(b))

  visible(b) <- FALSE
  expect_false(visible(b))
  visible(b) <- TRUE

  tooltip(b) <- "tip text"
  expect_true(grepl("tip", tooltip(b)))

  size(b) <- c(120, 40)
  size(b) <- list(width = 100, height = 30)
  size(b) <- c(height = 25)
  size(b) <- 80
  expect_equal(unname(size(b)), c(-1L, -1L))

  tag(b, "k") <- 1
  expect_equal(tag(b, "k"), 1)
  expect_true("k" %in% tag(b))

  b$set_invalid(TRUE, "bad")
  expect_true(b$is_invalid())
  b$set_invalid(FALSE, "")
  expect_false(b$is_invalid())

  b$set_font(list(size = 12))
  expect_equal(font(b)$size, 12)
  expect_true(nzchar(b$.css_class))
  expect_equal(length(b), 1L)
  expect_true(isExtant(b))
  focus(b) <- TRUE
  expect_false(focus(b)) ## get_focus always FALSE in Phase 1
  dispose(w)
})

test_that("handler block/unblock/remove and stubs warn", {
  w <- gwindow("h2", visible = FALSE)
  n <- 0
  b <- gbutton("Y", container = w, handler = function(h, ...) n <<- n + 1)
  b$notify_observers(signal = "clicked")
  expect_equal(n, 1)

  b$block_handlers()
  b$notify_observers(signal = "clicked")
  ## blocked observers should not fire
  expect_equal(n, 1)
  b$unblock_handlers()
  b$notify_observers(signal = "clicked")
  expect_equal(n, 2)

  expect_silent(b$add_drop_source(function(h) "x"))
  expect_silent(b$add_drop_target(function(h) NULL))
  expect_silent(b$add_drag_motion(function(h) NULL))
  expect_warning(b$add_handler_keystroke(function(h) NULL), "not fully implemented")
  expect_silent(b$add_popup_menu(list()))

  b$remove_handlers()
  dispose(w)
})

test_that("ggroup packing helpers, spring, space, spacing", {
  w <- gwindow("grp", visible = FALSE)
  g <- ggroup(horizontal = TRUE, spacing = 4, container = w)
  b1 <- gbutton("a", container = g)
  b2 <- gbutton("b", container = g, expand = TRUE, fill = "both")
  b3 <- gbutton("c", container = g, anchor = c(-1, 1))
  expect_equal(length(g), 3)
  expect_equal(svalue(g), 4)
  svalue(g) <- 8
  expect_equal(svalue(g), 8)
  g$add_spring()
  g$add_space(10)
  g$set_borderwidth(3)
  g$set_size(c(200, 100))
  ## remove child
  g$remove_child(b3)
  expect_equal(length(g), 2)
  expect_true(is(g[1], "GButton"))
  dispose(w)
})

test_that("scrolled ggroup constructs", {
  w <- gwindow("scroll", visible = FALSE)
  g <- ggroup(horizontal = FALSE, use.scrollwindow = TRUE, container = w)
  expect_true(is(g, "GGroup"))
  expect_false(identical(g$widget, g$block))
  dispose(w)
})

test_that("gcombobox editable and items API", {
  w <- gwindow("combo", visible = FALSE)
  cb <- gcombobox(c("a", "b", "c"), selected = 2, container = w)
  expect_equal(svalue(cb), "b")
  svalue(cb) <- "c"
  expect_equal(svalue(cb), "c")
  cb[] <- c("x", "y")
  expect_equal(cb[], c("x", "y"))

  cbe <- gcombobox(c("1", "2"), editable = TRUE, selected = 1, container = w)
  expect_true(nzchar(svalue(cbe)))
  svalue(cbe) <- "custom"
  expect_equal(svalue(cbe), "custom")
  dispose(w)
})

test_that("gwindow title, dispose, status helpers", {
  w <- gwindow("title1", visible = FALSE, width = 300, height = 200)
  expect_equal(svalue(w), "title1")
  svalue(w) <- "title2"
  expect_equal(svalue(w), "title2")
  focus(w) <- TRUE
  w$update_widget()
  w$set_icon("ok")
  w$set_statusbar("hi")
  w$clear_statusbar()
  w$set_infobar("info")
  expect_true(w$is_extant())
  dispose(w)
})

test_that("togglebutton and checkbox items", {
  w <- gwindow("tb", visible = FALSE)
  tb <- gcheckbox("tog", checked = TRUE, use.togglebutton = TRUE, container = w)
  expect_true(svalue(tb))
  svalue(tb) <- FALSE
  expect_false(svalue(tb))
  expect_equal(tb[], "tog")
  tb[] <- "new"
  expect_equal(tb[], "new")
  dispose(w)
})

test_that("icons S3 methods and stockIconFromObject", {
  tk <- guiToolkit("Rgtk4")
  expect_true(is.list(.getStockIcons.guiWidgetsToolkitRgtk4(tk)))
  expect_equal(as.character(.getStockIconByName.guiWidgetsToolkitRgtk4(tk, "gtk-ok")),
               "emblem-ok")
  .addStockIcons.guiWidgetsToolkitRgtk4(tk, "custom", tempfile(fileext = ".gif"))
  expect_equal(.stockIconFromObject.guiWidgetsToolkitRgtk4(tk, 1:3), "gw-numeric")
  expect_equal(.stockIconFromObject.guiWidgetsToolkitRgtk4(tk, "x"), "gw-character")
  expect_equal(.stockIconFromObject.guiWidgetsToolkitRgtk4(tk, factor("a")), "gw-factor")
  expect_equal(.stockIconFromObject.guiWidgetsToolkitRgtk4(tk, data.frame(a = 1)), "gw-dataframe")
})

test_that("set_child_expand_fill_anchor covers fill branches", {
  skip_if_no_display()
  Rgtk4::gtkInit()
  box <- gtkBoxNew(0L, 0L)
  child <- gtkButtonNewWithLabel("z")
  gtkBoxAppend(box, child)
  set_child_expand_fill_anchor(child, expand = TRUE, fill = "x", horizontal = TRUE)
  set_child_expand_fill_anchor(child, expand = TRUE, fill = "y", horizontal = FALSE)
  set_child_expand_fill_anchor(child, expand = TRUE, fill = TRUE, horizontal = TRUE)
  set_child_expand_fill_anchor(child, expand = TRUE, fill = "both",
                               anchor = c(0, 0), horizontal = TRUE, padding = 2L)
  expect_true(TRUE)
})
