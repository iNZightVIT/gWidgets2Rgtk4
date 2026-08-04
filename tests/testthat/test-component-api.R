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
  expect_warning(g$set_borderwidth(3), "deprecated")
  expect_equal(g$get_padding(), 3)
  g$set_size(c(200, 100))
  ## remove child
  g$remove_child(b3)
  expect_equal(length(g), 2)
  expect_true(is(g[1], "GButton"))
  dispose(w)
})

test_that("ggroup nine-spot anchors map to GTK START/CENTER/END", {
  ## gWidgets anchor in [-1,1]^2: x -1 left +1 right; y +1 top -1 bottom
  ## GTK Align: FILL=0 START=1 END=2 CENTER=3
  cases <- list(
    list(a = c(-1, 1), h = 1L, v = 1L),   # NW
    list(a = c(0, 1), h = 3L, v = 1L),    # N
    list(a = c(1, 1), h = 2L, v = 1L),    # NE
    list(a = c(-1, 0), h = 1L, v = 3L),   # W
    list(a = c(0, 0), h = 3L, v = 3L),    # center
    list(a = c(1, 0), h = 2L, v = 3L),    # E
    list(a = c(-1, -1), h = 1L, v = 2L),  # SW
    list(a = c(0, -1), h = 3L, v = 2L),   # S
    list(a = c(1, -1), h = 2L, v = 2L)    # SE
  )
  w <- gwindow("anchors", visible = FALSE)
  g <- ggroup(horizontal = TRUE, container = w)
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    btn <- gbutton(paste0("a", i), container = g, anchor = cs$a)
    .expect_align(getBlock(btn), halign = cs$h, valign = cs$v)
  }
  ## align= alias
  balias <- gbutton("alias", container = g, align = c(1, -1))
  .expect_align(getBlock(balias), halign = 2L, valign = 2L)
  dispose(w)
})

test_that("ggroup expand/fill sets hexpand/vexpand and FILL", {
  w <- gwindow("ef", visible = FALSE)
  gh <- ggroup(horizontal = TRUE, container = w)
  bx <- gbutton("x", container = gh, expand = TRUE, fill = "x")
  by <- gbutton("y", container = gh, expand = TRUE, fill = "y")
  bb <- gbutton("b", container = gh, expand = TRUE, fill = "both")
  expect_true(as.logical(gtkWidgetGetHexpand(getBlock(bx))))
  expect_false(as.logical(gtkWidgetGetVexpand(getBlock(bx))))
  .expect_align(getBlock(bx), halign = 0L) ## FILL

  expect_true(as.logical(gtkWidgetGetHexpand(getBlock(by))))
  expect_true(as.logical(gtkWidgetGetVexpand(getBlock(by))))

  expect_true(as.logical(gtkWidgetGetHexpand(getBlock(bb))))
  expect_true(as.logical(gtkWidgetGetVexpand(getBlock(bb))))
  .expect_align(getBlock(bb), halign = 0L)

  gv <- ggroup(horizontal = FALSE, container = w)
  bv <- gbutton("v", container = gv, expand = TRUE, fill = "both")
  expect_true(as.logical(gtkWidgetGetVexpand(getBlock(bv))))
  expect_true(as.logical(gtkWidgetGetHexpand(getBlock(bv))))
  .expect_align(getBlock(bv), valign = 0L)
  dispose(w)
})

test_that("glayout anchors and expand/fill map to GTK properties", {
  w <- gwindow("lay-pack", visible = FALSE)
  lay <- glayout(container = w)
  ## iNZight-shaped: right-aligned label, left-aligned field
  lay[1, 1, expand = FALSE, fill = FALSE, anchor = c(1, 0)] <- glabel("Right")
  lay[1, 2, expand = TRUE, fill = TRUE, anchor = c(-1, 0)] <- gedit("field")
  .expect_align(getBlock(lay[1, 1]), halign = 2L, valign = 3L)
  field <- getBlock(lay[1, 2])
  .expect_align(field, halign = 1L, valign = 3L)
  expect_true(as.logical(gtkWidgetGetHexpand(field)))

  lay[2, 1, expand = TRUE, fill = "both"] <- gbutton("fill")
  fill_btn <- getBlock(lay[2, 1])
  expect_true(as.logical(gtkWidgetGetHexpand(fill_btn)))
  expect_true(as.logical(gtkWidgetGetVexpand(fill_btn)))
  .expect_align(fill_btn, halign = 0L)
  dispose(w)
})

test_that("scrolled ggroup constructs", {
  w <- gwindow("scroll", visible = FALSE)
  g <- ggroup(horizontal = FALSE, use.scrollwindow = TRUE, container = w)
  expect_true(is(g, "GGroup"))
  expect_false(identical(g$widget, g$block))
  dispose(w)
})

test_that("ggroup padding/margin/border box model", {
  w <- gwindow("box", visible = FALSE)
  g <- gvbox(padding = 5, margin = 3, border = 1, container = w)
  expect_equal(g$get_padding(), 5)
  expect_equal(g$get_margin(), 3)
  expect_equal(g$get_border(), 1L)
  expect_true(nzchar(g$.box_css_class))
  expect_true(as.logical(gtkWidgetHasCssClass(g$widget, g$.box_css_class)))
  ## margin on outer block (same as widget when not scrolled)
  expect_equal(as.integer(gtkWidgetGetMarginTop(g$block))[1], 3L)
  expect_equal(as.integer(gtkWidgetGetMarginStart(g$block))[1], 3L)

  g$set_padding(c(1L, 2L, 3L, 4L))
  expect_equal(g$get_padding(), c(1L, 2L, 3L, 4L))
  g$set_margin(c(10L, 20L, 30L, 40L))
  expect_equal(as.integer(gtkWidgetGetMarginTop(g$block))[1], 10L)
  expect_equal(as.integer(gtkWidgetGetMarginEnd(g$block))[1], 20L)
  expect_equal(as.integer(gtkWidgetGetMarginBottom(g$block))[1], 30L)
  expect_equal(as.integer(gtkWidgetGetMarginStart(g$block))[1], 40L)

  expect_warning(g$set_borderwidth(8), "deprecated")
  expect_equal(g$get_padding(), 8)

  ## scrolled: margin on scrolled window, padding CSS on inner box
  gs <- ggroup(use.scrollwindow = TRUE, padding = 4, margin = 6, container = w)
  expect_false(identical(gs$widget, gs$block))
  expect_equal(as.integer(gtkWidgetGetMarginTop(gs$block))[1], 6L)
  expect_true(as.logical(gtkWidgetHasCssClass(gs$widget, gs$.box_css_class)))
  expect_equal(gs$get_padding(), 4)
  dispose(w)
})

test_that("normalize_css_sides and box CSS decls helpers", {
  expect_equal(normalize_css_sides(5L), c(5L, 5L, 5L, 5L))
  expect_equal(normalize_css_sides(c(1L, 2L)), c(1L, 2L, 1L, 2L))
  expect_equal(normalize_css_sides(c(1L, 2L, 3L, 4L)), c(1L, 2L, 3L, 4L))
  expect_match(.box_model_css_decls(5L, 0L), "padding: 5px 5px 5px 5px")
  expect_match(.box_model_css_decls(0L, 2L), "border: 2px solid")
  expect_equal(.box_model_css_decls(0L, 0L), "")
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

test_that("set_child_expand_fill_anchor covers fill branches and raw anchors", {
  skip_if_no_display()
  Rgtk4::gtkInit()
  box <- gtkBoxNew(0L, 0L)
  child <- gtkButtonNewWithLabel("z")
  gtkBoxAppend(box, child)

  set_child_expand_fill_anchor(child, expand = TRUE, fill = "x", horizontal = TRUE)
  expect_true(as.logical(gtkWidgetGetHexpand(child)))
  expect_false(as.logical(gtkWidgetGetVexpand(child)))
  .expect_align(child, halign = 0L)

  child2 <- gtkButtonNewWithLabel("z2")
  gtkBoxAppend(box, child2)
  set_child_expand_fill_anchor(child2, expand = TRUE, fill = "y", horizontal = FALSE)
  expect_true(as.logical(gtkWidgetGetVexpand(child2)))
  expect_false(as.logical(gtkWidgetGetHexpand(child2))) ## fill y only, not x
  .expect_align(child2, valign = 0L)

  child3 <- gtkButtonNewWithLabel("z3")
  gtkBoxAppend(box, child3)
  set_child_expand_fill_anchor(child3, expand = FALSE, fill = FALSE,
                               anchor = c(-1, 1), horizontal = TRUE, padding = 2L)
  .expect_align(child3, halign = 1L, valign = 1L)
  expect_equal(as.integer(gtkWidgetGetMarginStart(child3))[1], 2L)
})
