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
  ## size request is readable; allocation may still be unset when invisible
  sz <- unname(size(b))
  expect_true(is.numeric(sz) && length(sz) == 2L)
  expect_true(sz[1] > 0 || sz[1] == -1L)
  expect_true(sz[2] > 0 || sz[2] == -1L)

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
  expect_silent(b$add_handler_keystroke(function(h) NULL))
  expect_silent(b$add_popup_menu(list()))

  b$remove_handlers()
  dispose(w)
})

test_that("gwindow size/position/center helpers", {
  w <- gwindow("geo", visible = FALSE, width = 320, height = 240)
  sz <- size(w)
  expect_equal(unname(sz), c(320L, 240L))
  w$set_position(10L, 20L)
  pos <- w$get_position()
  expect_equal(unname(pos), c(10L, 20L))
  ## Named for iNZight$reload() ipos[["x"]] / ipos[["y"]]
  expect_equal(pos[["x"]], 10L)
  expect_equal(pos[["y"]], 20L)
  expect_identical(names(pos), c("x", "y"))
  ## Default before set_position also named
  w2 <- gwindow("geo2", visible = FALSE)
  pos0 <- w2$get_position()
  expect_identical(names(pos0), c("x", "y"))
  expect_silent(w$center())
  dispose(w)
  dispose(w2)
})

test_that("ggroup remove_child tolerates destroyed GTK widgets", {
  w <- gwindow("rm", visible = FALSE)
  g <- gvbox(container = w)
  b <- gbutton("x", container = g)
  dispose(w)
  ## After window destroy, remove should not error (iNZight destroy handlers)
  expect_silent(g$remove_child(b))
})

test_that("gtext margins and scroll_to", {
  w <- gwindow("txt", visible = FALSE)
  t <- gtext("hello\nworld", container = w)
  t$set_left_margin(0L)
  t$set_right_margin(0L)
  expect_equal(t$get_left_margin(), 0L)
  expect_equal(t$get_right_margin(), 0L)
  expect_silent(t$scroll_to("start"))
  expect_silent(t$scroll_to("end"))
  dispose(w)
})

test_that("gbutton set_icon from file path", {
  w <- gwindow("btn-img", visible = FALSE)
  b <- gbutton("", container = w)
  tmp <- tempfile(fileext = ".png")
  ## minimal 1x1 PNG
  writeBin(
    as.raw(c(
      0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d,
      0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
      0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xde, 0x00, 0x00, 0x00,
      0x0c, 0x49, 0x44, 0x41, 0x54, 0x08, 0xd7, 0x63, 0xf8, 0xcf, 0xc0, 0x00,
      0x00, 0x00, 0x03, 0x00, 0x01, 0x00, 0x05, 0xfe, 0xd4, 0xef, 0x00, 0x00,
      0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82
    )),
    tmp
  )
  expect_silent(b$set_icon(tmp))
  unlink(tmp)
  dispose(w)
})

test_that("gbutton set_icon keeps label and resolves gw-* to gif files", {
  w <- gwindow("btn-stock", visible = FALSE)
  b <- gbutton("Import data", container = w)
  expect_equal(svalue(b), "Import data")
  b$set_icon("gw-file")
  ## Label must survive (GTK4 set_icon_name would wipe it)
  expect_equal(svalue(b), "Import data")
  expect_true(is.list(b$button_icon))
  expect_equal(b$button_icon$kind, "file")
  expect_true(file.exists(b$button_icon$src))
  expect_match(b$button_icon$src, "file\\.gif$")
  ## Child is a box with image + label, not a lone icon
  child <- gtkButtonGetChild(b$widget)
  expect_true(inherits(child, "GtkBox"))
  img <- gtkWidgetGetFirstChild(child)
  expect_false(is.null(gtkImageGetPaintable(img)))
  ## Stock "ok" resolves via gWidgets gif registry (or theme fallback)
  b2 <- gbutton("Save", container = w)
  b2$set_icon("ok")
  expect_equal(svalue(b2), "Save")
  expect_true(b2$button_icon$kind %in% c("file", "theme"))
  expect_true(nzchar(b2$button_icon$src))
  ## Theme-only ids paint via icon-theme lookup (not from_icon_name)
  b3 <- gbutton("", container = w)
  b3$set_icon("new")
  expect_equal(b3$button_icon$kind, "theme")
  expect_false(is.null(gtkImageGetPaintable(gtkButtonGetChild(b3$widget))))
  ## go-back aliases to gWidgets gif
  expect_equal(resolve_icon_spec("go-back")$kind, "file")
  ## Plain label is not treated as a missing theme icon
  b4 <- gbutton("Hello", container = w)
  expect_null(b4$button_icon)
  expect_equal(svalue(b4), "Hello")
  dispose(w)
})

test_that("getStockIconByName prefers registry files over passthrough names", {
  skip_if_no_display()
  expect_match(as.character(getStockIconByName("gw-file")), "file\\.gif$")
  expect_match(as.character(getStockIconByName("file")), "file\\.gif$")
  ## gtk-ok has no gif key; maps to theme name
  expect_equal(as.character(getStockIconByName("gtk-ok")), "emblem-ok")
  expect_equal(as.character(getStockIconByName("Import data")), "")
})

test_that("setPointerCursor applies css", {
  w <- gwindow("cur", visible = FALSE)
  b <- gbutton("x", container = w)
  expect_silent(setPointerCursor(b))
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
  options(gWidgets2Rgtk4.warned_borderwidth = FALSE)
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

test_that("ggroup expand/fill sets axis expand and FILL (not cross-axis expand)", {
  w <- gwindow("ef", visible = FALSE)
  gh <- ggroup(horizontal = TRUE, container = w)
  bx <- gbutton("x", container = gh, expand = TRUE, fill = "x")
  by <- gbutton("y", container = gh, expand = TRUE, fill = "y")
  bb <- gbutton("b", container = gh, expand = TRUE, fill = "both")
  ## Horizontal box: expand → hexpand only; fill axes → FILL align, never vexpand
  expect_true(as.logical(gtkWidgetGetHexpand(getBlock(bx))))
  expect_false(as.logical(gtkWidgetGetVexpand(getBlock(bx))))
  .expect_align(getBlock(bx), halign = 0L) ## FILL

  expect_true(as.logical(gtkWidgetGetHexpand(getBlock(by))))
  expect_false(as.logical(gtkWidgetGetVexpand(getBlock(by))))
  .expect_align(getBlock(by), valign = 0L) ## cross-axis FILL, not vexpand

  expect_true(as.logical(gtkWidgetGetHexpand(getBlock(bb))))
  expect_false(as.logical(gtkWidgetGetVexpand(getBlock(bb))))
  .expect_align(getBlock(bb), halign = 0L, valign = 0L)

  gv <- ggroup(horizontal = FALSE, container = w)
  bv <- gbutton("v", container = gv, expand = TRUE, fill = "both")
  expect_true(as.logical(gtkWidgetGetVexpand(getBlock(bv))))
  expect_false(as.logical(gtkWidgetGetHexpand(getBlock(bv))))
  .expect_align(getBlock(bv), halign = 0L, valign = 0L)

  ## expand=FALSE pins main-axis expand so children cannot poison parent
  b0 <- gbutton("0", container = gv, expand = FALSE)
  expect_false(as.logical(gtkWidgetGetVexpand(getBlock(b0))))
  expect_false(as.logical(gtkWidgetGetHexpand(getBlock(b0))))
  dispose(w)
})

test_that("add_spring expands only along box orientation", {
  w <- gwindow("spring", visible = FALSE)
  gh <- ggroup(horizontal = TRUE, container = w)
  gbutton("a", container = gh)
  gh$add_spring()
  spring_h <- gtkWidgetGetLastChild(gh$widget)
  expect_true(as.logical(gtkWidgetGetHexpand(spring_h)))
  expect_false(as.logical(gtkWidgetGetVexpand(spring_h)))

  gv <- gvbox(container = w)
  gbutton("b", container = gv)
  gv$add_spring()
  spring_v <- gtkWidgetGetLastChild(gv$widget)
  expect_true(as.logical(gtkWidgetGetVexpand(spring_v)))
  expect_false(as.logical(gtkWidgetGetHexpand(spring_v)))
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
  ## fill wins over anchor on filled axes (RGtk2 ignored anchor on non-Misc)
  .expect_align(field, halign = 0L)
  expect_true(as.logical(gtkWidgetGetHexpand(field)))
  expect_true(as.logical(gtkWidgetGetVexpand(field))) ## fill=TRUE → both axes

  lay[2, 1, expand = TRUE, fill = "both"] <- gbutton("fill")
  fill_btn <- getBlock(lay[2, 1])
  expect_true(as.logical(gtkWidgetGetHexpand(fill_btn)))
  expect_true(as.logical(gtkWidgetGetVexpand(fill_btn)))
  .expect_align(fill_btn, halign = 0L, valign = 0L)

  ## fill="x" expands horizontally only (iNZight [<- default via fil→fill)
  lay[3, 1, expand = TRUE, fill = "x"] <- gbutton("fx")
  fx <- getBlock(lay[3, 1])
  expect_true(as.logical(gtkWidgetGetHexpand(fx)))
  expect_false(as.logical(gtkWidgetGetVexpand(fx)))
  dispose(w)
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
  expect_false(as.logical(gtkWidgetGetHexpand(child2)))
  .expect_align(child2, valign = 0L)

  ## Cross-axis fill in horizontal box: FILL align, not vexpand
  child2b <- gtkButtonNewWithLabel("z2b")
  gtkBoxAppend(box, child2b)
  set_child_expand_fill_anchor(child2b, expand = TRUE, fill = "both", horizontal = TRUE)
  expect_true(as.logical(gtkWidgetGetHexpand(child2b)))
  expect_false(as.logical(gtkWidgetGetVexpand(child2b)))
  .expect_align(child2b, halign = 0L, valign = 0L)

  child3 <- gtkButtonNewWithLabel("z3")
  gtkBoxAppend(box, child3)
  set_child_expand_fill_anchor(child3, expand = FALSE, fill = FALSE,
                               anchor = c(-1, 1), horizontal = TRUE, padding = 2L)
  .expect_align(child3, halign = 1L, valign = 1L)
  expect_false(as.logical(gtkWidgetGetHexpand(child3)))
  expect_equal(as.integer(gtkWidgetGetMarginStart(child3))[1], 2L)

  ## Grid mode: fill="both" expands both axes
  child4 <- gtkButtonNewWithLabel("z4")
  gtkBoxAppend(box, child4)
  set_child_expand_fill_anchor(child4, expand = TRUE, fill = "both", horizontal = NA)
  expect_true(as.logical(gtkWidgetGetHexpand(child4)))
  expect_true(as.logical(gtkWidgetGetVexpand(child4)))
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

  options(gWidgets2Rgtk4.warned_borderwidth = FALSE)
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
