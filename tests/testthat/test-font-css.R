skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("font<- on glabel maps to CSS and round-trips get_font", {
  w <- gwindow("font", visible = FALSE)
  l <- glabel("x", container = w)
  font(l) <- list(weight = "bold", size = 12, color = "blue", background = "yellow")
  expect_equal(font(l)$weight, "bold")
  expect_equal(font(l)$size, 12)
  expect_true(nzchar(l$.css_class))
  expect_true(grepl("font-weight:\\s*bold", l$.css_font_decls))
  expect_true(grepl("font-size:\\s*12pt", l$.css_font_decls))
  expect_true(grepl("color:\\s*#0000ff", l$.css_font_decls))
  expect_true(grepl("background-color:\\s*#ffff00", l$.css_font_decls))
  expect_true(as.logical(gtkWidgetHasCssClass(l$widget, l$.css_class)))

  font(l) <- "font-size: 18pt; color: red;"
  expect_equal(font(l)$css, "font-size: 18pt; color: red;")
  expect_true(grepl("18pt", l$.css_font_decls))
  dispose(w)
})

test_that("font family options remap and scale/style keys", {
  old <- getOption("gWidgets2Rgtk4.font.family")
  on.exit(options("gWidgets2Rgtk4.font.family" = old), add = TRUE)
  options("gWidgets2Rgtk4.font.family" = c(sans = "serif"))

  w <- gwindow("font2", visible = FALSE)
  l <- glabel("x", container = w)
  font(l) <- list(family = "sans", style = "italic", scale = "xx-large")
  expect_true(grepl('font-family:\\s*"serif"', l$.css_font_decls))
  expect_true(grepl("font-style:\\s*italic", l$.css_font_decls))
  expect_true(grepl("font-size:", l$.css_font_decls))
  dispose(w)
})

test_that("css<- merges with font<-; loadCss and addCssClass", {
  w <- gwindow("css", visible = FALSE)
  l <- glabel("x", container = w)
  font(l) <- list(weight = "bold")
  css(l) <- "padding: 4px;"
  expect_equal(css(l), "padding: 4px;")
  expect_true(grepl("font-weight", l$.css_font_decls))
  expect_equal(l$.css_extra_decls, "padding: 4px;")

  font(l) <- list(size = 11, css = "letter-spacing: 1px;")
  expect_true(grepl("letter-spacing", l$.css_font_decls))
  expect_equal(css(l), "padding: 4px;") ## css<- layer preserved

  loadCss(".gw-test-danger { color: #b00020; }")
  addCssClass(l, "gw-test-danger")
  expect_true(as.logical(gtkWidgetHasCssClass(l$widget, "gw-test-danger")))
  removeCssClass(l, "gw-test-danger")
  expect_false(as.logical(gtkWidgetHasCssClass(l$widget, "gw-test-danger")))
  dispose(w)
})

test_that("font_spec_to_css helper covers keys and edge cases", {
  f2c <- gWidgets2Rgtk4:::font_spec_to_css
  d <- f2c(list(weight = "bold", family = "monospace", size = 10))
  expect_true(grepl("font-weight", d))
  expect_true(grepl("monospace", d))
  expect_equal(f2c(NULL), "")
  expect_equal(f2c("raw css only"), "")
  expect_equal(f2c(list(css = "padding:1px")), "")
  expect_equal(f2c(c(weight = "bold")), "font-weight: bold;")
  expect_true(grepl("color:\\s*#336699", f2c(list(foreground = "#336699"))))
  expect_true(grepl("color:\\s*rgb", f2c(list(color = "rgb(1,2,3)"))))
  expect_equal(f2c(list(weight = NA)), "")
  expect_true(grepl("font-weight:\\s*900", f2c(list(weight = "900")))) ## unmapped passthrough
  expect_true(grepl("font-style:\\s*oblique", f2c(list(style = "oblique"))))
  expect_true(grepl("Helvetica", f2c(list(family = "helvetica"))))
  expect_true(grepl("CustomFace", f2c(list(family = "CustomFace"))))
  expect_true(grepl("font-size:", f2c(list(scale = "xx-small"))))
  expect_true(grepl("5\\.78|5\\.79|5\\.8", f2c(list(scale = "xx-small")))) ## ~10/1.728
  ## bad scale.base falls back to 10
  old_base <- getOption("gWidgets2Rgtk4.font.scale.base")
  on.exit(options("gWidgets2Rgtk4.font.scale.base" = old_base), add = TRUE)
  options("gWidgets2Rgtk4.font.scale.base" = -1)
  expect_true(grepl("font-size:\\s*10pt", f2c(list(scale = "medium"))))
  ## weight option overlay
  old_w <- getOption("gWidgets2Rgtk4.font.weight")
  on.exit(options("gWidgets2Rgtk4.font.weight" = old_w), add = TRUE)
  options("gWidgets2Rgtk4.font.weight" = c(bold = "600"))
  expect_equal(f2c(list(weight = "bold", nonsense = "x")), "font-weight: 600;")
  expect_equal(gWidgets2Rgtk4:::.combine_css_decls("", "  ", NULL), "")
  ## style_target on bare Gtk widget
  lab <- gtkLabelNew("z")
  expect_identical(gWidgets2Rgtk4:::.style_target(lab), lab)
})

test_that("color_to_css and class rule helpers", {
  c2c <- gWidgets2Rgtk4:::.color_to_css
  expect_equal(c2c("#abc"), "#abc")
  expect_equal(c2c("not-a-color-xyz"), "not-a-color-xyz")
  rule <- gWidgets2Rgtk4:::.font_class_rule("gw-x", "color: red")
  expect_true(grepl("\\.gw-x, \\.gw-x label", rule))
  expect_true(grepl("color: red;", rule))
  expect_equal(gWidgets2Rgtk4:::.font_class_rule("gw-x", ""), "")
  expect_equal(gWidgets2Rgtk4:::.combine_css_decls(NULL, NULL, NULL), "")
  expect_equal(gWidgets2Rgtk4:::.combine_css_decls("a", "b", NULL), "a; b;")
})

test_that("app css option loads once via ensure_app_css", {
  ## Reset package flag so option path runs
  env <- gWidgets2Rgtk4:::.font_env
  env$app_css_loaded <- FALSE
  old <- getOption("gWidgets2Rgtk4.css")
  on.exit({
    options("gWidgets2Rgtk4.css" = old)
    env$app_css_loaded <- TRUE
  }, add = TRUE)
  options("gWidgets2Rgtk4.css" = "label { opacity: 0.99; }")
  w <- gwindow("appcss", visible = FALSE)
  glabel("x", container = w) ## triggers .ensure_app_css in initialize
  expect_true(isTRUE(env$app_css_loaded))
  expect_true(grepl("opacity", env$display_css))
  dispose(w)
})

test_that("css() on non-GComponent returns empty; empty css<- clears extra", {
  expect_equal(css(list()), "")
  w <- gwindow("css2", visible = FALSE)
  l <- glabel("x", container = w)
  css(l) <- "padding: 2px;"
  expect_equal(css(l), "padding: 2px;")
  css(l) <- NULL
  expect_equal(css(l), "")
  dispose(w)
})

test_that("gtext font.attr and insert tags", {
  w <- gwindow("gtext-font", visible = FALSE)
  t <- gtext("hello", font.attr = list(family = "monospace"),
             width = 200, height = 120, container = w)
  expect_silent(insert(t, "world", font.attr = list(weight = "bold", color = "red"),
                       do.newline = FALSE))
  expect_true(grepl("helloworld", gsub("\n", "", svalue(t))))
  expect_silent(font(t) <- list(style = "italic", size = 12))
  expect_equal(font(t)$style, "italic")

  ## More tag keys: scale, background, foreground, size-as-scale-name, unknown
  expect_silent(font(t) <- list(
    scale = "large",
    background = "yellow",
    foreground = "navy",
    size = "x-large"
  ))
  ## raw CSS on text view
  expect_silent(font(t) <- "padding: 2px;")
  ## css= merged onto view
  expect_silent(font(t) <- list(weight = "bold", css = "margin: 1px;"))
  expect_silent(font(t) <- NULL)
  expect_silent(font(t) <- list())

  ## insert positions + empty insert no-op
  expect_silent(insert(t, "begin", where = "beginning", do.newline = FALSE,
                       font.attr = list(style = "oblique")))
  expect_silent(insert(t, "", do.newline = FALSE))
  expect_silent(insert(t, "cursorish", where = "at.cursor", do.newline = TRUE))

  ## tag reuse (second identical attr hits lookup path)
  expect_silent(insert(t, "again", font.attr = list(weight = "bold"), do.newline = FALSE))

  ## selection path: select all then set_font
  start <- gtkTextBufferGetStartIter(t$buffer)
  end <- gtkTextBufferGetEndIter(t$buffer)
  gtkTextBufferSelectRange(t$buffer, start, end)
  expect_silent(font(t) <- list(weight = "heavy"))

  expect_silent(font(t) <- c(style = "italic")) ## named vector → as.list path
  ## connect_to_toolkit_signal short-circuit for changed
  expect_silent(t$connect_to_toolkit_signal("changed", identity))
  expect_identical(t$style_widget(), t$widget)
  dispose(w)
})

test_that("gtext get_tag_name custom family and unknown prop", {
  w <- gwindow("gtext-tag", visible = FALSE)
  t <- gtext("x", container = w)
  nm <- t$get_tag_name("family", "JetBrains Mono")
  expect_true(grepl("family-", nm))
  nm2 <- t$get_tag_name("font", "Sans 12")
  expect_true(grepl("font-", nm2))
  ## unknown weight name → fallback 400
  expect_silent(t$get_tag_name("weight", "superbold"))
  expect_silent(t$get_tag_name("style", "weird"))
  expect_silent(t$get_tag_name("scale", "1.5"))
  dispose(w)
})

test_that("compound widgets accept font<-", {
  w <- gwindow("comp-font", visible = FALSE)
  b <- gbutton("B", container = w)
  expect_silent(font(b) <- list(weight = "bold", color = "blue"))
  expect_true(nzchar(b$.css_class))

  cb <- gcheckbox("C", container = w)
  expect_silent(font(cb) <- list(style = "italic"))

  cbg <- gcheckboxgroup(c("a", "b"), container = w)
  expect_silent(font(cbg) <- list(weight = "bold"))
  expect_identical(cbg$style_widget(), cbg$block)

  eg <- gexpandgroup("E", container = w)
  expect_silent(font(eg) <- list(weight = "bold"))
  expect_identical(eg$style_widget(), eg$block)

  fl <- gformlayout(container = w)
  gedit("x", label = "Name", container = fl)
  expect_equal(names(fl$get_labels()), "Name")
  expect_silent(fl$set_font("Name", list(weight = "bold", color = "purple")))
  expect_silent(fl$set_font("Name", "font-size: 11pt;"))
  expect_silent(fl$set_font("Missing", list(weight = "bold"))) ## no-op
  expect_silent(fl$set_label_tooltip("Name", "tip"))
  expect_silent(fl$set_label_tooltip("Missing", "x"))
  dispose(w)
})
