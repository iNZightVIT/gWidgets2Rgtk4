skip_if_not_installed("gWidgets2")
skip_if_not(interactive() || nzchar(Sys.getenv("DISPLAY")) || nzchar(Sys.getenv("WAYLAND_DISPLAY")),
            "GTK requires a display")

options(guiToolkit = "Rgtk4")

test_that("gstatusbar get/set value", {
  w <- gwindow("sb", visible = FALSE)
  sb <- gstatusbar("hello", container = w)
  expect_true(is(sb, "GStatusBar"))
  expect_equal(svalue(sb), "hello")
  svalue(sb) <- "world"
  expect_equal(svalue(sb), "world")
  dispose(w)
})

test_that("gtoolbar packs actions and respects enabled", {
  w <- gwindow("tb", visible = FALSE)
  hit <- NULL
  act <- gaction("open", icon = "open", handler = function(h, ...) {
    hit <<- "open"
  }, parent = w)
  tb <- gtoolbar(list(act), container = w)
  expect_true(is(tb, "GToolBar"))
  expect_equal(length(svalue(tb)), 1)
  ## proxy button exists and tracks enabled
  btn <- act$proxies[[1]]
  expect_true(inherits(btn, "GtkButton"))
  expect_true(as.logical(gtkWidgetGetSensitive(btn)))
  enabled(act) <- FALSE
  expect_false(as.logical(gtkWidgetGetSensitive(btn)))
  enabled(act) <- TRUE
  act$activate()
  expect_equal(hit, "open")
  dispose(w)
})

test_that("gmenu builds menubar from nested action list", {
  w <- gwindow("mb", visible = FALSE)
  hit <- NULL
  act <- gaction("save", icon = "save", handler = function(h, ...) {
    hit <<- "save"
  }, parent = w)
  mb <- gmenu(list(File = list(act)), container = w)
  expect_true(is(mb, "GMenuBar"))
  expect_true("File" %in% names(svalue(mb)))
  act$activate()
  expect_equal(hit, "save")
  dispose(w)
})

test_that("chrome example path: menu toolbar statusbar share actions", {
  w <- gwindow("chrome", visible = FALSE)
  status <- NULL
  acts <- list(
    open = gaction("open", icon = "open",
                   handler = function(...) status <<- "open", parent = w),
    quit = gaction("quit", icon = "quit",
                   handler = function(...) status <<- "quit", parent = w)
  )
  mb_list <- list(File = list(acts[[1]], gseparator(horizontal = TRUE), acts[[2]]))
  tb_list <- list(acts[[1]], acts[[2]])
  mb <- gmenu(mb_list, container = w)
  tb <- gtoolbar(tb_list, container = w)
  sb <- gstatusbar("ready", container = w)
  g <- gvbox(container = w)
  glabel("content", container = g)
  expect_true(is(mb, "GMenuBar"))
  expect_true(is(tb, "GToolBar"))
  expect_equal(svalue(sb), "ready")
  acts$open$activate()
  svalue(sb) <- status
  expect_equal(svalue(sb), "open")
  dispose(w)
})

test_that("gbutton mirrors gaction proxy", {
  w <- gwindow("btn-act", visible = FALSE)
  hit <- 0
  act <- gaction("Do", tooltip = "tip", handler = function(h, ...) {
    hit <<- hit + 1
  })
  b <- gbutton(action = act, container = w)
  expect_equal(svalue(b), "Do")
  expect_equal(tooltip(b), "tip")
  enabled(act) <- FALSE
  expect_false(enabled(b))
  enabled(act) <- TRUE
  act$activate()
  expect_equal(hit, 1)
  dispose(w)
})

test_that("add_popup_menu accepts menu list without warning", {
  w <- gwindow("pop", visible = FALSE)
  b <- gbutton("x", container = w)
  act <- gaction("one", handler = function(h, ...) NULL)
  expect_silent(b$add_popup_menu(list(act)))
  expect_silent(b$add_3rd_mouse_popup_menu(list(act)))
  dispose(w)
})
