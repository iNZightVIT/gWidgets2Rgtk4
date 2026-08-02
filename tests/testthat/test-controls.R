skip_if_not_installed("gWidgets2")
skip_if_not(interactive() || nzchar(Sys.getenv("DISPLAY")), "GTK requires a display")

options(guiToolkit = "Rgtk4")

test_that("selection and numeric controls", {
  w <- gwindow("ctrls", visible = FALSE)
  g <- gvbox(container = w)
  r <- gradio(c("a", "b", "c"), selected = 2, container = g)
  expect_equal(svalue(r), "b")
  cg <- gcheckboxgroup(c("x", "y"), checked = c(TRUE, FALSE), container = g)
  expect_equal(svalue(cg), "x")
  cb <- gcombobox(c("one", "two"), selected = 1, container = g)
  expect_equal(svalue(cb), "one")
  sl <- gslider(from = 0, to = 10, by = 1, value = 5, container = g)
  expect_equal(svalue(sl), 5)
  sp <- gspinbutton(from = 0, to = 10, by = 1, value = 3, container = g)
  expect_equal(svalue(sp), 3)
  pb <- gprogressbar(50, container = g)
  expect_equal(svalue(pb), 50)
  gseparator(container = g)
  dispose(w)
})

test_that("gtext get/set value", {
  w <- gwindow("txt", visible = FALSE)
  t <- gtext("hello", container = w, height = 100)
  expect_true(grepl("hello", svalue(t)))
  svalue(t) <- "world"
  expect_true(grepl("world", svalue(t)))
  dispose(w)
})

test_that("gtimer starts and stops", {
  n <- 0
  t <- gtimer(50, function(data) { n <<- n + 1 }, one.shot = TRUE, start = TRUE)
  Sys.sleep(0.2)
  expect_true(n >= 1)
  t$stop_timer()
})

test_that("gaction holds label", {
  a <- gaction("Act", tooltip = "tip", handler = function(h, ...) NULL)
  expect_equal(svalue(a), "Act")
  svalue(a) <- "Other"
  expect_equal(svalue(a), "Other")
})
