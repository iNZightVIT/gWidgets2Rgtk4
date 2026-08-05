skip_if_not_installed("gWidgets2")
skip_if_not(interactive() || nzchar(Sys.getenv("DISPLAY")), "GTK requires a display")

options(guiToolkit = "Rgtk4")

test_that("gframe and gexpandgroup construct", {
  w <- gwindow("c", visible = FALSE)
  fr <- gframe("Frame", container = w)
  expect_true(is(fr, "GFrame"))
  expect_equal(names(fr), "Frame")
  eg <- gexpandgroup("More", container = w)
  expect_true(is(eg, "GExpandGroup"))
  dispose(w)
})

test_that("glayout attaches children", {
  w <- gwindow("lay", visible = FALSE)
  lay <- glayout(container = w)
  lay[1, 1] <- "A"
  lay[1, 2] <- gbutton("B", cont = lay)
  expect_equal(unname(dim(lay)[["nrow"]]), 1)
  dispose(w)
})

test_that("glayout remove_child updates child_positions", {
  w <- gwindow("lay-rm", visible = FALSE)
  lay <- glayout(container = w)
  lay[1, 1] <- gbutton("keep", container = NULL)
  sl <- gslider(from = 0, to = 5, by = 1, value = 2, container = NULL)
  lay[2, 1:2, expand = TRUE] <- sl
  lay[2, 3] <- gbutton("extra", container = NULL)
  expect_equal(length(lay$child_positions), 3L)

  to_remove <- lapply(
    Filter(function(item) any(item$x == 2L), lay$child_positions),
    `[[`, "child"
  )
  expect_equal(length(to_remove), 2L)
  for (child in to_remove)
    lay$remove_child(child)

  expect_equal(length(lay$child_positions), 1L)
  expect_false(any(vapply(lay$child_positions, function(x) any(x$x == 2L), logical(1))))
  dispose(w)
})

test_that("gnotebook and gstackwidget page switching", {
  w <- gwindow("nb", visible = FALSE)
  nb <- gnotebook(container = w)
  add(nb, glabel("p1"), label = "One")
  add(nb, glabel("p2"), label = "Two")
  expect_equal(length(nb), 2)
  svalue(nb) <- 2
  expect_equal(svalue(nb), 2)
  dispose(w)

  w2 <- gwindow("st", visible = FALSE)
  st <- gstackwidget(container = w2)
  add(st, glabel("a"))
  add(st, glabel("b"))
  expect_equal(length(st), 2)
  dispose(w2)
})

test_that("gpanedgroup accepts two children", {
  w <- gwindow("pg", visible = FALSE)
  pg <- gpanedgroup(container = w)
  gbutton("L", container = pg)
  gbutton("R", container = pg)
  expect_equal(length(pg), 2)
  dispose(w)
})

test_that("gpanedgroup maps expand to resize and honors size request", {
  w <- gwindow("pg-size", visible = FALSE, width = 900, height = 400)
  pg <- gpanedgroup(container = w, expand = TRUE)
  left <- ggroup(container = pg, expand = FALSE)
  size(left) <- c(220, -1)
  glabel("L", container = left)
  right <- ggroup(container = pg, expand = TRUE)
  glabel("R", container = right)

  expect_false(as.logical(Rgtk4::gtkPanedGetResizeStartChild(pg$widget)))
  expect_true(as.logical(Rgtk4::gtkPanedGetResizeEndChild(pg$widget)))
  expect_false(as.logical(Rgtk4::gtkPanedGetShrinkStartChild(pg$widget)))
  expect_false(as.logical(Rgtk4::gtkPanedGetShrinkEndChild(pg$widget)))

  visible(w) <- TRUE
  for (i in 1:20)
    Rgtk4::gtkMainIterationDo(FALSE)

  expect_equal(as.integer(unname(size(left)[1])), 220L)
  dispose(w)
})

test_that("gpanedgroup svalue uses proportion or integer pixels", {
  w <- gwindow("pg-svalue", visible = FALSE, width = 800, height = 300)
  pg <- gpanedgroup(container = w, expand = TRUE)
  gbutton("L", container = pg)
  gbutton("R", container = pg)
  visible(w) <- TRUE
  for (i in 1:20)
    Rgtk4::gtkMainIterationDo(FALSE)

  svalue(pg) <- 0.25
  for (i in 1:10)
    Rgtk4::gtkMainIterationDo(FALSE)
  expect_equal(
    as.integer(Rgtk4::gtkPanedGetPosition(pg$widget)),
    as.integer(0.25 * unname(size(pg)[1]))
  )
  expect_equal(unname(svalue(pg)), 0.25, tolerance = 0.02)

  svalue(pg) <- 180L
  for (i in 1:10)
    Rgtk4::gtkMainIterationDo(FALSE)
  expect_equal(as.integer(Rgtk4::gtkPanedGetPosition(pg$widget)), 180L)
  dispose(w)
})
