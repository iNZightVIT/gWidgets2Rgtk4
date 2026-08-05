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
