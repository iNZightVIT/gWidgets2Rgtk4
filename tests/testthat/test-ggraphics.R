skip_if_not_installed("gWidgets2")
skip_if_not_installed("unigd")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("ggraphics opens a unigd device and records dev.cur", {
  w <- gwindow("gg", visible = FALSE, width = 400, height = 350)
  before <- grDevices::dev.cur()
  gg <- ggraphics(width = 300, height = 250, container = w)
  expect_true(is(gg, "GGraphics"))
  expect_true(gg$device_alive())
  expect_true(gg$device_number > 1)
  expect_equal(as.integer(grDevices::dev.cur()), as.integer(gg$device_number))
  gg$teardown_device()
  expect_false(gg$device_alive())
  dispose(w)
  if (before > 1 && before %in% as.integer(grDevices::dev.list()))
    grDevices::dev.set(before)
})

test_that("ggraphics blits a plot into the picture", {
  w <- gwindow("gg-plot", visible = TRUE, width = 420, height = 360)
  gg <- ggraphics(width = 320, height = 280, container = w)
  plot(1:10, main = "ggraphics spike")
  ## Force sync (poll would also pick this up)
  expect_true(gg$sync_from_device(force = TRUE))
  f <- file.path(gg$render_dir, "current.png")
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 100)
  dispose(w)
})

test_that("visible(gg) <- TRUE reactivates device", {
  w <- gwindow("gg-tabs", visible = FALSE, width = 400, height = 350)
  g1 <- ggraphics(width = 200, height = 200, container = w)
  d1 <- g1$device_number
  g2 <- ggraphics(width = 200, height = 200, container = w)
  d2 <- g2$device_number
  expect_equal(as.integer(grDevices::dev.cur()), as.integer(d2))
  visible(g1) <- TRUE
  expect_equal(as.integer(grDevices::dev.cur()), as.integer(d1))
  dispose(w)
})
