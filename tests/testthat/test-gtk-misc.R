test_that("stock_to_icon_name maps common stock ids", {
  expect_equal(stock_to_icon_name("ok"), "emblem-ok")
  expect_equal(stock_to_icon_name("gtk-ok"), "emblem-ok")
  expect_equal(stock_to_icon_name("gtk-open"), "document-open")
  expect_equal(stock_to_icon_name("dialog-error"), "dialog-error")
})

test_that("stock_to_icon_name handles empty", {
  expect_null(stock_to_icon_name(NULL))
  expect_null(stock_to_icon_name(""))
})

test_that("fill_to_logical maps fill strings", {
  expect_true(fill_to_logical("both", TRUE))
  expect_true(fill_to_logical("x", TRUE))
  expect_false(fill_to_logical("x", FALSE))
  expect_true(fill_to_logical("y", FALSE))
  expect_false(fill_to_logical(NULL))
  expect_true(fill_to_logical(TRUE))
})

test_that("RtoGObjectConversion maps types", {
  expect_equal(RtoGObjectConversion(1L), "gint")
  expect_equal(RtoGObjectConversion(1.5), "gdouble")
  expect_equal(RtoGObjectConversion(TRUE), "gboolean")
  expect_equal(RtoGObjectConversion("a"), "gchararray")
  expect_equal(RtoGObjectConversion(factor("a")), "gchararray")
})

test_that("getWidget/getBlock stop on RGtkObject", {
  skip_if_not_installed("Rgtk4")
  skip_if_not(interactive() || nzchar(Sys.getenv("DISPLAY")), "needs display")
  Rgtk4::gtkInit()
  btn <- Rgtk4::gtkButtonNew()
  expect_identical(getWidget(btn), btn)
  expect_identical(getBlock(btn), btn)
})
