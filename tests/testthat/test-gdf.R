skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("gdf set_frame/get_frame round-trip", {
  w <- gwindow("df", visible = FALSE, width = 400, height = 300)
  df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  gd <- gdf(df, container = w)
  expect_true(is(gd, "GDf"))
  expect_equal(gd$get_frame(), df)
  expect_equal(names(gd), c("a", "b"))
  expect_equal(unname(dim(gd)["rows"]), 3)

  df2 <- data.frame(x = 10:11, y = c(TRUE, FALSE))
  gd$set_frame(df2)
  expect_equal(gd$get_frame(), df2)
  dispose(w)
})

test_that("gdf cell edit via commit_edit fires changed", {
  w <- gwindow("df2", visible = FALSE)
  gd <- gdf(data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE),
            container = w)
  hit <- NULL
  addHandlerChanged(gd, handler = function(h, ...) {
    hit <<- list(i = h$i, j = h$j, value = h$value)
  })
  gd$commit_edit(1L, 2L, "zz")
  expect_equal(hit$i, 1L)
  expect_equal(hit$j, 2L)
  expect_equal(hit$value, "zz")
  expect_equal(gd$get_frame()[1, 2], "zz")
  dispose(w)
})

test_that("gdf set_editable and remove_popup_menu / add_dnd_columns", {
  w <- gwindow("df3", visible = FALSE)
  gd <- gdf(data.frame(a = 1:2, b = 3:4), container = w)
  expect_true(gd$is_editable(1))
  gd$set_editable(FALSE, 1)
  expect_false(gd$is_editable(1))
  expect_silent(gd$remove_popup_menu())
  expect_silent(gd$add_dnd_columns())
  dispose(w)
})

test_that("gdf svalue index selects row", {
  w <- gwindow("df4", visible = FALSE)
  gd <- gdf(data.frame(id = letters[1:4]), container = w)
  svalue(gd, index = TRUE) <- 3
  expect_equal(svalue(gd, index = TRUE), 3L)
  dispose(w)
})

test_that("gdf [ and [<- replace frame", {
  w <- gwindow("df5", visible = FALSE)
  gd <- gdf(data.frame(a = 1), container = w)
  gd[] <- data.frame(p = 1:2, q = 3:4)
  expect_equal(gd[, "p"], 1:2)
  expect_equal(names(gd), c("p", "q"))
  dispose(w)
})
