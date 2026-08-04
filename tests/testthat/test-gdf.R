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
  expect_false(isTRUE(as.logical(gtkColumnViewGetReorderable(gd$widget))))

  visible(w) <- TRUE
  pump_gtk()
  expect_silent(gd$add_dnd_columns())
  st <- gdf_header_dnd_stats(gd)
  expect_equal(st$headers, 2L)
  expect_equal(st$drag_sources, 2L)
  ## Secondary-click GestureClick restored for header menus after DnD strip
  expect_equal(st$title_clicks, 2L)
  expect_equal(st$header_drags, 0L)
  ## Re-entrant: does not stack sources
  gd$add_dnd_columns()
  expect_equal(gdf_header_dnd_stats(gd)$drag_sources, 2L)
  expect_equal(gdf_header_dnd_stats(gd)$title_clicks, 2L)
  dispose(w)
})

test_that("gdf column DnD rewires after set_frame", {
  w <- gwindow("df-dnd", visible = FALSE, width = 420, height = 280)
  gd <- gdf(data.frame(mpg = 1:2, cyl = 3:4), container = w)
  visible(w) <- TRUE
  pump_gtk()
  gd$add_dnd_columns()
  expect_equal(gdf_header_dnd_stats(gd)$drag_sources, 2L)

  ## Per-column prepare closures (local() env) — attach and resolve payload.
  titles <- .dnd_columnview_header_titles(gd$widget)
  payloads <- c("mpg", "cyl")
  for (k in seq_along(titles)) {
    local({
      nm <- payloads[k]
      .dnd_attach_text_source(titles[[k]], function() nm)
      ## Mimic prepare side-effect used by drop targets
      .dnd_set_active(nm)
      expect_equal(.dnd_resolve_dropdata(), nm)
      .dnd_clear_active()
    })
  }
  expect_equal(.count_matching_controllers(titles, .dnd_is_drag_source), 2L)

  gd$set_frame(data.frame(hp = 1:2, wt = 3:4, qsec = 5:6))
  pump_gtk(50L)
  ## dnd_columns_wanted stays TRUE — rewire without another add_dnd_columns.
  st <- gdf_header_dnd_stats(gd)
  expect_equal(st$headers, 3L)
  expect_equal(st$drag_sources, 3L)
  expect_equal(st$title_clicks, 3L)
  expect_equal(names(gd), c("hp", "wt", "qsec"))
  dispose(w)
})

test_that("gdf add_dnd_columns on empty frame is silent", {
  w <- gwindow("df-empty", visible = FALSE)
  gd <- gdf(data.frame(), container = w)
  visible(w) <- TRUE
  pump_gtk(20L)
  expect_silent(gd$add_dnd_columns())
  expect_equal(gdf_header_dnd_stats(gd)$headers, 0L)
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

test_that("gdf column mutate helpers", {
  w <- gwindow("df-mut", visible = FALSE)
  gd <- gdf(data.frame(mpg = 1:3, cyl = 4:6), container = w)
  gd$insert_column(2L, letters[1:3], "letter")
  expect_equal(names(gd), c("mpg", "letter", "cyl"))
  gd$coerce_column(1L, as.character)
  expect_true(is.character(gd$get_frame()$mpg))
  gd$replace_column(2L, factor(c("x", "y", "x")))
  expect_true(is.factor(gd$get_frame()$letter))
  gd$remove_column(2L)
  expect_equal(names(gd), c("mpg", "cyl"))
  expect_false(gd$can_undo())
  expect_silent(gd$undo())
  dispose(w)
})

test_that("gdf header menus coexist with add_dnd_columns", {
  w <- gwindow("df-hdr", visible = FALSE, width = 420, height = 280)
  gd <- gdf(data.frame(a = 1:2, b = 3:4), container = w)
  expect_equal(length(gd$header_action_prefixes), 2L)
  expect_equal(length(gd$header_action_groups), 2L)
  visible(w) <- TRUE
  pump_gtk()
  gd$add_dnd_columns()
  ## Menus kept; DnD sources present; secondary clicks restored
  expect_equal(length(gd$header_action_prefixes), 2L)
  st <- gdf_header_dnd_stats(gd)
  expect_equal(st$drag_sources, 2L)
  expect_equal(st$title_clicks, 2L)
  dispose(w)
})
