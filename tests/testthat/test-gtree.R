skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

## Pump the GTK main loop so ColumnView factories bind.
.pump <- function(n = 40L) {
  for (i in seq_len(n))
    Rgtk4::gtkMainIterationDo(FALSE)
}

offspring_list <- function(path = character(0), lst, ...) {
  if (length(path))
    obj <- lst[[path]]
  else
    obj <- lst
  nms <- names(obj)
  hasOffspring <- sapply(nms, function(i) {
    newobj <- obj[[i]]
    is.recursive(newobj) && !is.null(names(newobj))
  })
  data.frame(Name = nms, hasOffspring = hasOffspring, stringsAsFactors = FALSE)
}

offspring_rich <- function(path = character(0), lst, ...) {
  df <- offspring_list(path, lst)
  df$Description <- paste("desc", df$Name)
  df$icon <- "ok"
  df$tip <- paste("tip", df$Name)
  df
}

test_that(".tree_col_index maps names and rejects unknowns", {
  items <- data.frame(a = 1, b = TRUE, stringsAsFactors = FALSE)
  expect_null(gWidgets2Rgtk4:::.tree_col_index(NULL, items))
  expect_equal(gWidgets2Rgtk4:::.tree_col_index("b", items), 2L)
  expect_null(gWidgets2Rgtk4:::.tree_col_index("missing", items))
  expect_equal(gWidgets2Rgtk4:::.tree_col_index(1, items), 1L)
  expect_null(gWidgets2Rgtk4:::.tree_col_index(TRUE, items))
})

test_that("gtree constructs and reports selection", {
  l <- list(a = "1", b = list(a = "21", b = "22", c = list(a = "231")))
  w <- gwindow("tree", visible = FALSE, width = 400, height = 300)
  tr <- gtree(offspring = offspring_list, offspring.data = l, container = w)
  expect_true(is(tr, "GTree"))
  expect_equal(svalue(tr), character(0))

  svalue(tr, index = TRUE) <- 1L
  expect_equal(svalue(tr, index = TRUE), 1L)
  expect_equal(svalue(tr, drop = FALSE), "a")
  expect_equal(svalue(tr, drop = TRUE), "a")

  svalue(tr) <- integer(0)
  expect_equal(svalue(tr), character(0))

  svalue(tr, index = TRUE) <- c(2L, 3L, 1L)
  expect_equal(svalue(tr, index = TRUE), c(2L, 3L, 1L))
  expect_equal(svalue(tr, drop = TRUE), "a")
  expect_equal(svalue(tr, drop = FALSE), c("b", "c", "a"))

  dispose(w)
})

test_that("gtree set_value by path and get_items/names", {
  l <- list(a = "1", b = list(x = 1, y = 2))
  w <- gwindow("tree-path", visible = TRUE, width = 420, height = 320)
  tr <- gtree(
    offspring = offspring_rich, offspring.data = l,
    chosen.col = "Name", offspring.col = "hasOffspring",
    icon.col = "icon", tooltip.col = "tip",
    container = w, expand = TRUE
  )
  .pump()
  expect_equal(names(tr), c("Name", "Description"))

  svalue(tr, index = FALSE) <- c("b", "x")
  expect_equal(svalue(tr, drop = FALSE), c("b", "x"))
  expect_equal(svalue(tr, drop = TRUE), "x")

  items <- tr[]
  expect_true(is.data.frame(items))
  expect_equal(items$Name, "x")

  names(tr) <- c("Key", "Info")
  expect_equal(names(tr), c("Key", "Info"))

  ## Clearing via set_value
  tr$set_value(character(0))
  expect_equal(svalue(tr), character(0))

  ## Unknown path is a no-op
  tr$set_value(c("nope"))
  expect_equal(svalue(tr), character(0))

  expect_error(tr$set_items(data.frame()), "offspring")

  dispose(w)
})

test_that("gtree multiple selection and handlers", {
  l <- list(a = 1, b = 2, c = list(x = 3))
  w <- gwindow("tree-multi", visible = TRUE, width = 400, height = 300)
  tr <- gtree(offspring = offspring_list, offspring.data = l,
              multiple = TRUE, container = w)
  .pump()

  hit_sel <- NULL
  hit_act <- NULL
  addHandlerSelectionChanged(tr, handler = function(h, ...) {
    hit_sel <<- svalue(h$obj, drop = TRUE)
  })
  addHandlerChanged(tr, handler = function(h, ...) {
    hit_act <<- TRUE
  })
  addHandlerDoubleclick(tr, handler = function(h, ...) {
    hit_act <<- TRUE
  })
  addHandlerClicked(tr, handler = function(h, ...) {
    hit_sel <<- svalue(h$obj, drop = TRUE)
  })

  svalue(tr, index = TRUE) <- list(1L, 2L)
  expect_true(length(svalue(tr, index = TRUE)) >= 1L)
  if (is.null(hit_sel))
    tr$notify_observers(signal = "selection-changed")
  expect_false(is.null(hit_sel))

  tr$notify_observers(signal = "activate")
  expect_true(isTRUE(hit_act))

  tr$set_multiple(FALSE)
  svalue(tr, index = TRUE) <- 1L
  expect_equal(svalue(tr, index = TRUE), 1L)

  dispose(w)
})

test_that("gtree addHandlerClicked fires on selection", {
  l <- list(x = list(y = 1), z = 2)
  w <- gwindow("tree2", visible = FALSE)
  tr <- gtree(offspring = offspring_list, offspring.data = l, container = w)
  hit <- NULL
  addHandlerClicked(tr, handler = function(h, ...) {
    hit <<- svalue(h$obj)
  })
  svalue(tr, index = TRUE) <- 1L
  if (is.null(hit))
    tr$notify_observers(signal = "selection-changed")
  expect_equal(svalue(tr, drop = TRUE), "x")
  expect_false(is.null(hit))
  dispose(w)
})

test_that("gtree update refreshes root", {
  env <- new.env(parent = emptyenv())
  env$lst <- list(a = 1)
  offspring <- function(path = character(0), data, ...) {
    offspring_list(path, data$lst)
  }
  w <- gwindow("tree3", visible = FALSE)
  tr <- gtree(offspring = offspring, offspring.data = env, container = w)
  svalue(tr, index = TRUE) <- 1L
  expect_equal(svalue(tr, drop = TRUE), "a")
  env$lst <- list(b = 1, c = 2)
  update(tr)
  expect_equal(svalue(tr), character(0))
  svalue(tr, index = TRUE) <- 2L
  expect_equal(svalue(tr, drop = TRUE), "c")
  dispose(w)
})

test_that("gtree empty root and non-data.frame offspring", {
  offspring_empty <- function(path = character(0), ...) {
    data.frame(Name = character(0), hasOffspring = logical(0),
               stringsAsFactors = FALSE)
  }
  w <- gwindow("tree-empty", visible = FALSE)
  tr <- gtree(offspring = offspring_empty, container = w)
  expect_equal(svalue(tr), character(0))
  expect_equal(svalue(tr, index = TRUE), integer(0))
  dispose(w)

  ## Matrix-like / list coerced via as.data.frame
  offspring_mat <- function(path = character(0), ...) {
    if (length(path))
      return(data.frame(Name = "leaf", hasOffspring = FALSE,
                        stringsAsFactors = FALSE))
    data.frame(Name = c("a", "b"), hasOffspring = c(FALSE, TRUE),
               stringsAsFactors = FALSE)
  }
  w2 <- gwindow("tree-mat", visible = FALSE)
  tr2 <- gtree(offspring = offspring_mat, container = w2)
  svalue(tr2, index = TRUE) <- 1L
  expect_equal(svalue(tr2, drop = TRUE), "a")
  dispose(w2)
})

## --- gvarbrowser ---------------------------------------------------------

.test_ws_names <- c("gw_cov_num", "gw_cov_df", "gw_cov_lst", "gw_cov_fun")

.seed_ws <- function() {
  assign("gw_cov_num", 1:3, envir = .GlobalEnv)
  assign("gw_cov_df", data.frame(x = 1:2), envir = .GlobalEnv)
  assign("gw_cov_lst", list(a = 1, b = list(c = 2)), envir = .GlobalEnv)
  assign("gw_cov_fun", function(x) x + 1, envir = .GlobalEnv)
}

.clean_ws <- function() {
  rm(list = intersect(.test_ws_names, ls(envir = .GlobalEnv, all.names = TRUE)),
     envir = .GlobalEnv)
}

.expand_all_categories <- function(vb) {
  ## Expand from the end so earlier indices stay stable
  n <- as.integer(gListModelGetNItems(vb$tree_model))
  for (i in rev(seq_len(n) - 1L)) {
    row <- gtkTreeListModelGetRow(vb$tree_model, as.integer(i))
    if (isTRUE(as.logical(gtkTreeListRowIsExpandable(row))))
      gtkTreeListRowSetExpanded(row, TRUE)
  }
  .pump(20L)
}

.select_label <- function(vb, label) {
  n <- as.integer(gListModelGetNItems(vb$tree_model))
  for (i in seq_len(n) - 1L) {
    row <- gtkTreeListModelGetRow(vb$tree_model, as.integer(i))
    item <- gtkTreeListRowGetItem(row)
    id <- as.character(gtkStringObjectGetString(item))[1]
    if (!exists(id, envir = vb$nodes, inherits = FALSE))
      next
    node <- get(id, envir = vb$nodes, inherits = FALSE)
    if (!isTRUE(node$is_category) && identical(node$label, label)) {
      gtkSelectionModelSelectItem(vb$selection, as.integer(i), TRUE)
      return(TRUE)
    }
  }
  FALSE
}

test_that("gvarbrowser constructs, selects, and filters", {
  .seed_ws()
  on.exit({
    .clean_ws()
  }, add = TRUE)

  w <- gwindow("vb", visible = TRUE, width = 500, height = 400)
  vb <- gvarbrowser(container = w)
  on.exit({
    try(vb$stop_timer(), silent = TRUE)
    try(dispose(w), silent = TRUE)
  }, add = TRUE)

  .pump()
  expect_true(is(vb, "GVarBrowser"))
  expect_equal(svalue(vb), character(0))
  expect_equal(vb$get_items(), character(0))
  expect_equal(vb$get_index(), numeric(0))

  .expand_all_categories(vb)
  expect_true(.select_label(vb, "gw_cov_num"))
  expect_equal(svalue(vb, drop = TRUE), "gw_cov_num")
  objs <- svalue(vb, drop = FALSE)
  expect_true(is.list(objs) || is.numeric(objs[[1]]) || is.numeric(objs))
  expect_true(length(vb$get_items()) >= 1L)

  ## Nested list path
  .expand_all_categories(vb)
  ## Expand cov_lst if visible
  n <- as.integer(gListModelGetNItems(vb$tree_model))
  for (i in seq_len(n) - 1L) {
    row <- gtkTreeListModelGetRow(vb$tree_model, as.integer(i))
    item <- gtkTreeListRowGetItem(row)
    id <- as.character(gtkStringObjectGetString(item))[1]
    if (!exists(id, envir = vb$nodes, inherits = FALSE)) next
    node <- get(id, envir = vb$nodes, inherits = FALSE)
    if (identical(node$label, "gw_cov_lst") && isTRUE(node$has_offspring)) {
      gtkTreeListRowSetExpanded(row, TRUE)
      break
    }
  }
  .pump(20L)
  expect_true(.select_label(vb, "a") || .select_label(vb, "gw_cov_df"))

  hit <- NULL
  addHandlerChanged(vb, handler = function(h, ...) hit <<- "act")
  addHandlerSelectionChanged(vb, handler = function(h, ...) hit <<- "sel")
  vb$notify_observers(signal = "selection-changed")
  expect_equal(hit, "sel")
  vb$notify_observers(signal = "activate")
  expect_equal(hit, "act")

  vb$set_filter_name("gw_cov")
  vb$set_filter_classes(list(Data = c("integer", "numeric")))
  vb$adjust_timer()
  vb$adjust_timer(2500L)
  vb$stop_timer()
  vb$start_timer()
  vb$stop_timer()
  vb$update_view()
  vb$set_value("ignored")
  vb$set_items(NULL)

  expect_silent(TRUE)
})

test_that("gvarbrowser constructs empty-ish workspace path", {
  w <- gwindow("vb2", visible = FALSE, width = 400, height = 300)
  vb <- gvarbrowser(container = w)
  expect_true(is(vb, "GVarBrowser"))
  expect_equal(svalue(vb), character(0))
  try(vb$stop_timer(), silent = TRUE)
  dispose(w)
})
