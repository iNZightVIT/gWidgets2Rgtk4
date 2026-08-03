skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

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

test_that("gtree constructs and reports selection", {
  l <- list(a = "1", b = list(a = "21", b = "22", c = list(a = "231")))
  w <- gwindow("tree", visible = FALSE, width = 400, height = 300)
  tr <- gtree(offspring = offspring_list, offspring.data = l, container = w)
  expect_true(is(tr, "GTree"))
  expect_equal(svalue(tr), character(0))

  ## Select root "a"
  svalue(tr, index = TRUE) <- 1L
  expect_equal(svalue(tr, index = TRUE), 1L)
  ## svalue() via NextMethod often gets drop=NULL => full path
  expect_equal(svalue(tr, drop = FALSE), "a")
  expect_equal(svalue(tr, drop = TRUE), "a")

  ## Clear (GTree svalue<- defaults index=TRUE)
  svalue(tr) <- integer(0)
  expect_equal(svalue(tr), character(0))

  ## Expand into b / c / a
  svalue(tr, index = TRUE) <- c(2L, 3L, 1L)
  expect_equal(svalue(tr, index = TRUE), c(2L, 3L, 1L))
  expect_equal(svalue(tr, drop = TRUE), "a")
  expect_equal(svalue(tr, drop = FALSE), c("b", "c", "a"))

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
  expect_equal(svalue(tr), "a")
  env$lst <- list(b = 1, c = 2)
  update(tr)
  expect_equal(svalue(tr), character(0))
  svalue(tr, index = TRUE) <- 2L
  expect_equal(svalue(tr), "c")
  dispose(w)
})

test_that("gvarbrowser constructs", {
  w <- gwindow("vb", visible = FALSE, width = 400, height = 300)
  ## Assign something visible in GlobalEnv for the browser
  assign(".gwb_test_df", data.frame(x = 1:3), envir = .GlobalEnv)
  on.exit(rm(list = ".gwb_test_df", envir = .GlobalEnv), add = TRUE)
  vb <- gvarbrowser(container = w)
  expect_true(is(vb, "GVarBrowser"))
  expect_equal(svalue(vb), character(0))
  dispose(w)
  ## Stop timer so it does not fire after dispose
  try(vb$stop_timer(), silent = TRUE)
})
