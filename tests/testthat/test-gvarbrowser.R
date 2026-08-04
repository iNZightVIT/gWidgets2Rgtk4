skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("gvarbrowser set_value and get_index", {
  gw_vb_num <<- c(1, 2, 3)
  gw_vb_df <<- data.frame(x = 1:2)
  on.exit({
    if (exists("gw_vb_num", envir = .GlobalEnv, inherits = FALSE))
      rm(gw_vb_num, envir = .GlobalEnv)
    if (exists("gw_vb_df", envir = .GlobalEnv, inherits = FALSE))
      rm(gw_vb_df, envir = .GlobalEnv)
  }, add = TRUE)

  w <- gwindow("vb", visible = FALSE, width = 400, height = 400)
  vb <- gvarbrowser(container = w)
  ## Force a refresh so WSWatcher sees assigns
  vb$ws_model$update_state()
  vb$force_full_rebuild <- TRUE
  vb$update_view()

  vb$set_value("gw_vb_df")
  expect_equal(as.character(svalue(vb)), "gw_vb_df")
  idx <- vb$get_index()
  expect_true(is.numeric(idx) || is.integer(idx))
  expect_gte(length(idx), 1L)

  dispose(w)
})

test_that("gvarbrowser preserves selection across update_view", {
  gw_vb_a <<- 1L
  gw_vb_b <<- 2L
  on.exit({
    for (nm in c("gw_vb_a", "gw_vb_b", "gw_vb_c")) {
      if (exists(nm, envir = .GlobalEnv, inherits = FALSE))
        rm(list = nm, envir = .GlobalEnv)
    }
  }, add = TRUE)

  w <- gwindow("vb2", visible = FALSE, width = 400, height = 400)
  vb <- gvarbrowser(container = w)
  vb$ws_model$update_state()
  vb$force_full_rebuild <- TRUE
  vb$update_view()
  vb$set_value("gw_vb_a")
  expect_equal(as.character(svalue(vb)), "gw_vb_a")

  gw_vb_c <<- 3L
  vb$ws_model$update_state()
  vb$update_view()
  expect_equal(as.character(svalue(vb)), "gw_vb_a")

  dispose(w)
})
