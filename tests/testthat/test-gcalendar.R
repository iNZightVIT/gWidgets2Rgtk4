skip_if_not_installed("gWidgets2")
skip_if_no_display()

options(guiToolkit = "Rgtk4")

test_that("gcalendar get/set value and format", {
  w <- gwindow("cal", visible = FALSE)
  c1 <- gcalendar(cont = w)
  expect_true(is(c1, "GCalendar"))
  expect_equal(svalue(c1), "")

  svalue(c1) <- "2000-01-31"
  expect_equal(svalue(c1), "2000-01-31")
  expect_equal(svalue(c1, drop = FALSE), as.Date("2000-01-31"))

  c2 <- gcalendar("01/31/2000", cont = w, format = "%m/%d/%Y")
  expect_equal(svalue(c2), "01/31/2000")
  expect_equal(svalue(c2, drop = FALSE), as.Date("2000-01-31"))

  svalue(c2) <- "02/29/2000"
  expect_equal(svalue(c2), "02/29/2000")

  ## invalid text -> empty / NA Date
  svalue(c1) <- "not-a-date"
  expect_equal(svalue(c1), "")
  expect_true(is.na(svalue(c1, drop = FALSE)))

  dispose(w)
})

test_that("gcalendar pick_date OK writes selection", {
  w <- gwindow("cal-pick", visible = FALSE)
  cal <- gcalendar("2000-01-15", cont = w)

  local_mocked_bindings(
    gtkDialogRun = function(dialog) {
      area <- gtkDialogGetContentArea(dialog)
      child <- gtkWidgetGetFirstChild(area)
      dt <- gDateTimeNewLocal(2000L, 6L, 20L, 0L, 0L, 0)
      gtkCalendarSelectDay(child, dt)
      -5L ## OK
    }
  )

  cal$pick_date()
  expect_equal(svalue(cal), "2000-06-20")
  dispose(w)
})

test_that("gcalendar pick_date Cancel leaves value", {
  w <- gwindow("cal-cancel", visible = FALSE)
  cal <- gcalendar("1999-12-31", cont = w)

  local_mocked_bindings(
    gtkDialogRun = function(dialog) -6L
  )

  cal$pick_date()
  expect_equal(svalue(cal), "1999-12-31")
  dispose(w)
})

test_that("gcalendar pick_date respects custom format", {
  w <- gwindow("cal-fmt", visible = FALSE)
  cal <- gcalendar("01/15/2000", cont = w, format = "%m/%d/%Y")

  local_mocked_bindings(
    gtkDialogRun = function(dialog) {
      area <- gtkDialogGetContentArea(dialog)
      child <- gtkWidgetGetFirstChild(area)
      dt <- gDateTimeNewLocal(2001L, 3L, 4L, 0L, 0L, 0)
      gtkCalendarSelectDay(child, dt)
      -5L
    }
  )

  cal$pick_date()
  expect_equal(svalue(cal), "03/04/2001")
  dispose(w)
})
