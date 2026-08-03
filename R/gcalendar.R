##' @include GWidget.R
NULL

##' Toolkit gcalendar constructor
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gcalendar guiWidgetsToolkitRgtk4
.gcalendar.guiWidgetsToolkitRgtk4 <- function(toolkit,
                                              text = "",
                                              format = "%Y-%m-%d",
                                              handler = NULL, action = NULL,
                                              container = NULL, ...) {
  GCalendar$new(toolkit,
                text = text,
                format = format,
                handler = handler, action = action,
                container = container, ...)
}

## Entry + button that pops a modal GtkCalendar dialog.
## GTK4 removed day-selected-double-click; OK/Cancel matches dialog patterns.
GCalendar <- setRefClass(
  "GCalendar",
  contains = "GWidget",
  fields = list(
    format = "character",
    button = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL,
                          text = "",
                          format = "%Y-%m-%d",
                          handler = NULL, action = NULL,
                          container = NULL, ...) {
      widget <<- gtkEntryNew()
      if (nzchar(text))
        gtkEditableSetText(widget, as.character(text)[1])
      button <<- gtkButtonNewWithLabel("Date...")
      block <<- gtkBoxNew(.GtkOrientation$HORIZONTAL, 5L)
      gtkWidgetSetHexpand(widget, TRUE)
      gtkBoxAppend(block, widget)
      gtkBoxAppend(block, button)
      initFields(format = as.character(format)[1],
                 change_signal = "activate")
      gSignalConnectR(button, "clicked", function(...) {
        .self$pick_date()
      })
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    ## Parse entry text with `format`; return Date or NA.
    parse_date = function(val = NULL) {
      if (is.null(val))
        val <- gtkEditableGetText(widget)
      val <- as.character(val)[1]
      if (!nzchar(val))
        return(as.Date(NA))
      d <- try(as.Date(val, format = format), silent = TRUE)
      if (inherits(d, "try-error") || is.na(d))
        as.Date(NA)
      else
        d
    },
    ## Modal calendar dialog; on OK write formatted date into the entry.
    pick_date = function() {
      parent_win <- tryCatch(getTopLevel(.self), error = function(e) NULL)
      .parent <- .dialog_ensure_parent(parent_win)
      dlg <- gtkDialogNew()
      gtkWindowSetTitle(dlg, "Select date")
      gtkWindowSetTransientFor(dlg, .parent)
      gtkWindowSetModal(dlg, TRUE)
      gtkDialogAddButton(dlg, "OK", -5L)
      gtkDialogAddButton(dlg, "Cancel", -6L)
      gtkDialogSetDefaultResponse(dlg, -5L)

      cal <- gtkCalendarNew()
      cur <- parse_date()
      if (!is.na(cur)) {
        dt <- gDateTimeNewLocal(
          as.integer(format(cur, "%Y")),
          as.integer(format(cur, "%m")),
          as.integer(format(cur, "%d")),
          0L, 0L, 0
        )
        if (!is.null(dt))
          gtkCalendarSelectDay(cal, dt)
      }
      area <- gtkDialogGetContentArea(dlg)
      gtkBoxAppend(area, cal)

      gtkWidgetSetVisible(dlg, TRUE)
      response <- gtkDialogRun(dlg)
      if (response %in% c(-5L, -3L, -8L)) {
        gdt <- gtkCalendarGetDate(cal)
        if (!is.null(gdt)) {
          ymd <- gDateTimeGetYmd(gdt)
          d <- as.Date(sprintf("%04d-%02d-%02d",
                               as.integer(ymd$year),
                               as.integer(ymd$month),
                               as.integer(ymd$day)))
          set_value(format(d, format = format))
        }
      }
      try(gtkWindowDestroy(dlg), silent = TRUE)
      invisible(NULL)
    },
    get_value = function(drop = TRUE, ...) {
      cur_date <- parse_date()
      if (missing(drop) || is.null(drop) || isTRUE(drop))
        if (is.na(cur_date)) "" else format(cur_date, format = format)
      else
        cur_date
    },
    set_value = function(value, ...) {
      gtkEditableSetText(widget, as.character(value)[1])
      invoke_change_handler()
    }
  )
)
