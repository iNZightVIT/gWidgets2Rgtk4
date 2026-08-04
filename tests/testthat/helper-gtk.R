## Shared helpers for GTK-backed tests.

has_display <- function() {
  interactive() || nzchar(Sys.getenv("DISPLAY")) || nzchar(Sys.getenv("WAYLAND_DISPLAY"))
}

skip_if_no_display <- function() {
  skip_if_not(has_display(), "GTK requires a display")
}

## Rgtk4 returns GtkAlign as an extptr whose address is the enum value.
align_as_int <- function(align) {
  addr <- .Call("R_extptr_address", align, PACKAGE = "Rgtk4")
  if (is.null(addr) || identical(addr, "(nil)"))
    0L
  else
    as.integer(strtoi(addr, 16L))
}

## GTK4 Align: FILL=0, START=1, END=2, CENTER=3
.expect_align <- function(widget, halign = NULL, valign = NULL) {
  if (!is.null(halign))
    expect_identical(align_as_int(gtkWidgetGetHalign(widget)), as.integer(halign))
  if (!is.null(valign))
    expect_identical(align_as_int(gtkWidgetGetValign(widget)), as.integer(valign))
}

## Respond to a modal GtkDialog from an idle callback so gtkDialogRun returns.
respond_dialog <- function(dialog, response = -5L, delay_ms = 50L) {
  gTimeoutAdd(as.integer(delay_ms), function() {
    try(gtkDialogResponse(dialog, as.integer(response)), silent = TRUE)
    FALSE
  })
}

## Pump the main loop so ColumnView can build header title widgets.
pump_gtk <- function(iters = 40L, sleep = 0.01) {
  for (i in seq_len(as.integer(iters))) {
    try(gtkMainIterationDo(FALSE), silent = TRUE)
    Sys.sleep(sleep)
  }
  invisible(NULL)
}

.count_matching_controllers <- function(widgets, predicate) {
  n <- 0L
  for (w in widgets) {
    m <- tryCatch(gtkWidgetObserveControllers(w), error = function(e) NULL)
    if (is.null(m))
      next
    nn <- tryCatch(as.integer(gListModelGetNItems(m))[1], error = function(e) 0L)
    if (!length(nn) || is.na(nn) || nn < 1L)
      next
    for (j in seq_len(nn) - 1L) {
      ctrl <- tryCatch(gListModelGetObject(m, as.integer(j)), error = function(e) NULL)
      if (isTRUE(predicate(ctrl)))
        n <- n + 1L
    }
  }
  n
}

gdf_header_dnd_stats <- function(gd) {
  titles <- .dnd_columnview_header_titles(gd$widget)
  hdr <- .dnd_columnview_header_row(gd$widget)
  list(
    headers = length(titles),
    drag_sources = .count_matching_controllers(titles, .dnd_is_drag_source),
    title_clicks = .count_matching_controllers(titles, .dnd_is_gesture_click),
    header_drags = if (is.null(hdr)) 0L else
      .count_matching_controllers(list(hdr), .dnd_is_gesture_drag)
  )
}
