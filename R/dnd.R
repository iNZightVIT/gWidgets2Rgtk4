##' @include gtk-misc.R
NULL

## Drag-and-drop helpers for GTK4.
## In-app payloads live in .dnd.env$active (object drops need not round-trip
## through Gdk content). GtkTextView ships its own DropTarget — strip it
## before attaching ours.
##
## IMPORTANT: gtk_drop_target_async_new() is (transfer full) on formats and
## unrefs what you pass. Never share one GdkContentFormats across multiple
## targets (UAF → segfault / unfinished DROPPING / wedged notebook).

.dnd.env <- new.env(parent = emptyenv())
.dnd.env$counter <- 0L
.dnd.env$string_gtype <- NULL
.dnd.env$active <- NULL

.dnd_next_key <- function() {
  .dnd.env$counter <- as.integer(.dnd.env$counter) + 1L
  sprintf("gwdnd-%d", .dnd.env$counter)
}

.dnd_string_gtype <- function() {
  gt <- .dnd.env$string_gtype
  if (is.null(gt) || (is.numeric(gt) && identical(as.numeric(gt)[1], 0))) {
    gt <- as.numeric(gTypeFromName("gchararray"))
    .dnd.env$string_gtype <- gt
  }
  gt
}

## Gdk.DragAction COPY / NONE
.dnd_action_copy <- 1L
.dnd_action_none <- 0L

.dnd_is_drop_controller <- function(ctrl) {
  if (is.null(ctrl))
    return(FALSE)
  cls <- class(ctrl)
  if (any(cls %in% c("GtkDropTarget", "GtkDropTargetAsync")))
    return(TRUE)
  gt <- attr(ctrl, "glib_type")
  is.character(gt) && any(gt %in% c("DropTarget", "DropTargetAsync",
                                    "GtkDropTarget", "GtkDropTargetAsync"))
}

## Remove built-in (and prior) drop controllers so ours alone owns the drop.
.dnd_remove_drop_controllers <- function(widget) {
  model <- tryCatch(gtkWidgetObserveControllers(widget), error = function(e) NULL)
  if (is.null(model))
    return(invisible(NULL))
  n <- tryCatch(as.integer(gListModelGetNItems(model))[1], error = function(e) 0L)
  if (!length(n) || is.na(n) || n < 1L)
    return(invisible(NULL))
  to_remove <- list()
  for (i in seq_len(n) - 1L) {
    ctrl <- tryCatch(gListModelGetObject(model, as.integer(i)), error = function(e) NULL)
    if (.dnd_is_drop_controller(ctrl))
      to_remove[[length(to_remove) + 1L]] <- ctrl
  }
  for (ctrl in to_remove) {
    tryCatch(gtkWidgetRemoveController(widget, ctrl), error = function(e) invisible(NULL))
  }
  invisible(NULL)
}

.dnd_text_provider <- function(text) {
  text <- paste(as.character(text), collapse = "\n")
  if (!length(text) || is.na(text[1]))
    text <- ""
  ## Heap GValue via anchored raw buffer. (gBytesNew + short payloads
  ## can trip Rgtk4 make_gobject_ptr false-positives on inline GBytes.)
  buf <- raw(64L)
  ptr <- rawToExtptr(buf)
  gt <- .dnd_string_gtype()
  gValueInit(ptr, gt)
  gValueSetString(ptr, text[1])
  prov <- gdkContentProviderNewForValue(ptr)
  ## Keep buf alive for the duration of this call; provider copies the value.
  force(buf)
  prov
}

.dnd_set_active <- function(payload) {
  .dnd.env$active <- payload
  invisible(NULL)
}

.dnd_clear_active <- function() {
  .dnd.env$active <- NULL
  invisible(NULL)
}

.dnd_get_active <- function() {
  .dnd.env$active
}

.dnd_store_object <- function(val) {
  key <- .dnd_next_key()
  assign(key, val, envir = .dnd.env)
  key
}

.dnd_clear_key <- function(key) {
  if (!is.null(key) && exists(key, envir = .dnd.env, inherits = FALSE))
    rm(list = key, envir = .dnd.env)
  invisible(NULL)
}

.dnd_resolve_dropdata <- function(text = NULL) {
  ## Prefer in-app active payload (avoids GdkDrop read failures).
  active <- .dnd_get_active()
  if (!is.null(active))
    return(active)
  text <- as.character(text)[1]
  if (is.na(text) || !nzchar(text))
    return(text)
  if (exists(text, envir = .dnd.env, inherits = FALSE))
    get(text, envir = .dnd.env, inherits = FALSE)
  else
    text
}

.dnd_finish_drop <- function(drop, ok = TRUE) {
  if (is.null(drop))
    return(invisible(NULL))
  tryCatch(
    gdkDropFinish(drop, if (isTRUE(ok)) .dnd_action_copy else .dnd_action_none),
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}

.dnd_is_drop_motion_controller <- function(ctrl) {
  if (is.null(ctrl))
    return(FALSE)
  if (inherits(ctrl, "GtkDropControllerMotion"))
    return(TRUE)
  gt <- attr(ctrl, "glib_type")
  is.character(gt) && any(gt %in% c("DropControllerMotion", "GtkDropControllerMotion"))
}

.dnd_remove_drop_motion_controllers <- function(widget) {
  model <- tryCatch(gtkWidgetObserveControllers(widget), error = function(e) NULL)
  if (is.null(model))
    return(invisible(NULL))
  n <- tryCatch(as.integer(gListModelGetNItems(model))[1], error = function(e) 0L)
  if (!length(n) || is.na(n) || n < 1L)
    return(invisible(NULL))
  to_remove <- list()
  for (i in seq_len(n) - 1L) {
    ctrl <- tryCatch(gListModelGetObject(model, as.integer(i)), error = function(e) NULL)
    if (.dnd_is_drop_motion_controller(ctrl))
      to_remove[[length(to_remove) + 1L]] <- ctrl
  }
  for (ctrl in to_remove) {
    tryCatch(gtkWidgetRemoveController(widget, ctrl), error = function(e) invisible(NULL))
  }
  invisible(NULL)
}

## GTK Notebook installs GtkDropControllerMotion on every tab *and* on the
## scroll arrows (even when few tabs fit). Hovering those during any drag
## starts a timer that cycles pages — looks like tabs changing at random.
## Walk the notebook widget tree and strip all of them.
.dnd_notebook_disable_tab_hover_switch <- function(nb_widget) {
  walk <- function(w, depth = 0L) {
    if (is.null(w) || depth > 12L)
      return()
    .dnd_remove_drop_motion_controllers(w)
    ch <- tryCatch(gtkWidgetGetFirstChild(w), error = function(e) NULL)
    while (!is.null(ch)) {
      walk(ch, depth + 1L)
      ch <- tryCatch(gtkWidgetGetNextSibling(ch), error = function(e) NULL)
    }
  }
  tryCatch(walk(nb_widget), error = function(e) invisible(NULL))
  invisible(NULL)
}

.dnd_is_drag_source <- function(ctrl) {
  if (is.null(ctrl))
    return(FALSE)
  if (inherits(ctrl, "GtkDragSource"))
    return(TRUE)
  gt <- attr(ctrl, "glib_type")
  is.character(gt) && any(gt %in% c("DragSource", "GtkDragSource"))
}

.dnd_is_gesture_click <- function(ctrl) {
  if (is.null(ctrl))
    return(FALSE)
  if (inherits(ctrl, "GtkGestureClick"))
    return(TRUE)
  gt <- attr(ctrl, "glib_type")
  is.character(gt) && any(gt %in% c("GestureClick", "GtkGestureClick"))
}

.dnd_is_gesture_drag <- function(ctrl) {
  if (is.null(ctrl))
    return(FALSE)
  ## DragSource subclasses GestureDrag — do not treat it as a plain drag.
  if (.dnd_is_drag_source(ctrl))
    return(FALSE)
  if (inherits(ctrl, "GtkGestureDrag"))
    return(TRUE)
  gt <- attr(ctrl, "glib_type")
  is.character(gt) && any(gt %in% c("GestureDrag", "GtkGestureDrag"))
}

.dnd_remove_controllers_matching <- function(widget, predicate) {
  model <- tryCatch(gtkWidgetObserveControllers(widget), error = function(e) NULL)
  if (is.null(model))
    return(invisible(NULL))
  n <- tryCatch(as.integer(gListModelGetNItems(model))[1], error = function(e) 0L)
  if (!length(n) || is.na(n) || n < 1L)
    return(invisible(NULL))
  to_remove <- list()
  for (i in seq_len(n) - 1L) {
    ctrl <- tryCatch(gListModelGetObject(model, as.integer(i)), error = function(e) NULL)
    if (isTRUE(predicate(ctrl)))
      to_remove[[length(to_remove) + 1L]] <- ctrl
  }
  for (ctrl in to_remove) {
    tryCatch(gtkWidgetRemoveController(widget, ctrl), error = function(e) invisible(NULL))
  }
  invisible(NULL)
}

.dnd_remove_drag_sources <- function(widget) {
  .dnd_remove_controllers_matching(widget, .dnd_is_drag_source)
}

.dnd_remove_gesture_clicks <- function(widget) {
  .dnd_remove_controllers_matching(widget, .dnd_is_gesture_click)
}

.dnd_remove_gesture_drags <- function(widget) {
  .dnd_remove_controllers_matching(widget, .dnd_is_gesture_drag)
}

## GtkColumnView header row: css "header" → children css "button"
## (GtkColumnViewTitle), one per column, in column order.
.dnd_columnview_header_row <- function(view) {
  if (is.null(view))
    return(NULL)
  ch <- tryCatch(gtkWidgetGetFirstChild(view), error = function(e) NULL)
  while (!is.null(ch)) {
    css <- tryCatch(gtkWidgetGetCssName(ch), error = function(e) "")
    if (identical(as.character(css)[1], "header"))
      return(ch)
    ch <- tryCatch(gtkWidgetGetNextSibling(ch), error = function(e) NULL)
  }
  NULL
}

.dnd_columnview_header_titles <- function(view) {
  out <- list()
  header <- .dnd_columnview_header_row(view)
  if (is.null(header))
    return(out)
  btn <- tryCatch(gtkWidgetGetFirstChild(header), error = function(e) NULL)
  while (!is.null(btn)) {
    css <- tryCatch(gtkWidgetGetCssName(btn), error = function(e) "")
    if (identical(as.character(css)[1], "button"))
      out[[length(out) + 1L]] <- btn
    btn <- tryCatch(gtkWidgetGetNextSibling(btn), error = function(e) NULL)
  }
  out
}

## ColumnViewTitle installs GestureClick that CLAIMS on press (sort/menu),
## which denies GtkDragSource. Header GestureDrag owns resize/reorder.
## Strip those before attaching our text DragSource.
.dnd_prepare_columnview_headers_for_dnd <- function(view) {
  header <- .dnd_columnview_header_row(view)
  if (!is.null(header)) {
    .dnd_remove_gesture_drags(header)
    .dnd_remove_gesture_clicks(header)
  }
  for (title in .dnd_columnview_header_titles(view))
    .dnd_remove_gesture_clicks(title)
  invisible(NULL)
}

## Attach a text DragSource; get_text() is called from prepare.
## Re-entrant: strips existing DragSources on the widget first.
.dnd_attach_text_source <- function(widget, get_text) {
  if (is.null(widget) || !is.function(get_text))
    return(invisible(NULL))
  force(get_text)
  .dnd_remove_drag_sources(widget)
  src <- gtkDragSourceNew()
  gtkDragSourceSetActions(src, .dnd_action_copy)
  tryCatch(
    gtkEventControllerSetPropagationPhase(src, 1L), ## CAPTURE
    error = function(e) invisible(NULL)
  )
  state <- new.env(parent = emptyenv())
  state$provider <- NULL
  gSignalConnectR(src, "prepare", function(source, x, y) {
    wire <- tryCatch({
      val <- get_text()
      if (is.null(val) || (length(val) == 0 && !is.list(val)))
        ""
      else
        paste(as.character(val), collapse = "\n")
    }, error = function(e) {
      warning("column DnD prepare error: ", conditionMessage(e), call. = FALSE)
      ""
    })
    .dnd_set_active(wire)
    provider <- .dnd_text_provider(wire)
    state$provider <- provider
    provider
  })
  gSignalConnectR(src, "drag-end", function(source, drag, delete_data) {
    state$provider <- NULL
    .dnd_clear_active()
    NULL
  })
  gtkWidgetAddController(widget, src)
  invisible(src)
}

## GTK ColumnViewTitle opens header-menu via GestureClick secondary.
## DnD prep strips that gesture — restore a secondary-only click that
## pops a PopoverMenu from the column's header GMenuModel.
.dnd_attach_header_menu_click <- function(title, menu_model, host_widget,
                                          action_prefix, action_group) {
  if (is.null(title) || is.null(menu_model) || is.null(action_group))
    return(invisible(NULL))
  force(menu_model)
  force(host_widget)
  force(action_prefix)
  force(action_group)
  force(title)
  g <- gtkGestureClickNew()
  ## Listen for all buttons; only act on secondary (3) so primary stays free for drag.
  gtkGestureSingleSetButton(g, 0L)
  tryCatch(
    gtkEventControllerSetPropagationPhase(g, 2L), ## TARGET
    error = function(e) invisible(NULL)
  )
  gSignalConnectR(g, "pressed", function(gest, n_press, x, y) {
    event <- tryCatch(gtkEventControllerGetCurrentEvent(gest),
                      error = function(e) NULL)
    event_btn <- tryCatch(as.integer(gdkButtonEventGetButton(event)),
                          error = function(e) 0L)
    if (!identical(event_btn, 3L))
      return()
    pop <- gtkPopoverMenuNewFromModel(menu_model)
    try(gtkPopoverSetHasArrow(pop, FALSE), silent = TRUE)
    ## Parent to title so it points at the header; actions on host + title.
    try(gtkWidgetSetParent(pop, title), silent = TRUE)
    try(gtkWidgetInsertActionGroup(title, action_prefix, action_group),
        silent = TRUE)
    if (!is.null(host_widget))
      try(gtkWidgetInsertActionGroup(host_widget, action_prefix, action_group),
          silent = TRUE)
    tryCatch({
      gtkPopoverSetOffset(pop, as.integer(x - 8L), as.integer(-(20L - y)))
    }, error = function(e) invisible(NULL))
    gtkPopoverPopup(pop)
  })
  gtkWidgetAddController(title, g)
  invisible(g)
}
