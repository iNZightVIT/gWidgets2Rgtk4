##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gradio guiWidgetsToolkitRgtk4
.gradio.guiWidgetsToolkitRgtk4 <- function(toolkit, items, selected = 1, horizontal = FALSE,
                                           handler = NULL, action = NULL, container = NULL, ...) {
  GRadio$new(toolkit, items, selected, horizontal, handler, action, container, ...)
}

GRadio <- setRefClass(
  "GRadio",
  contains = "GWidgetWithItems",
  methods = list(
    initialize = function(toolkit, items, selected, horizontal, handler, action, container, ...) {
      widgets <<- list()
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      block <<- gtkBoxNew(orient, 2L)
      widget <<- block
      change_signal <<- "toggled"
      set_items(value = items)
      set_index(selected)
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = TRUE, ...) get_items(get_index()),
    set_value = function(value, drop = TRUE, ...) {
      set_index(pmatch(value, get_items()))
    },
    get_index = function(...) {
      which(vapply(widgets, function(w) as.logical(gtkCheckButtonGetActive(w)), logical(1)))
    },
    set_index = function(value, ...) {
      idx <- as.integer(value)[1]
      if (!is.na(idx) && idx >= 1 && idx <= length(widgets))
        gtkCheckButtonSetActive(widgets[[idx]], TRUE)
    },
    get_items = function(i, ...) {
      items <- vapply(widgets, function(w) as.character(gtkCheckButtonGetLabel(w)), character(1))
      if (missing(i)) items else items[i]
    },
    set_items = function(value, i, ...) {
      while (!is.null(ch <- tryCatch(gtkWidgetGetFirstChild(block), error = function(e) NULL)))
        gtkBoxRemove(block, ch)
      value <- as.character(value)
      btns <- lapply(value, gtkCheckButtonNewWithLabel)
      if (length(btns) > 1) {
        for (i in seq_along(btns)[-1])
          gtkCheckButtonSetGroup(btns[[i]], btns[[1]])
      }
      widgets <<- btns
      for (w in widgets) {
        gtkBoxAppend(block, w)
        gSignalConnectR(w, "toggled", function(btn) {
          if (as.logical(gtkCheckButtonGetActive(btn)))
            .self$notify_observers(signal = "toggled")
        })
      }
    },
    get_length = function(...) length(widgets),
    get_enabled = function() as.logical(gtkWidgetGetSensitive(block)),
    set_enabled = function(value) gtkWidgetSetSensitive(block, as.logical(value)),
    get_visible = function() as.logical(gtkWidgetGetVisible(block)),
    set_visible = function(value) gtkWidgetSetVisible(block, as.logical(value))
  )
)
