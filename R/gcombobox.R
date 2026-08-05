##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gcombobox guiWidgetsToolkitRgtk4
.gcombobox.guiWidgetsToolkitRgtk4 <- function(toolkit, items, selected = 1, editable = FALSE,
                                              coerce.with = NULL, handler = NULL, action = NULL,
                                              container = NULL, ...) {
  if (editable)
    GComboBoxWithEntry$new(toolkit, items, selected = selected, coerce.with = coerce.with,
                           handler = handler, action = action, container = container, ...)
  else
    GComboBoxNoEntry$new(toolkit, items, selected = selected, coerce.with = coerce.with,
                         handler = handler, action = action, container = container, ...)
}

GComboBox <- setRefClass(
  "GComboBox",
  contains = "GWidget",
  fields = list(items = "ANY"),
  methods = list(
    get_index = function(...) {
      idx <- as.integer(gtkComboBoxGetActive(widget))
      if (length(idx) < 1L || is.na(idx) || idx < 0L) NA_integer_ else idx + 1L
    },
    set_index = function(value, ...) {
      value <- as.integer(value)[1]
      n <- get_length()
      if (length(value) < 1L || is.na(value) || value < 1L) {
        gtkComboBoxSetActive(widget, -1L)
      } else {
        value <- min(max(1L, value), max(n, 1L))
        gtkComboBoxSetActive(widget, value - 1L)
      }
    },
    get_length = function(...) length(items),
    add_handler_clicked = function(handler, action = NULL, ...) {
      add_handler("changed", handler, action = action, ...)
    }
  )
)

GComboBoxNoEntry <- setRefClass(
  "GComboBoxNoEntry",
  contains = "GComboBox",
  methods = list(
    initialize = function(toolkit = NULL, items, selected = 1, coerce.with = NULL,
                          handler, action, container, ...) {
      vals <- items
      if (is.data.frame(vals) || is.matrix(vals))
        vals <- vals[, 1]
      items <<- as.character(vals)
      widget <<- gtkComboBoxTextNew()
      for (it in .self$items)
        gtkComboBoxTextAppendText(widget, it)
      if (selected > 0)
        set_index(selected)
      initFields(block = widget, coerce_with = coerce.with, change_signal = "changed")
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = TRUE, ...) {
      idx <- get_index()
      if (length(idx) < 1L || is.na(idx) || idx < 1L) "" else items[idx]
    },
    set_value = function(value, drop = TRUE, ...) {
      idx <- match(as.character(value)[1], items)
      if (!is.na(idx)) set_index(idx)
    },
    get_items = function(i, ...) {
      if (missing(i)) items else items[i]
    },
    set_items = function(value, i, ...) {
      block_observers()
      on.exit(unblock_observers())
      vals <- value
      if (is.data.frame(vals) || is.matrix(vals))
        vals <- vals[, 1]
      items <<- as.character(vals)
      gtkComboBoxTextRemoveAll(widget)
      for (it in items)
        gtkComboBoxTextAppendText(widget, it)
      ## Match RGtk2: clear selection after replace (callers restore via set_value)
      set_index(0L)
    }
  )
)

GComboBoxWithEntry <- setRefClass(
  "GComboBoxWithEntry",
  contains = "GComboBox",
  methods = list(
    initialize = function(toolkit = NULL, items, selected = 1, coerce.with = NULL,
                          handler, action, container, ...) {
      vals <- items
      if (is.data.frame(vals) || is.matrix(vals))
        vals <- vals[, 1]
      items <<- as.character(vals)
      widget <<- gtkComboBoxTextNewWithEntry()
      for (it in .self$items)
        gtkComboBoxTextAppendText(widget, it)
      if (selected > 0)
        set_index(selected)
      initFields(block = widget, coerce_with = coerce.with, change_signal = "changed")
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = TRUE, ...) {
      as.character(gtkComboBoxTextGetActiveText(widget))
    },
    set_value = function(value, drop = TRUE, ...) {
      idx <- match(as.character(value)[1], items)
      if (!is.na(idx))
        set_index(idx)
      else {
        child <- gtkComboBoxGetChild(widget)
        if (!is.null(child))
          gtkEditableSetText(child, as.character(value)[1])
      }
    },
    get_items = function(i, ...) {
      if (missing(i)) items else items[i]
    },
    set_items = function(value, i, ...) {
      block_observers()
      on.exit(unblock_observers())
      vals <- value
      if (is.data.frame(vals) || is.matrix(vals))
        vals <- vals[, 1]
      items <<- as.character(vals)
      gtkComboBoxTextRemoveAll(widget)
      for (it in items)
        gtkComboBoxTextAppendText(widget, it)
      set_index(0L)
    }
  )
)
