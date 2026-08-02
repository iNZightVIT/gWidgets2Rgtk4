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
    get_index = function(...) as.integer(gtkComboBoxGetActive(widget)) + 1L,
    set_index = function(value, ...) {
      value <- as.integer(value)[1]
      n <- get_length()
      value <- min(max(0L, value), n)
      gtkComboBoxSetActive(widget, value - 1L)
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
      if (is.na(idx) || idx < 1) "" else items[idx]
    },
    set_value = function(value, drop = TRUE, ...) {
      idx <- match(as.character(value)[1], items)
      if (!is.na(idx)) set_index(idx)
    },
    get_items = function(i, ...) {
      if (missing(i)) items else items[i]
    },
    set_items = function(value, i, ...) {
      vals <- value
      if (is.data.frame(vals) || is.matrix(vals))
        vals <- vals[, 1]
      items <<- as.character(vals)
      gtkComboBoxTextRemoveAll(widget)
      for (it in items)
        gtkComboBoxTextAppendText(widget, it)
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
      vals <- value
      if (is.data.frame(vals) || is.matrix(vals))
        vals <- vals[, 1]
      items <<- as.character(vals)
      gtkComboBoxTextRemoveAll(widget)
      for (it in items)
        gtkComboBoxTextAppendText(widget, it)
    }
  )
)
