##' @include GWidget.R
NULL

##' Toolkit gcheckbox constructor
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gcheckbox guiWidgetsToolkitRgtk4
.gcheckbox.guiWidgetsToolkitRgtk4 <- function(toolkit, text, checked = FALSE,
                                              use.togglebutton = FALSE, handler = NULL,
                                              action = NULL, container = NULL, ...) {
  if (use.togglebutton)
    GToggleButton$new(toolkit, text, checked, handler, action, container, ...)
  else
    GCheckbox$new(toolkit, text, checked, handler, action, container, ...)
}

GCheckbox <- setRefClass(
  "GCheckbox",
  contains = "GWidget",
  methods = list(
    initialize = function(toolkit = NULL, text = "", checked = FALSE, handler = NULL,
                          action = NULL, container = NULL, ...) {
      if (is(widget, "uninitializedField")) {
        widget <<- gtkCheckButtonNewWithLabel(as.character(text)[1])
        gtkCheckButtonSetActive(widget, as.logical(checked))
        initFields(block = widget, change_signal = "toggled")
        add_to_parent(container, .self, ...)
        handler_id <<- add_handler_changed(handler, action)
      }
      callSuper(toolkit)
    },
    set_value = function(value, index = TRUE, drop = TRUE, ...) {
      gtkCheckButtonSetActive(widget, as.logical(value))
    },
    get_value = function(index = TRUE, drop = TRUE, ...) {
      as.logical(gtkCheckButtonGetActive(widget))
    },
    get_items = function(i, j, ..., drop = TRUE) {
      as.character(gtkCheckButtonGetLabel(widget))
    },
    set_items = function(value, i, j, ...) {
      gtkCheckButtonSetLabel(widget, as.character(value)[1])
    },
    ## CSS rule styles .class label descendants
    style_widget = function() widget
  )
)

GToggleButton <- setRefClass(
  "GToggleButton",
  contains = "GCheckbox",
  methods = list(
    initialize = function(toolkit = NULL, text, checked = FALSE, handler = NULL,
                          action = NULL, container = NULL, ...) {
      widget <<- gtkToggleButtonNewWithLabel(as.character(text)[1])
      gtkToggleButtonSetActive(widget, as.logical(checked))
      initFields(block = widget, change_signal = "toggled")
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_items = function(i, j, ..., drop = TRUE) {
      as.character(gtkButtonGetLabel(widget))
    },
    set_items = function(value, i, j, ...) {
      gtkButtonSetLabel(widget, as.character(value)[1])
    },
    set_value = function(value, index = TRUE, drop = TRUE, ...) {
      gtkToggleButtonSetActive(widget, as.logical(value))
    },
    get_value = function(index = TRUE, drop = TRUE, ...) {
      as.logical(gtkToggleButtonGetActive(widget))
    }
  )
)
