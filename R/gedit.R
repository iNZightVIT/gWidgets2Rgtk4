##' @include GWidget.R
NULL

##' Toolkit gedit constructor
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gedit guiWidgetsToolkitRgtk4
.gedit.guiWidgetsToolkitRgtk4 <- function(toolkit, text = "", width = 25, coerce.with = NULL,
                                          initial.msg = "", handler = NULL, action = NULL,
                                          container = NULL, ...) {
  GEdit$new(toolkit, text = text, width = width, coerce.with = coerce.with,
            initial.msg = initial.msg, handler = handler, action = action,
            container = container, ...)
}

GEdit <- setRefClass(
  "GEdit",
  contains = "GWidget",
  fields = list(
    init_msg = "character",
    init_msg_flag = "logical",
    completion = "ANY",
    validator = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL, text = "", width = 25, coerce.with = NULL,
                          initial.msg = "", handler = NULL, action = NULL,
                          container = NULL, ...) {
      widget <<- gtkEntryNew()
      gtkEditableSetWidthChars(widget, as.integer(width))
      initFields(
        block = widget,
        coerce_with = coerce.with,
        init_msg = as.character(initial.msg),
        init_msg_flag = FALSE,
        completion = NULL,
        validator = NULL,
        change_signal = "activate"
      )
      if (nzchar(init_msg))
        gtkEntrySetPlaceholderText(widget, init_msg)
      if (nzchar(text))
        set_value(text)
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    set_value = function(value, index = TRUE, drop = TRUE, ...) {
      gtkEditableSetText(widget, as.character(value)[1])
      ## emit activate to match RGtk2 behaviour
      invoke_change_handler()
    },
    get_value = function(index = TRUE, drop = TRUE, ...) {
      as.character(gtkEditableGetText(widget))
    },
    get_items = function(i, j, ..., drop = TRUE) character(0),
    set_items = function(value, i, j, ...) {
      ## completion deferred / simplified
      invisible(NULL)
    },
    set_error = function(msg) set_invalid(TRUE, msg),
    clear_error = function() set_invalid(FALSE, ""),
    validate_value = function() TRUE
  )
)
