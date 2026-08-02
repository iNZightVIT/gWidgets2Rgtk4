##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gspinbutton guiWidgetsToolkitRgtk4
.gspinbutton.guiWidgetsToolkitRgtk4 <- function(toolkit, from = 0, to = 10, by = 1, value = from,
                                                digits = 0, handler = NULL, action = NULL,
                                                container = NULL, ...) {
  GSpinButton$new(toolkit, from, to, by, value, digits, handler, action, container, ...)
}

GSpinButton <- setRefClass(
  "GSpinButton",
  contains = "GWidget",
  methods = list(
    initialize = function(toolkit, from = 0, to = 10, by = 1, value = from, digits = 0,
                          handler, action, container, ...) {
      if (digits == 0 && as.logical(by %% 1))
        digits <- abs(floor(log(by, 10)))
      widget <<- gtkSpinButtonNewWithRange(as.numeric(from), as.numeric(to), as.numeric(by))
      gtkSpinButtonSetDigits(widget, as.integer(digits))
      set_value(value)
      initFields(block = widget, change_signal = "value-changed")
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = TRUE, ...) as.numeric(gtkSpinButtonGetValue(widget)),
    set_value = function(value, drop = TRUE, ...) {
      gtkSpinButtonSetValue(widget, as.numeric(value)[1])
    }
  )
)
