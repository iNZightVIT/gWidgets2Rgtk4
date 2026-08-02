##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gprogressbar guiWidgetsToolkitRgtk4
.gprogressbar.guiWidgetsToolkitRgtk4 <- function(toolkit, value, container, ...) {
  GProgressBar$new(toolkit, value, container, ...)
}

GProgressBar <- setRefClass(
  "GProgressBar",
  contains = "GWidget",
  methods = list(
    initialize = function(toolkit = NULL, value, container, ...) {
      widget <<- gtkProgressBarNew()
      if (!missing(value))
        set_value(value)
      initFields(block = widget)
      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },
    set_value = function(value, index = TRUE, drop = TRUE, ...) {
      if (is.null(value)) {
        gtkProgressBarPulse(widget)
      } else {
        frac <- (as.numeric(value) / 100) %% 1
        gtkProgressBarSetFraction(widget, frac)
      }
    },
    get_value = function(index = TRUE, drop = TRUE, ...) {
      as.integer(gtkProgressBarGetFraction(widget) * 100)
    }
  )
)
