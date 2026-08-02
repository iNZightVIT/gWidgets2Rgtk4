##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gseparator guiWidgetsToolkitRgtk4
.gseparator.guiWidgetsToolkitRgtk4 <- function(toolkit, horizontal = TRUE, container = NULL, ...) {
  GSeparator$new(toolkit, horizontal = horizontal, container = container, ...)
}

GSeparator <- setRefClass(
  "GSeparator",
  contains = "GWidget",
  methods = list(
    initialize = function(toolkit, horizontal = TRUE, container = NULL, ...) {
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      widget <<- gtkSeparatorNew(orient)
      initFields(block = widget)
      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    }
  )
)
