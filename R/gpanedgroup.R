##' @include GContainer.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gpanedgroup guiWidgetsToolkitRgtk4
.gpanedgroup.guiWidgetsToolkitRgtk4 <- function(toolkit, horizontal = TRUE,
                                                container = NULL, ...) {
  GPanedGroup$new(toolkit, horizontal = horizontal, container = container, ...)
}

GPanedGroup <- setRefClass(
  "GPanedGroup",
  contains = "GContainer",
  fields = list(horizontal = "logical"),
  methods = list(
    initialize = function(toolkit = NULL, horizontal = TRUE, container = NULL, ...) {
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      widget <<- gtkPanedNew(orient)
      initFields(block = widget, horizontal = horizontal)
      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },
    get_value = function(...) {
      as.integer(gtkPanedGetPosition(widget))
    },
    set_value = function(value, ...) {
      gtkPanedSetPosition(widget, as.integer(value)[1])
    },
    get_items = function(i, j, ..., drop = TRUE) {
      children[[i]]
    },
    get_length = function() length(children),
    add_child = function(child, expand = NULL, fill = NULL, anchor = NULL,
                         resize = TRUE, shrink = TRUE, ...) {
      n <- get_length()
      if (n >= 2) {
        message("Already have two children. Remove one?")
        return()
      }
      child_block <- getBlock(child)
      if (n == 0) {
        gtkPanedSetStartChild(widget, child_block)
        gtkPanedSetResizeStartChild(widget, as.logical(resize))
        gtkPanedSetShrinkStartChild(widget, as.logical(shrink))
      } else {
        gtkPanedSetEndChild(widget, child_block)
        gtkPanedSetResizeEndChild(widget, as.logical(resize))
        gtkPanedSetShrinkEndChild(widget, as.logical(shrink))
      }
      child_bookkeeping(child)
    }
  )
)
