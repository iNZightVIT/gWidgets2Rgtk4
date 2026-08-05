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
      ## Proportion of allocated size (gWidgets2 API); match RGtk2.
      pos <- as.integer(gtkPanedGetPosition(widget))[1]
      sz <- as.numeric(get_size())
      denom <- if (horizontal) sz[1] else sz[2]
      if (is.na(denom) || denom <= 0)
        return(NA_real_)
      unname(pos / denom)
    },
    set_value = function(value, ...) {
      ## Integer => pixels from start; otherwise proportion in [0, 1].
      ## Ambiguous 1 vs 1L is distinguished by class (see ?gpanedgroup).
      value <- value[1]
      if (is.integer(value)) {
        pos <- value
      } else {
        sz <- as.numeric(get_size())
        denom <- if (horizontal) sz[1] else sz[2]
        if (is.na(denom) || denom <= 0)
          return(invisible(NULL))
        pos <- as.integer(as.numeric(value) * denom)
      }
      gtkPanedSetPosition(widget, pos)
    },
    get_items = function(i, j, ..., drop = TRUE) {
      children[[i]]
    },
    get_length = function() length(children),
    add_child = function(child, expand = NULL, fill = NULL, anchor = NULL,
                         resize = TRUE, shrink = FALSE, ...) {
      ## GtkPaned uses resize/shrink, not box expand. Callers (e.g. iNZight)
      ## pass expand=FALSE expecting a fixed-width pane; map expand -> resize
      ## when resize was not given explicitly. shrink defaults FALSE like RGtk2.
      if (missing(resize) && !is.null(expand))
        resize <- as.logical(expand)[1]
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
