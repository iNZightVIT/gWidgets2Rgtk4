##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gformlayout guiWidgetsToolkitRgtk4
.gformlayout.guiWidgetsToolkitRgtk4 <- function(toolkit, align = "left", spacing = 5,
                                                container = NULL, ...) {
  GFormLayout$new(toolkit, align, spacing, container = container, ...)
}

GFormLayout <- setRefClass(
  "GFormLayout",
  contains = "GContainer",
  fields = list(align = "character", spacing = "numeric", nrows = "integer"),
  methods = list(
    initialize = function(toolkit = NULL, align = "left", spacing = 5,
                          container = NULL, ...) {
      initFields(align = align, spacing = as.numeric(spacing), nrows = 0L)
      widget <<- gtkGridNew()
      gtkGridSetColumnSpacing(widget, as.integer(spacing))
      gtkGridSetRowSpacing(widget, as.integer(spacing))
      block <<- widget
      add_to_parent(container, .self)
      callSuper(toolkit, ...)
    },
    add_child = function(child, expand = NULL, fill = NULL, anchor = NULL, ..., label = "") {
      add_row(label, child, expand, fill, anchor, ...)
    },
    add_row = function(label, child, expand = NULL, fill = NULL, anchor = NULL, ...) {
      row <- nrows
      lab <- gtkLabelNew(as.character(label)[1])
      halign <- switch(align, left = .GtkAlign$START, right = .GtkAlign$END, .GtkAlign$END)
      gtkWidgetSetHalign(lab, halign)
      child_widget <- getBlock(child)
      gtkWidgetSetHexpand(child_widget, TRUE)
      gtkGridAttach(widget, lab, 0L, as.integer(row), 1L, 1L)
      gtkGridAttach(widget, child_widget, 1L, as.integer(row), 1L, 1L)
      nrows <<- as.integer(row + 1L)
      child_bookkeeping(child)
    },
    get_length = function(...) nrows
  )
)
