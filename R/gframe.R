##' @include ggroup.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gframe guiWidgetsToolkitRgtk4
.gframe.guiWidgetsToolkitRgtk4 <- function(toolkit, text, markup, pos, horizontal = TRUE,
                                           spacing = 5, container = NULL,
                                           padding = NULL, margin = NULL, border = NULL,
                                           ...) {
  GFrame$new(toolkit, text, markup, pos, horizontal, spacing, container,
             padding = padding, margin = margin, border = border, ...)
}

GFrame <- setRefClass(
  "GFrame",
  contains = "GGroupBase",
  fields = list(markup = "logical", spacing = "numeric", label_widget = "ANY"),
  methods = list(
    initialize = function(toolkit = NULL, text = "", markup = FALSE, pos = 0,
                          horizontal = TRUE, spacing = 5, container = NULL,
                          padding = NULL, margin = NULL, border = NULL, ...) {
      horizontal <<- horizontal
      spacing <<- spacing
      make_widget(text, markup, pos)
      init_box_model(padding = padding, margin = margin, border = border)
      add_to_parent(container, .self, ...)
      callSuper(toolkit, horizontal = horizontal, ...)
    },
    make_widget = function(text, markup, pos) {
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      widget <<- gtkBoxNew(orient, as.integer(spacing))
      markup <<- isTRUE(markup)
      block <<- gtkFrameNew(NULL)
      label_widget <<- gtkLabelNew("")
      gtkFrameSetLabelWidget(block, label_widget)
      gtkFrameSetLabelAlign(block, as.numeric(pos)[1])
      set_names(text)
      gtkFrameSetChild(block, widget)
    },
    get_names = function(...) as.character(gtkLabelGetText(label_widget)),
    set_names = function(value, ...) {
      value <- as.character(value)[1]
      if (markup)
        gtkLabelSetMarkup(label_widget, value)
      else
        gtkLabelSetText(label_widget, value)
    }
  )
)
