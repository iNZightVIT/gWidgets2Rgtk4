##' @include gframe.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gexpandgroup guiWidgetsToolkitRgtk4
.gexpandgroup.guiWidgetsToolkitRgtk4 <- function(toolkit, text, markup, horizontal = TRUE,
                                                 handler = NULL, action = NULL,
                                                 container = NULL, ...) {
  GExpandGroup$new(toolkit, text = text, markup = markup, horizontal = horizontal,
                   handler = handler, action = action, container = container, ...)
}

GExpandGroup <- setRefClass(
  "GExpandGroup",
  contains = "GGroupBase",
  fields = list(markup = "logical"),
  methods = list(
    initialize = function(toolkit = NULL, text, markup = FALSE, horizontal = TRUE,
                          handler, action, container = NULL, ..., expand = FALSE, fill = FALSE) {
      horizontal <<- horizontal
      if (is(widget, "uninitializedField"))
        make_widget(text, markup)
      handler_id <<- add_handler_changed(handler, action)
      add_to_parent(container, .self, expand, fill, ...)
      callSuper(toolkit, horizontal = horizontal, ...)
    },
    make_widget = function(text, markup) {
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      widget <<- gtkBoxNew(orient, 5L)
      markup <<- isTRUE(markup)
      block <<- gtkExpanderNew(as.character(text)[1])
      if (markup)
        gtkExpanderSetUseMarkup(block, TRUE)
      gtkExpanderSetChild(block, widget)
      initFields(change_signal = "notify::expanded")
    },
    get_names = function(...) as.character(gtkExpanderGetLabel(block)),
    set_names = function(value, ...) gtkExpanderSetLabel(block, as.character(value)[1]),
    get_visible = function() as.logical(gtkExpanderGetExpanded(block)),
    set_visible = function(value) gtkExpanderSetExpanded(block, as.logical(value)),
    ## Expander chrome (label) lives on block, not the inner box
    style_widget = function() block,
    add_handler_changed = function(handler, action = NULL, ...) {
      add_handler("notify::expanded", handler, action, ...)
    }
  )
)
