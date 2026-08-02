##' @include GWidget.R
NULL

##' Toolkit label constructor
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .glabel guiWidgetsToolkitRgtk4
.glabel.guiWidgetsToolkitRgtk4 <- function(toolkit, text = "", markup = FALSE, editable = FALSE,
                                           handler = NULL, action = NULL, container = NULL, ...) {
  GLabel$new(toolkit, text, markup, editable, handler, action, container, ...)
}

##' Label class for Rgtk4
##' @rdname gWidgets2Rgtk4-package
GLabel <- setRefClass(
  "GLabel",
  contains = "GWidget",
  fields = list(
    markup = "ANY",
    editable = "logical",
    edit_widget = "ANY",
    state = "character",
    box = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL, text, markup = FALSE, editable = FALSE,
                          handler, action, container, ...) {
      widget <<- gtkLabelNew("")
      gtkWidgetSetCanFocus(widget, FALSE)
      if (markup)
        gtkLabelSetUseMarkup(widget, TRUE)

      ## GTK4: no EventBox — pack in a box; widget == block unless editable
      box <<- gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L)
      gtkBoxAppend(box, widget)
      block <<- box

      initFields(
        markup = markup,
        editable = editable,
        change_signal = "notify::label"
      )
      add_to_parent(container, .self, ...)
      set_value(text)

      if (editable) {
        state <<- "label"
        edit_widget <<- gtkEntryNew()
        gtkWidgetSetVisible(edit_widget, FALSE)
        gtkBoxAppend(box, edit_widget)
        gSignalConnectR(edit_widget, "activate", function(...) {
          show_label_widget()
        })
        ## clickable via gesture would be ideal; Phase 1: double-use via notify
      }

      if (is_handler(handler))
        handler_id <<- add_handler_changed(handler, action)

      callSuper(toolkit)
    },
    set_value = function(value, index = TRUE, drop = TRUE, ...) {
      value <- paste(value, collapse = "\n")
      if (isTRUE(markup))
        gtkLabelSetMarkup(widget, value)
      else
        gtkLabelSetText(widget, value)
      invoke_change_handler()
    },
    get_value = function(index = TRUE, drop = TRUE, ...) {
      value <- as.character(gtkLabelGetText(widget))
      if (isTRUE(markup))
        value <- gsub("<[^>]*>", "", value)
      value
    },
    show_edit_widget = function() {
      gtkEditableSetText(edit_widget, get_value())
      gtkWidgetSetVisible(widget, FALSE)
      gtkWidgetSetVisible(edit_widget, TRUE)
      gtkWidgetGrabFocus(edit_widget)
      state <<- "edit"
    },
    show_label_widget = function() {
      set_value(gtkEditableGetText(edit_widget))
      gtkWidgetSetVisible(edit_widget, FALSE)
      gtkWidgetSetVisible(widget, TRUE)
      state <<- "label"
    },
    set_angle = function(angle) {
      ## GTK4 Label angle removed in some versions; no-op
      invisible(NULL)
    }
  )
)
