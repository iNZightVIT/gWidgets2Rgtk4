##' @include GWidget.R
NULL

##' toolkit constructor
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gbutton guiWidgetsToolkitRgtk4
.gbutton.guiWidgetsToolkitRgtk4 <- function(toolkit, text, handler, action, container, ...) {
  GButton$new(toolkit, text, handler, action, container, ...)
}

##' Button class for Rgtk4
##' @rdname gWidgets2Rgtk4-package
GButton <- setRefClass(
  "GButton",
  contains = "GWidget",
  methods = list(
    initialize = function(toolkit = NULL, text = NULL, handler, action, container, ...) {
      if (!is_empty(text))
        widget <<- gtkButtonNewWithLabel(as.character(text)[1])
      else
        widget <<- gtkButtonNew()
      block <<- widget
      toolkit <<- toolkit
      initFields(change_signal = "clicked")
      if (!is_empty(text))
        set_icon(text)
      add_to_parent(container, .self, ...)
      if (is(action, "GAction")) {
        tip <- tryCatch(action$get_tooltip(), error = function(e) NULL)
        if (!is.null(tip) && nzchar(tip))
          set_tooltip(tip)
        handler_id <<- add_handler_changed(handler, action)
      } else {
        handler_id <<- add_handler_changed(handler, action)
      }
      callSuper(toolkit)
    },
    set_value = function(value, index = TRUE, drop = TRUE, ...) {
      old_value <- get_value()
      if (!is_empty(old_value) && !is_empty(value) && identical(as.character(value)[1], old_value))
        return()
      gtkButtonSetLabel(widget, as.character(value)[1])
      set_icon(value)
      invoke_change_handler()
    },
    get_value = function(index = TRUE, drop = TRUE, ...) {
      lab <- gtkButtonGetLabel(widget)
      if (is.null(lab)) "" else as.character(lab)
    },
    set_icon = function(value) {
      value <- as.character(value)[1]
      ## Only treat known stock ids as icons; plain labels stay labels
      nms <- names(.stock_to_icon_name)
      bare <- sub("^(gtk|gw)-", "", value)
      if (!(value %in% nms || bare %in% nms || grepl("^(gtk-|gw-)", value)))
        return(invisible(NULL))
      icon <- stock_to_icon_name(value)
      if (!is.null(icon) && nzchar(icon) && !file.exists(icon))
        gtkButtonSetIconName(widget, icon)
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      add_handler_changed(handler, action, ...)
    },
    remove_border = function() {
      ## GTK4: no relief style; no-op
      invisible(NULL)
    }
  )
)
