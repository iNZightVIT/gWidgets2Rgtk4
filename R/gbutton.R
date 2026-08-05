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
##'
##' GTK4 has no GtkButton image+label pairing like GTK2 `setImage`. We keep the
##' label text in a field and rebuild the child as icon, label, or a box with
##' both. `gtk_button_set_icon_name` alone would wipe the label.
##'
##' @rdname gWidgets2Rgtk4-package
GButton <- setRefClass(
  "GButton",
  contains = "GWidget",
  fields = list(
    button_label = "character",
    ## list(kind = "file"|"theme", src = path or icon-name), or NULL
    button_icon = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL, text = NULL, handler, action, container, ...) {
      if (is(text, "GAction")) {
        action <- text
        text <- action$get_value()
      }
      lab <- if (!is_empty(text)) as.character(text)[1] else ""
      if (is.na(lab)) lab <- ""
      initFields(button_label = lab, button_icon = NULL, change_signal = "clicked")
      if (nzchar(lab))
        widget <<- gtkButtonNewWithLabel(lab)
      else
        widget <<- gtkButtonNew()
      block <<- widget
      toolkit <<- toolkit
      ## If text is a stock/gw icon id, add the icon beside the label
      if (nzchar(lab))
        set_icon(lab)
      add_to_parent(container, .self, ...)
      if (is(action, "GAction")) {
        lab2 <- action$get_value()
        if (!is_empty(lab2)) {
          button_label <<- as.character(lab2)[1]
          ic <- action$get_icon()
          if (!is.null(ic) && !is_empty(ic))
            set_icon(ic)
          else
            set_icon(lab2)
          rebuild_button_face()
        }
        tip <- tryCatch(action$get_tooltip(), error = function(e) NULL)
        if (!is.null(tip) && nzchar(as.character(tip)[1]))
          set_tooltip(tip)
        gtkWidgetSetSensitive(widget, action$get_enabled())
        action$add_proxy(widget)
        gSignalConnectR(widget, "clicked", function(...) {
          action$activate()
        })
        if (is_handler(handler))
          handler_id <<- add_handler_changed(handler, NULL)
      } else {
        handler_id <<- add_handler_changed(handler, action)
      }
      callSuper(toolkit)
    },
    set_value = function(value, index = TRUE, drop = TRUE, ...) {
      new_lab <- if (is_empty(value)) "" else as.character(value)[1]
      if (is.na(new_lab)) new_lab <- ""
      if (identical(new_lab, button_label))
        return()
      button_label <<- new_lab
      ## Stock-named labels (e.g. "ok") also pick up a matching icon
      set_icon(new_lab)
      rebuild_button_face()
      invoke_change_handler()
    },
    get_value = function(index = TRUE, drop = TRUE, ...) {
      button_label
    },
    ## Resolve value to file path or theme icon; NULL if not an icon id
    resolve_icon = function(value) {
      resolve_icon_spec(value)
    },
    set_icon = function(value) {
      value <- as.character(value)[1]
      if (is.na(value) || !nzchar(value)) {
        button_icon <<- NULL
        rebuild_button_face()
        return(invisible(NULL))
      }
      spec <- resolve_icon(value)
      if (is.null(spec)) {
        ## Plain label text (initialize set_icon("Import data")) — no-op
        return(invisible(NULL))
      }
      button_icon <<- spec
      rebuild_button_face()
      invisible(NULL)
    },
    rebuild_button_face = function() {
      lab <- button_label
      if (is.null(lab) || is.na(lab)) lab <- ""
      spec <- button_icon
      has_lab <- nzchar(lab)
      has_icon <- is.list(spec) && !is.null(spec$src) && nzchar(spec$src)

      make_image <- function() {
        make_gtk_image(spec$src, kind = spec$kind, pixel_size = 16L)
      }

      if (has_icon && has_lab) {
        box <- gtkBoxNew(.GtkOrientation$HORIZONTAL, 4L)
        gtkBoxAppend(box, make_image())
        gtkBoxAppend(box, gtkLabelNew(lab))
        gtkButtonSetChild(widget, box)
      } else if (has_icon) {
        gtkButtonSetChild(widget, make_image())
      } else if (has_lab) {
        gtkButtonSetLabel(widget, lab)
      } else {
        gtkButtonSetLabel(widget, "")
      }
      invisible(NULL)
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      add_handler_changed(handler, action, ...)
    },
    remove_border = function() {
      ## GTK4: no relief style; no-op
      invisible(NULL)
    },
    ## Style the button so CSS also hits the child label
    style_widget = function() widget
  )
)
