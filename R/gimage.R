##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gimage guiWidgetsToolkitRgtk4
.gimage.guiWidgetsToolkitRgtk4 <- function(toolkit, filename = "", dirname = "", stock.id = NULL,
                                           size = "", handler = NULL, action = NULL,
                                           container = NULL, ...) {
  GImage$new(toolkit, filename = filename, dirname = dirname, stock.id = stock.id,
             size = size, handler = handler, action = action, container = container, ...)
}

GImage <- setRefClass(
  "GImage",
  contains = "GWidget",
  fields = list(image_name = "ANY"),
  methods = list(
    initialize = function(toolkit = NULL, filename = "", dirname = "", stock.id = NULL,
                          size = "", handler = NULL, action = NULL, container = NULL, ...) {
      if (!is.null(stock.id)) {
        image_name <<- stock.id
      } else {
        if (nzchar(dirname))
          filename <- file.path(dirname, filename)
        image_name <<- filename
      }
      widget <<- gtkImageNew()
      block <<- widget
      ## GTK4: no EventBox — clicks via GtkGestureClick (see ensure_click_controller)
      try(gtkWidgetSetCanTarget(widget, TRUE), silent = TRUE)
      initFields(change_signal = "clicked")
      add_to_parent(container, .self, ...)
      set_value(image_name)
      if (is_handler(handler))
        handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(...) image_name,
    set_value = function(value, ...) {
      image_name <<- value
      if (file.exists(value)) {
        gtkImageSetFromFile(widget, value)
      } else {
        icon <- stock_to_icon_name(value)
        if (!is.null(icon))
          gtkImageSetFromIconName(widget, icon)
      }
    },
    ## Observer signal "clicked" is logical; GtkImage has no clicked GObject signal.
    ensure_click_controller = function() {
      if (!is.null(get_attr(".click_gesture")))
        return(invisible(NULL))
      g <- gtkGestureClickNew()
      gtkGestureSingleSetButton(g, 1L)
      try(gtkEventControllerSetPropagationPhase(g, 2L), silent = TRUE)
      gSignalConnectR(g, "released", function(gest, n_press, x, y) {
        .self$notify_observers(signal = "clicked")
        FALSE
      })
      gtkWidgetAddController(widget, g)
      set_attr(".click_gesture", g)
      invisible(NULL)
    },
    connect_to_toolkit_signal = function(signal, decorator,
                                         emitter = .self$handler_widget()) {
      if (identical(signal, "clicked")) {
        ensure_click_controller()
        return(invisible(NULL))
      }
      if (missing(decorator))
        callSuper(signal, emitter = emitter)
      else
        callSuper(signal, decorator, emitter = emitter)
    },
    add_handler_changed = function(handler, action = NULL, ...) {
      add_handler_clicked(handler, action, ...)
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      add_handler("clicked", handler, action, ...)
    }
  )
)
