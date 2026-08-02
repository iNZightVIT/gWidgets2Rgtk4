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
    add_handler_changed = function(handler, action = NULL, ...) {
      ## no click signal on image alone in Phase 1
      if (is_handler(handler))
        invisible(NULL)
    }
  )
)
