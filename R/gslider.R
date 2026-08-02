##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gslider guiWidgetsToolkitRgtk4
.gslider.guiWidgetsToolkitRgtk4 <- function(toolkit, from = 0, to = 100, by = 1, value = from,
                                            horizontal = TRUE, handler = NULL, action = NULL,
                                            container = NULL, ...) {
  GSlider$new(toolkit, from, to, by, value, horizontal, handler, action, container, ...)
}

GSlider <- setRefClass(
  "GSlider",
  contains = "GWidget",
  fields = list(items = "ANY"),
  methods = list(
    initialize = function(toolkit, from, to, by, value, horizontal,
                          handler, action, container, ...) {
      if (length(from) == 1)
        x <- seq(from, to, by)
      else
        x <- from
      items <<- sort(unique(x))
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      widget <<- gtkScaleNewWithRange(orient, 1, length(items), 1)
      set_value(value[1])
      initFields(
        block = widget,
        default_expand = TRUE,
        default_fill = ifelse(horizontal, "x", "y"),
        change_signal = "value-changed"
      )
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = TRUE, ...) items[get_index()],
    set_value = function(value, drop = TRUE, ...) {
      idx <- which.min(abs(items - as.numeric(value)[1]))
      set_index(idx)
    },
    get_index = function(...) as.integer(round(gtkRangeGetValue(widget))),
    set_index = function(value, ...) {
      gtkRangeSetValue(widget, as.numeric(value)[1])
    }
  )
)
