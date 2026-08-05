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
      ## Discrete index scale: snap thumb to integers (GTK default round_digits=-1
      ## leaves continuous fractions between steps — feels "too smooth" for factors).
      gtkRangeSetRoundDigits(widget, 0L)
      gtkRangeSetIncrements(widget, 1, 1)
      ## Show item labels (RGtk2 used format-value); value is the 1..n index.
      gtkScaleSetFormatValueFunc(widget, function(scale, value) {
        i <- as.integer(round(value))
        if (i < 1L || i > length(items)) return("")
        format(items[i], digits = 3)
      })
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
      ## Numeric items: nearest value. Character/factor items: label match
      ## (RGtk2 used pmatch; arithmetic fails on factors with Ops.factor).
      if (is.numeric(items)) {
        idx <- which.min(abs(items - as.numeric(value)[1]))
      } else {
        idx <- match(as.character(value)[1], as.character(items))
        if (is.na(idx)) return()
      }
      set_index(idx)
    },
    get_index = function(...) as.integer(round(gtkRangeGetValue(widget))),
    set_index = function(value, ...) {
      if (length(value) < 1L || is.na(value[1])) return()
      gtkRangeSetValue(widget, as.numeric(value)[1])
    }
  )
)
