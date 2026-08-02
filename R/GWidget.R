##' @include GComponent.R
NULL

##' Base class for widget objects
##' @rdname gWidgets2Rgtk4-package
GWidget <- setRefClass(
  "GWidget",
  contains = "GComponentObservable",
  methods = list(
    initialize = function(..., coerce.with = NULL) {
      if (is.null(coerce_with) && !is.null(coerce.with))
        coerce_with <<- coerce.with
      callSuper(...)
    }
  )
)

##' Base class for selection widgets based on a set of items
##' @rdname gWidgets2Rgtk4-package
GWidgetWithItems <- setRefClass(
  "GWidgetWithItems",
  contains = "GWidget",
  fields = list(widgets = "list"),
  methods = list(
    connect_to_toolkit_signal = function(signal, f, emitter) {
      ## override — done when adding items
    },
    get_enabled = function() {
      if (length(widgets))
        gtkWidgetGetSensitive(widgets[[1]])
      else
        TRUE
    },
    set_enabled = function(value) {
      lapply(widgets, function(w) gtkWidgetSetSensitive(w, as.logical(value)))
    }
  )
)

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method getWidget GWidgetWithItems
getWidget.GWidgetWithItems <- function(obj) getWidget(obj$block)
