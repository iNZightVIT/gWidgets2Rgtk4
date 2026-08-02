##' @include GComponent.R
NULL

##' Base class for container objects
##' @rdname gWidgets2Rgtk4-package
GContainer <- setRefClass(
  "GContainer",
  contains = "GComponentObservable",
  fields = list(children = "list"),
  methods = list(
    add_child = function(child, expand, fill, anchor, ...) {
      "Add child to parent, do internal book keeping"
    },
    child_bookkeeping = function(child) {
      if (is(child, "GComponent"))
        child$set_parent(.self)
      children <<- c(children, child)
    },
    set_child_align = function(child, alt_child, anchor) {
      ## GTK4: align via widget properties
      if (is.null(anchor))
        return()
      target <- child
      if (!inherits(target, "RGtkObject"))
        target <- alt_child
      if (inherits(target, "RGtkObject")) {
        ax <- as.numeric(anchor[1])
        ay <- as.numeric(anchor[2])
        halign <- if (ax < 0.33) .GtkAlign$START else if (ax > 0.67) .GtkAlign$END else .GtkAlign$CENTER
        valign <- if (ay < 0.33) .GtkAlign$START else if (ay > 0.67) .GtkAlign$END else .GtkAlign$CENTER
        gtkWidgetSetHalign(target, halign)
        gtkWidgetSetValign(target, valign)
      }
    },
    set_child_fill = function(child, fill, horizontal = TRUE) {
      fill_to_logical(fill, horizontal)
    }
  )
)
