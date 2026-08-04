##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gcheckboxgroup guiWidgetsToolkitRgtk4
.gcheckboxgroup.guiWidgetsToolkitRgtk4 <- function(toolkit = NULL, items, checked = FALSE,
                                                   horizontal = FALSE, use.table = FALSE,
                                                   handler = NULL, action = NULL,
                                                   container = NULL, ...) {
  GCheckboxGroup$new(toolkit, items, checked = checked, horizontal = horizontal,
                     handler = handler, action = action, container = container, ...)
}

GCheckboxGroup <- setRefClass(
  "GCheckboxGroup",
  contains = "GWidgetWithItems",
  methods = list(
    initialize = function(toolkit, items, checked = FALSE, horizontal = FALSE,
                          handler = NULL, action = NULL, container = NULL, ...) {
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      block <<- gtkBoxNew(orient, 1L)
      widget <<- block
      initFields(widgets = list(), change_signal = "toggled")
      set_items(value = items)
      set_index(checked)
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = TRUE, ...) {
      items <- get_items()
      items[get_index()]
    },
    set_value = function(value, drop = TRUE, ...) {
      items <- get_items()
      if (is.logical(value) && !is.logical(items)) {
        set_index(value)
      } else {
        set_index(pmatch(value, items))
      }
    },
    get_index = function(...) {
      which(vapply(widgets, function(i) as.logical(gtkCheckButtonGetActive(i)), logical(1)))
    },
    set_index = function(value, ...) {
      block_observers()
      n <- get_length()
      if (is.logical(value))
        value <- rep(value, length.out = n)
      if (is.numeric(value))
        value <- seq_len(n) %in% value
      for (i in seq_len(n))
        gtkCheckButtonSetActive(widgets[[i]], as.logical(value[i]))
      unblock_observers()
      notify_observers(signal = "toggled")
    },
    get_items = function(i, ...) {
      items <- vapply(widgets, function(w) as.character(gtkCheckButtonGetLabel(w)), character(1))
      if (missing(i)) items else items[i]
    },
    set_items = function(value, i, ...) {
      while (!is.null(ch <- tryCatch(gtkWidgetGetFirstChild(block), error = function(e) NULL)))
        gtkBoxRemove(block, ch)
      widgets <<- lapply(as.character(value), gtkCheckButtonNewWithLabel)
      for (w in widgets) {
        gtkBoxAppend(block, w)
        gSignalConnectR(w, "toggled", function(...) {
          .self$notify_observers(signal = "toggled")
        })
      }
    },
    get_length = function(...) length(widgets),
    ## Style all checkbox labels via descendant selector on the box
    style_widget = function() block
  )
)
