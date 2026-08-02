##' @include gnotebook.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gstackwidget guiWidgetsToolkitRgtk4
.gstackwidget.guiWidgetsToolkitRgtk4 <- function(toolkit, container = NULL, ...) {
  GStackWidget$new(toolkit, container = container, ...)
}

GStackWidget <- setRefClass(
  "GStackWidget",
  contains = "GContainer",
  fields = list(page_names = "character"),
  methods = list(
    initialize = function(toolkit = NULL, container = NULL, ...) {
      widget <<- gtkStackNew()
      initFields(block = widget, page_names = character(0))
      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },
    get_value = function(...) {
      name <- gtkStackGetVisibleChildName(widget)
      if (is.null(name) || !nzchar(as.character(name)))
        return(1L)
      match(as.character(name), page_names)
    },
    set_value = function(value, ...) {
      idx <- as.integer(value)[1]
      if (idx >= 1 && idx <= length(page_names))
        gtkStackSetVisibleChildName(widget, page_names[idx])
    },
    get_index = function(...) get_value(),
    set_index = function(value, ...) set_value(value),
    get_names = function(...) page_names,
    set_names = function(...) {},
    get_items = function(i, j, ..., drop = TRUE) {
      items <- children[i]
      if (drop && length(items) == 1) items[[1]] else items
    },
    get_length = function(...) length(children),
    add_child = function(child, index = NULL, ...) {
      child_block <- getBlock(child)
      name <- sprintf("page-%d", length(children) + 1L)
      gtkStackAddNamed(widget, child_block, name)
      page_names <<- c(page_names, name)
      gtkStackSetVisibleChildName(widget, name)
      child_bookkeeping(child)
    }
  )
)
