##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gstatusbar guiWidgetsToolkitRgtk4
.gstatusbar.guiWidgetsToolkitRgtk4 <- function(toolkit, text = "", container = NULL, ...) {
  GStatusBar$new(toolkit, text = text, container = container, ...)
}

GStatusBar <- setRefClass(
  "GStatusBar",
  contains = "GWidget",
  fields = list(
    context_id = "integer",
    text_value = "character"
  ),
  methods = list(
    initialize = function(toolkit = NULL, text = "", container = NULL, ...) {
      block <<- gtkStatusbarNew()
      widget <<- block
      ctx <- tryCatch(
        as.integer(gtkStatusbarGetContextId(block, "gWidgets2")),
        error = function(e) 1L
      )
      initFields(context_id = ctx, text_value = "")
      set_value(text)
      if (!is.null(container)) {
        if (!is(container, "GWindow"))
          getTopLevel(container)$add_statusbar(.self)
        else
          container$add_statusbar(.self)
      }
      callSuper(toolkit)
    },
    get_value = function(...) text_value,
    set_value = function(value, ...) {
      text_value <<- paste(value, collapse = ";")
      try(gtkStatusbarRemoveAll(block, context_id), silent = TRUE)
      if (nzchar(text_value))
        try(gtkStatusbarPush(block, context_id, text_value), silent = TRUE)
      invisible(NULL)
    }
  )
)
