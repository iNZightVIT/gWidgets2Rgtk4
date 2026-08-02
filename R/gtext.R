##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gtext guiWidgetsToolkitRgtk4
.gtext.guiWidgetsToolkitRgtk4 <- function(toolkit, text = NULL, width = NULL, height = 300,
                                          font.attr = NULL, wrap = TRUE, handler = NULL,
                                          action = NULL, container = NULL, ...) {
  GText$new(toolkit, text = text, width = width, height = height, font.attr = font.attr,
            wrap = wrap, handler = handler, action = action, container = container, ...)
}

GText <- setRefClass(
  "GText",
  contains = "GWidget",
  fields = list(buffer = "ANY"),
  methods = list(
    initialize = function(toolkit = NULL, text = NULL, width = NULL, height = 300,
                          font.attr = NULL, wrap = TRUE, handler = NULL, action = NULL,
                          container = NULL, ...) {
      widget <<- gtkTextViewNew()
      buffer <<- gtkTextViewGetBuffer(widget)
      ## WrapMode: NONE=0 CHAR=1 WORD=2 WORD_CHAR=3
      gtkTextViewSetWrapMode(widget, if (isTRUE(wrap)) 2L else 0L)
      block <<- gtkScrolledWindowNew()
      gtkScrolledWindowSetPolicy(block, .GtkPolicyType$AUTOMATIC, .GtkPolicyType$AUTOMATIC)
      if (!is.null(width))
        gtkWidgetSetSizeRequest(block, as.integer(width), as.integer(height))
      else if (!is.null(height))
        gtkWidgetSetSizeRequest(block, -1L, as.integer(height))
      gtkScrolledWindowSetChild(block, widget)
      if (!is.null(text))
        set_value(text)
      initFields(change_signal = "changed", default_expand = TRUE, default_fill = TRUE)
      ## connect buffer changed → observers
      gSignalConnectR(buffer, "changed", function(...) {
        notify_observers(signal = "changed")
      })
      connected_signals[["changed"]] <<- TRUE
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = FALSE, ...) {
      bounds <- gtkTextBufferGetBounds(buffer)
      ## GetBounds typically returns start/end iters — API may return list
      txt <- tryCatch({
        start <- gtkTextBufferGetStartIter(buffer)
        end <- gtkTextBufferGetEndIter(buffer)
        as.character(gtkTextBufferGetText(buffer, start, end, TRUE))
      }, error = function(e) {
        ## fallback
        ""
      })
      txt
    },
    set_value = function(value, ...) {
      value <- paste(value, collapse = "\n")
      gtkTextBufferSetText(buffer, value, -1L)
    },
    insert_text = function(value, where = "end", do.newline = TRUE, ...) {
      if (is.null(value) || !nzchar(paste(value, collapse = "")))
        return()
      value <- paste(value, collapse = "\n")
      if (do.newline)
        value <- paste0(value, "\n")
      end <- gtkTextBufferGetEndIter(buffer)
      gtkTextBufferInsert(buffer, end, value, -1L)
    },
    connect_to_toolkit_signal = function(signal, decorator, emitter = .self$handler_widget()) {
      ## buffer already connected for "changed"
      if (identical(signal, "changed"))
        return()
      callSuper(signal, decorator, emitter)
    }
  )
)
