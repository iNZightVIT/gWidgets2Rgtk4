##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gtoolbar guiWidgetsToolkitRgtk4
.gtoolbar.guiWidgetsToolkitRgtk4 <- function(toolkit, toolbar.list = list(),
                                             style = c("both", "icons", "text", "both-horiz"),
                                             container = NULL, ...) {
  GToolBar$new(toolkit, toolbar.list = toolbar.list, style = style,
               container = container, ...)
}

GToolBar <- setRefClass(
  "GToolBar",
  contains = "GWidget",
  fields = list(
    toolbar_list = "list",
    toolbar_style = "character"
  ),
  methods = list(
    initialize = function(toolkit = NULL, toolbar.list = list(),
                          style = c("both", "icons", "text", "both-horiz"),
                          container = NULL, ...) {
      widget <<- gtkBoxNew(.GtkOrientation$HORIZONTAL, 2L)
      style <- match.arg(style)
      initFields(
        block = widget,
        toolbar_list = list(),
        toolbar_style = style
      )
      add_toolbar_items(toolbar.list)
      if (!is.null(container) && is(container, "GWindow"))
        add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },
    add_toolbar_items = function(items) {
      sapply(items, function(item) {
        if (is(item, "GAction"))
          add_gaction_toolitem(item)
        else if (is(item, "GSeparator"))
          add_gseparator_toolitem(item)
        else
          add_widget_toolitem(item)
      })
      toolbar_list <<- gWidgets2:::merge.list(toolbar_list, items)
      invisible(NULL)
    },
    add_gseparator_toolitem = function(obj) {
      sep <- gtkSeparatorNew(.GtkOrientation$VERTICAL)
      gtkBoxAppend(widget, sep)
      invisible(NULL)
    },
    add_gaction_toolitem = function(obj) {
      style <- toolbar_style
      lab <- obj$get_value()
      icon <- obj$get_icon()
      show_icon <- style %in% c("both", "icons", "both-horiz")
      show_text <- style %in% c("both", "text", "both-horiz")

      icon_spec <- NULL
      if (show_icon) {
        candidates <- character(0)
        if (!is.null(icon) && !is_empty(icon))
          candidates <- c(candidates, as.character(icon)[1])
        if (!is_empty(lab))
          candidates <- c(candidates, as.character(lab)[1])
        for (cand in candidates) {
          icon_spec <- resolve_icon_spec(cand)
          if (!is.null(icon_spec))
            break
        }
      }

      has_icon <- !is.null(icon_spec)
      has_text <- show_text && !is_empty(lab) && nzchar(as.character(lab)[1])
      btn <- gtkButtonNew()
      if (has_icon && has_text) {
        box <- gtkBoxNew(.GtkOrientation$HORIZONTAL, 4L)
        gtkBoxAppend(box, make_gtk_image(icon_spec$src, icon_spec$kind, 16L))
        gtkBoxAppend(box, gtkLabelNew(as.character(lab)[1]))
        gtkButtonSetChild(btn, box)
      } else if (has_icon) {
        gtkButtonSetChild(btn, make_gtk_image(icon_spec$src, icon_spec$kind, 16L))
      } else {
        gtkButtonSetLabel(btn, if (has_text) as.character(lab)[1] else "")
      }

      tip <- obj$get_tooltip()
      if (!is.null(tip) && nzchar(as.character(tip)[1]))
        gtkWidgetSetTooltipText(btn, as.character(tip)[1])
      gtkWidgetSetSensitive(btn, obj$get_enabled())
      obj$add_proxy(btn)
      gSignalConnectR(btn, "clicked", function(...) {
        obj$activate()
      })
      gtkBoxAppend(widget, btn)
      invisible(NULL)
    },
    add_widget_toolitem = function(obj) {
      gtkBoxAppend(widget, getBlock(obj))
      invisible(NULL)
    },
    clear_toolbar = function() {
      child <- tryCatch(gtkWidgetGetFirstChild(widget), error = function(e) NULL)
      while (!is.null(child)) {
        gtkBoxRemove(widget, child)
        child <- tryCatch(gtkWidgetGetFirstChild(widget), error = function(e) NULL)
      }
      toolbar_list <<- list()
      invisible(NULL)
    },
    get_value = function(...) toolbar_list,
    set_value = function(value, ...) {
      clear_toolbar()
      add_toolbar_items(value)
    }
  )
)
