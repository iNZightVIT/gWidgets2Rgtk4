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
      icon_name <- NULL
      if (!is.null(icon) && !is_empty(icon))
        icon_name <- stock_to_icon_name(icon)
      if (is.null(icon_name) || !nzchar(icon_name))
        icon_name <- stock_to_icon_name(lab)

      show_icon <- style %in% c("both", "icons", "both-horiz")
      show_text <- style %in% c("both", "text", "both-horiz")

      if (show_icon && !is.null(icon_name) && nzchar(icon_name) && !file.exists(icon_name)) {
        btn <- gtkButtonNewFromIconName(icon_name)
        if (show_text)
          gtkButtonSetLabel(btn, lab)
      } else if (show_text || !show_icon) {
        btn <- gtkButtonNewWithLabel(lab)
      } else {
        btn <- gtkButtonNewWithLabel(lab)
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
