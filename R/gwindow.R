##' @include GContainer.R
NULL

##' toolkit constructor for gwindow
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gwindow guiWidgetsToolkitRgtk4
.gwindow.guiWidgetsToolkitRgtk4 <- function(toolkit, title, visible = TRUE, name, width, height,
                                            parent, handler, action, ...) {
  GWindow$new(toolkit, title, visible = visible, name, width, height, parent, handler, action, ...)
}

## Main class for gwindow instances
GWindow <- setRefClass(
  "GWindow",
  contains = "GContainer",
  fields = list(
    menubar_area = "ANY",
    toolbar_area = "ANY",
    content_area = "ANY",
    statusbar_area = "ANY",
    statusbar_widget = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL, title = "", visible = TRUE, name = NULL,
                          width = NULL, height = NULL, parent = NULL, handler, action, ...) {
      widget <<- gtkWindowNew()
      set_value(title)
      if (is.null(width))
        width <- 400L
      if (is.null(height))
        height <- as.integer(0.7 * width)
      gtkWindowSetDefaultSize(widget, as.integer(width), as.integer(height))

      if (!is.null(parent)) {
        if (inherits(parent, "GComponent")) {
          parent_widget <- getWidget(parent)
          ## walk up to window if needed — best effort
          if (inherits(parent_widget, "GtkWindow")) {
            gtkWindowSetTransientFor(widget, parent_widget)
            gtkWindowSetModal(widget, TRUE)
          }
        } else if (is.numeric(parent) && length(parent) >= 2) {
          ## position not always available; ignore quietly
        }
      }

      initFields(
        toolkit = toolkit,
        block = NULL,
        menubar_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L),
        toolbar_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L),
        content_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L),
        statusbar_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L)
      )
      gtkWidgetSetHexpand(content_area, TRUE)
      gtkWidgetSetVexpand(content_area, TRUE)
      layout_widget()

      handler_id <<- add_handler_changed(handler, action)

      if (visible)
        gtkWindowPresent(widget)

      callSuper(...)
    },
    layout_widget = function() {
      outer <- gtkBoxNew(.GtkOrientation$VERTICAL, 0L)
      gtkBoxAppend(outer, menubar_area)
      gtkBoxAppend(outer, toolbar_area)
      gtkBoxAppend(outer, content_area)
      gtkBoxAppend(outer, statusbar_area)
      gtkWindowSetChild(widget, outer)
    },
    get_value = function(...) gtkWindowGetTitle(widget),
    set_value = function(value, ...) {
      gtkWindowSetTitle(widget, paste(value, collapse = " "))
    },
    set_focus = function(value) {
      if (isTRUE(value))
        gtkWindowPresent(widget)
    },
    get_size = function() c(width = -1L, height = -1L),
    update_widget = function(...) invisible(NULL),
    is_extant = function() {
      !inherits(try(gtkWindowGetTitle(widget), silent = TRUE), "try-error")
    },
    set_icon = function(stock) {
      ## GTK4: window icons via paintable / icon name — best-effort
      invisible(NULL)
    },
    add_child = function(child, ...) {
      if (missing(child) || is.null(child))
        return()
      if (is(child, "GMenuBar")) {
        add_menubar(child)
      } else if (is(child, "GToolBar")) {
        add_toolbar(child)
      } else if (is(child, "GStatusBar")) {
        add_statusbar(child)
      } else {
        ## only one content child
        old <- tryCatch(gtkWidgetGetFirstChild(content_area), error = function(e) NULL)
        if (!is.null(old))
          gtkBoxRemove(content_area, old)
        child_block <- getBlock(child)
        gtkWidgetSetHexpand(child_block, TRUE)
        gtkWidgetSetVexpand(child_block, TRUE)
        gtkBoxAppend(content_area, child_block)
      }
      child_bookkeeping(child)
    },
    remove_child = function(child) {
      child$set_parent(NULL)
      gtkBoxRemove(content_area, getBlock(child))
    },
    dispose_window = function() {
      gtkWindowDestroy(widget)
    },
    add_menubar = function(child, ...) {
      gtkBoxAppend(menubar_area, getBlock(child))
    },
    add_toolbar = function(child, ...) {
      gtkBoxAppend(toolbar_area, getBlock(child))
    },
    add_statusbar = function(child, ...) {
      statusbar_widget <<- child
      gtkBoxAppend(statusbar_area, getBlock(child))
    },
    set_infobar = function(msg, ...) {
      ## Phase 1: no GtkInfoBar — use statusbar if present
      set_statusbar(msg, ...)
    },
    set_statusbar = function(msg, ...) {
      if (!is(statusbar_widget, "uninitializedField"))
        statusbar_widget$set_value(msg)
    },
    clear_statusbar = function(...) {
      if (!is(statusbar_widget, "uninitializedField"))
        statusbar_widget$set_value("")
    },
    add_handler_changed = function(handler, action = NULL, ...) {
      add_handler_destroy(handler, action, ...)
    },
    add_handler_destroy = function(handler, action = NULL, ...) {
      add_handler("destroy", handler, action = action, ...)
    },
    add_handler_unrealize = function(handler, action, ...) {
      ## GTK4 close-request; return TRUE to stop close
      if (is_handler(handler)) {
        gSignalConnectR(widget, "close-request", function(...) {
          h <- list(obj = .self, action = action)
          out <- handler(h)
          isTRUE(out)
        })
      }
    }
  )
)
