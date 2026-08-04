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
    statusbar_widget = "ANY",
    .window_pos = "ANY"
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
          set_position(as.integer(parent[1]), as.integer(parent[2]))
        }
      }

      initFields(
        toolkit = toolkit,
        block = NULL,
        menubar_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L),
        toolbar_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L),
        content_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L),
        statusbar_area = gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L),
        .window_pos = c(x = -1L, y = -1L)
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
    get_value = function(...) as.character(gtkWindowGetTitle(widget)),
    set_value = function(value, ...) {
      gtkWindowSetTitle(widget, paste(value, collapse = " "))
    },
    set_focus = function(value) {
      if (isTRUE(value))
        gtkWindowPresent(widget)
    },
    get_size = function() {
      sz <- .widget_get_size(widget)
      if (sz[["width"]] > 0L && sz[["height"]] > 0L)
        return(sz)
      ds <- tryCatch(gtkWindowGetDefaultSize(widget), error = function(e) NULL)
      if (!is.null(ds)) {
        dw <- as.integer(ds$width)[1]
        dh <- as.integer(ds$height)[1]
        if (!is.na(dw) && !is.na(dh) && dw > 0L && dh > 0L)
          return(c(width = dw, height = dh))
      }
      sz
    },
    ## GTK4: apps cannot reliably move toplevels (esp. Wayland). Track requested
    ## coords for API parity; present() is the portable action.
    get_position = function() {
      if (is(.window_pos, "uninitializedField") || is.null(.window_pos))
        c(x = -1L, y = -1L)
      else
        as.integer(.window_pos)
    },
    set_position = function(x, y = NULL) {
      if (length(x) >= 2L && (missing(y) || is.null(y))) {
        y <- x[2]
        x <- x[1]
      }
      .window_pos <<- c(x = as.integer(x)[1], y = as.integer(y)[1])
      invisible(.window_pos)
    },
    center = function() {
      ## Best-effort: present window. True pixel centering needs compositor
      ## support and readable GdkRectangle fields (opaque in Rgtk4 today).
      sz <- get_size()
      mon <- tryCatch({
        display <- gdkDisplayGetDefault()
        mons <- gdkDisplayGetMonitors(display)
        n <- as.integer(gListModelGetNItems(mons))[1]
        if (!is.na(n) && n > 0L) gListModelGetObject(mons, 0L) else NULL
      }, error = function(e) NULL)
      if (!is.null(mon) && sz[["width"]] > 0L && sz[["height"]] > 0L) {
        ## Approximate from physical mm when pixel geometry is unavailable
        w_mm <- tryCatch(as.integer(gdkMonitorGetWidthMm(mon))[1], error = function(e) NA_integer_)
        h_mm <- tryCatch(as.integer(gdkMonitorGetHeightMm(mon))[1], error = function(e) NA_integer_)
        scale <- tryCatch(as.integer(gdkMonitorGetScaleFactor(mon))[1], error = function(e) 1L)
        if (!is.na(w_mm) && !is.na(h_mm) && w_mm > 0L && h_mm > 0L) {
          ## ~96 DPI → px ≈ mm * 96 / 25.4
          mw <- as.integer(w_mm * 96 / 25.4 * max(scale, 1L))
          mh <- as.integer(h_mm * 96 / 25.4 * max(scale, 1L))
          set_position(as.integer((mw - sz[["width"]]) / 2),
                       as.integer((mh - sz[["height"]]) / 2))
        }
      }
      gtkWindowPresent(widget)
      invisible(NULL)
    },
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
