##' @include gtk-misc.R
NULL

##' Base class for widgets and containers
##'
##' @rdname gWidgets2Rgtk4-package
##' @export GComponent
GComponent <- setRefClass(
  "GComponent",
  contains = "BasicToolkitInterface",
  fields = list(
    handler_id = "ANY",
    .e = "environment",
    ..invalid = "logical",
    ..invalid_reason = "character",
    coerce_with = "FunctionOrNULL"
  ),
  methods = list(
    initialize = function(toolkit = guiToolkit(), ...,
                          expand, fill, anchor, label) {
      initFields(toolkit = toolkit, .e = new.env())
      if (is(handler_id, "uninitializedField"))
        handler_id <<- NULL
      if (is(default_expand, "uninitializedField"))
        default_expand <<- NULL
      if (is(default_fill, "uninitializedField"))
        default_fill <<- NULL
      callSuper(...)
    },
    show = function() {
      cat(sprintf("Object of class %s\n", class(.self)[1]))
    },
    get_length = function(...) 1L,
    get_visible = function() as.logical(gtkWidgetGetVisible(widget)),
    set_visible = function(value) gtkWidgetSetVisible(widget, as.logical(value)),
    get_focus = function() FALSE,
    set_focus = function(value) {
      if (isTRUE(value) && gtkWidgetGetCanFocus(block))
        gtkWidgetGrabFocus(block)
    },
    get_enabled = function() as.logical(gtkWidgetGetSensitive(widget)),
    set_enabled = function(value) gtkWidgetSetSensitive(widget, as.logical(value)),
    get_tooltip = function(...) {
      tip <- gtkWidgetGetTooltipText(widget)
      if (is.null(tip)) "" else as.character(tip)
    },
    set_tooltip = function(value) {
      gtkWidgetSetTooltipText(widget, paste(value, collapse = "\n"))
    },
    set_font = function(value) {
      ## Fonts deferred (CSS / Pango); no-op in Phase 1
      invisible(NULL)
    },
    get_attr = function(key) {
      if (missing(key))
        ls(.e)
      else if (exists(key, envir = .e, inherits = FALSE))
        get(key, envir = .e, inherits = FALSE)
      else
        NULL
    },
    set_attr = function(key, value) {
      assign(key, value, envir = .e)
    },
    is_extant = function() {
      if (is.null(block) || is(block, "uninitializedField"))
        return(!is.null(widget) && !is(widget, "uninitializedField"))
      !inherits(try(gtkWidgetGetVisible(block), silent = TRUE), "try-error")
    },
    get_size = function(...) {
      ## Allocation may be 0 before realize; size-request is a fallback
      c(width = -1L, height = -1L)
    },
    set_size = function(value, ...) {
      if (is.list(value))
        value <- unlist(value)
      if (length(value) >= 2) {
        width <- as.integer(value[1])
        height <- as.integer(value[2])
      } else if (!is.null(names(value)) && names(value)[1] == "height") {
        width <- -1L
        height <- as.integer(value)
      } else {
        width <- as.integer(value)
        height <- -1L
      }
      gtkWidgetSetSizeRequest(getBlock(.self), width, height)
    },
    set_invalid = function(value, msg) {
      if (as.logical(value)) {
        ..invalid <<- TRUE
        ..invalid_reason <<- as.character(msg)
      } else {
        ..invalid <<- FALSE
        ..invalid_reason <<- ""
      }
    },
    is_invalid = function(...) {
      if (length(..invalid) == 0)
        ..invalid <<- FALSE
      ..invalid
    },
    set_parent = function(parent) {
      parent <<- parent
    },
    add_to_parent = function(parent, child, expand = NULL, fill = NULL, anchor = NULL, ...) {
      if (missing(parent) || is.null(parent))
        return()
      if (is(parent, "GLayout"))
        return()
      if (!is(parent, "GContainer") && is.logical(parent) && parent) {
        tmp <- gwindow(toolkit = toolkit)
        tmp$add_child(child, expand, fill, anchor, ...)
        return()
      }
      if (!is(parent, "GContainer")) {
        message("parent is not a container")
        return()
      }
      parent$add_child(child, expand, fill, anchor, ...)
    },
    handler_widget = function() widget,
    ## DnD deferred to later phase
    add_drop_source = function(handler, action = NULL, data.type = "text", ...) {
      warning("Drag-and-drop source not implemented in gWidgets2Rgtk4 Phase 1", call. = FALSE)
    },
    add_drop_target = function(handler, action = NULL, ...) {
      warning("Drag-and-drop target not implemented in gWidgets2Rgtk4 Phase 1", call. = FALSE)
    },
    add_drag_motion = function(handler, action = NULL, ...) {
      warning("Drag motion not implemented in gWidgets2Rgtk4 Phase 1", call. = FALSE)
    }
  )
)

##' Observable GComponent with toolkit signal integration
GComponentObservable <- setRefClass(
  "GComponentObservable",
  fields = list(
    change_signal = "character",
    connected_signals = "list"
  ),
  contains = "GComponent",
  methods = list(
    event_decorator = function(handler) {
      force(handler)
      function(.self, ...) {
        out <- handler(.self, ...)
        if (is.atomic(out) && is.logical(out) && out[1])
          out[1]
        else
          FALSE
      }
    },
    is_handler = function(handler) {
      !missing(handler) && !is.null(handler) && is.function(handler)
    },
    add_handler = function(signal, handler, action = NULL, decorator, emitter) {
      if (is_handler(handler)) {
        o <- gWidgets2:::observer(.self, handler, action)
        ## Do not pass missing decorator/emitter through named args —
        ## evaluating a missing formal errors before the callee default applies.
        if (missing(emitter) && missing(decorator)) {
          connect_to_toolkit_signal(signal)
        } else if (missing(emitter)) {
          connect_to_toolkit_signal(signal, decorator)
        } else if (missing(decorator)) {
          connect_to_toolkit_signal(signal, emitter = emitter)
        } else {
          connect_to_toolkit_signal(signal, decorator, emitter = emitter)
        }
        invisible(add_observer(o, signal))
      }
    },
    connect_to_toolkit_signal = function(signal, decorator,
                                         emitter = .self$handler_widget()) {
      f <- function(...) {
        .self$notify_observers(signal = signal, ...)
      }
      if (!missing(decorator))
        f <- decorator(f)
      if (is.null(connected_signals[[signal, exact = TRUE]])) {
        gSignalConnectR(emitter, signal, f)
        connected_signals[[signal]] <<- TRUE
      }
    },
    invoke_handler = function(signal, ...) {
      notify_observers(..., signal = signal)
    },
    invoke_change_handler = function(...) {
      if (!is(change_signal, "uninitializedField") && length(change_signal))
        invoke_handler(signal = change_signal, ...)
    },
    block_handlers = function() block_observers(),
    block_handler = function(ID) block_observer(ID),
    unblock_handlers = function() unblock_observers(),
    unblock_handler = function(ID) unblock_observer(ID),
    remove_handlers = function() remove_observers(),
    remove_handler = function(ID) remove_observer(ID),
    add_handler_changed = function(handler, action = NULL, ...) {
      if (!is(change_signal, "uninitializedField") && length(change_signal)) {
        add_handler(change_signal, handler, action, ...)
      } else {
        stop("No change_signal defined for widget")
      }
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      add_handler("clicked", handler, action, ...)
    },
    add_handler_keystroke = function(handler, action = NULL, ...) {
      ## GTK4: key events need event controllers; stub for Phase 1
      warning("addHandlerKeystroke not fully implemented for GTK4", call. = FALSE)
    },
    add_handler_button_press = function(handler, action = NULL, ...) {
      warning("addHandlerButtonPress not fully implemented for GTK4", call. = FALSE)
    },
    add_handler_button_release = function(handler, action = NULL, ...) {
      warning("addHandlerButtonRelease not fully implemented for GTK4", call. = FALSE)
    },
    add_handler_control_clicked = function(handler, action = NULL, ...) {
      warning("addHandlerControlClicked not fully implemented for GTK4", call. = FALSE)
    },
    add_handler_shift_clicked = function(handler, action = NULL, ...) {
      warning("addHandlerShiftClicked not fully implemented for GTK4", call. = FALSE)
    },
    add_handler_focus = function(handler, action = NULL, ...) {
      warning("addHandlerFocus not fully implemented for GTK4", call. = FALSE)
    },
    add_handler_blur = function(handler, action = NULL, ...) {
      warning("addHandlerBlur not fully implemented for GTK4", call. = FALSE)
    },
    add_popup_menu = function(menulist, action = NULL, ...) {
      warning("Popup menus deferred to Phase 2", call. = FALSE)
    },
    add_3rd_mouse_popup_menu = function(menulist, action = NULL, ...) {
      warning("Popup menus deferred to Phase 2", call. = FALSE)
    }
  )
)
