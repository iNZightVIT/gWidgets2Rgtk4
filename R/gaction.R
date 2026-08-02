##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gaction guiWidgetsToolkitRgtk4
.gaction.guiWidgetsToolkitRgtk4 <- function(toolkit, label, tooltip = NULL, icon = NULL,
                                            key.accel = NULL, handler = NULL, action = NULL,
                                            parent = NULL, ...) {
  GAction$new(toolkit, label, tooltip = tooltip, icon = icon, key.accel = key.accel,
              handler = handler, action = action, parent = parent, ...)
}

## Lightweight action proxy (Gio/menu integration deferred to Phase 2)
GAction <- setRefClass(
  "GAction",
  contains = "GWidget",
  fields = list(
    label_value = "character",
    tip_value = "ANY",
    icon_value = "ANY",
    accel_key = "ANY",
    enabled_value = "logical"
  ),
  methods = list(
    initialize = function(toolkit = NULL, label = "", tooltip = NULL, icon = NULL,
                          key.accel = NULL, handler, action = NULL, parent, ...) {
      initFields(
        label_value = as.character(label)[1],
        tip_value = tooltip,
        icon_value = icon,
        accel_key = key.accel,
        enabled_value = TRUE,
        change_signal = "activate",
        widget = NULL,
        block = NULL
      )
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(...) label_value,
    set_value = function(value, ...) {
      label_value <<- as.character(value)[1]
      invoke_change_handler()
    },
    get_tooltip = function(...) tip_value,
    set_tooltip = function(value) {
      tip_value <<- value
    },
    get_enabled = function() enabled_value,
    set_enabled = function(value) {
      enabled_value <<- as.logical(value)
    },
    is_extant = function() TRUE,
    ## Proxy activate
    activate = function(...) invoke_change_handler(...),
    connect_to_toolkit_signal = function(signal, decorator, emitter) {
      ## no native GTK action object in Phase 1
    },
    add_handler_changed = function(handler, action = NULL, ...) {
      if (is_handler(handler)) {
        o <- gWidgets2:::observer(.self, handler, action)
        invisible(add_observer(o, "activate"))
      }
    }
  )
)
