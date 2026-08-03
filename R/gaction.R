##' @include GWidget.R
NULL

.gwa_action_env <- new.env(parent = emptyenv())
.gwa_action_env$counter <- 0L

next_gwa_action_name <- function() {
  .gwa_action_env$counter <- .gwa_action_env$counter + 1L
  paste0("a", .gwa_action_env$counter)
}

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gaction guiWidgetsToolkitRgtk4
.gaction.guiWidgetsToolkitRgtk4 <- function(toolkit, label, tooltip = NULL, icon = NULL,
                                            key.accel = NULL, handler = NULL, action = NULL,
                                            parent = NULL, ...) {
  GAction$new(toolkit, label, tooltip = tooltip, icon = icon, key.accel = key.accel,
              handler = handler, action = action, parent = parent, ...)
}

##' @noRd
accel_key_to_gtk <- function(accel_key) {
  accel_key <- as.character(accel_key)[1]
  if (!nzchar(accel_key))
    return(NULL)
  parts <- strsplit(accel_key, "-", fixed = TRUE)[[1]]
  if (length(parts) == 1L)
    return(parts[1])
  key <- parts[length(parts)]
  mods <- parts[-length(parts)]
  paste0(paste0("<", mods, ">", collapse = ""), key)
}

## Action proxy with syncable UI surfaces (buttons, Gio actions)
GAction <- setRefClass(
  "GAction",
  contains = "GWidget",
  fields = list(
    label_value = "character",
    tip_value = "ANY",
    icon_value = "ANY",
    accel_key = "ANY",
    enabled_value = "logical",
    gio_name = "character",
    proxies = "list",
    gio_actions = "list"
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
        gio_name = next_gwa_action_name(),
        proxies = list(),
        gio_actions = list(),
        change_signal = "activate",
        widget = NULL,
        block = NULL
      )
      if (!missing(parent) && !is.null(parent) && !is.null(key.accel))
        add_key_accel(parent)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    action_name = function() gio_name,
    detailed_action_name = function(prefix = "gwa") {
      paste0(prefix, ".", gio_name)
    },
    add_proxy = function(w) {
      if (is.null(w))
        return(invisible(NULL))
      proxies <<- c(proxies, list(w))
      sync_proxy(w)
      invisible(NULL)
    },
    register_gio_action = function(simple_action) {
      if (is.null(simple_action))
        return(invisible(NULL))
      gio_actions <<- c(gio_actions, list(simple_action))
      try(gSimpleActionSetEnabled(simple_action, enabled_value), silent = TRUE)
      invisible(NULL)
    },
    sync_proxy = function(w) {
      try({
        gtkWidgetSetSensitive(w, enabled_value)
        if (!is.null(tip_value) && nzchar(as.character(tip_value)[1]))
          gtkWidgetSetTooltipText(w, as.character(tip_value)[1])
        if (inherits(w, "GtkButton"))
          gtkButtonSetLabel(w, label_value)
      }, silent = TRUE)
      invisible(NULL)
    },
    sync_proxies = function() {
      lapply(proxies, function(w) sync_proxy(w))
      lapply(gio_actions, function(a) {
        try(gSimpleActionSetEnabled(a, enabled_value), silent = TRUE)
      })
      invisible(NULL)
    },
    get_value = function(...) label_value,
    set_value = function(value, ...) {
      label_value <<- as.character(value)[1]
      sync_proxies()
      invoke_change_handler()
    },
    get_tooltip = function(...) tip_value,
    set_tooltip = function(value) {
      tip_value <<- value
      sync_proxies()
    },
    get_enabled = function() enabled_value,
    set_enabled = function(value) {
      enabled_value <<- as.logical(value)[1]
      sync_proxies()
    },
    get_icon = function() icon_value,
    is_extant = function() TRUE,
    activate = function(...) invoke_change_handler(...),
    connect_to_toolkit_signal = function(signal, decorator, emitter) {
      ## no native GTK widget; observers only
    },
    add_handler_changed = function(handler, action = NULL, ...) {
      if (is_handler(handler)) {
        o <- gWidgets2:::observer(.self, handler, action)
        invisible(add_observer(o, "activate"))
      }
    },
    add_key_accel = function(parent) {
      if (is.null(accel_key) || is.null(parent))
        return(invisible(NULL))
      tryCatch({
        trigger_str <- accel_key_to_gtk(accel_key)
        if (is.null(trigger_str))
          return(invisible(NULL))
        toplevel <- getTopLevel(parent)
        win <- getWidget(toplevel)
        trigger <- gtkShortcutTriggerParseString(trigger_str)
        cb <- gtkCallbackActionNew(function(widget, args) {
          activate()
        })
        shortcut <- gtkShortcutNew(trigger, cb)
        ctrl <- gtkShortcutControllerNew()
        ## GTK_SHORTCUT_SCOPE_LOCAL = 0; managed/global may vary — local on window
        gtkShortcutControllerAddShortcut(ctrl, shortcut)
        gtkWidgetAddController(win, ctrl)
      }, error = function(e) invisible(NULL))
      invisible(NULL)
    }
  )
)
