##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gmenu guiWidgetsToolkitRgtk4
.gmenu.guiWidgetsToolkitRgtk4 <- function(toolkit, menu.list = list(), popup = FALSE,
                                          container = NULL, ...) {
  if (isTRUE(popup))
    GMenuPopup$new(toolkit, menu.list = menu.list, ...)
  else
    GMenuBar$new(toolkit, menu.list = menu.list, container = container, ...)
}

.gmenu_env <- new.env(parent = emptyenv())

##' @noRd
build_gmenu_model <- function(items, action_prefix = "gwa") {
  model <- gMenuNew()
  group <- gSimpleActionGroupNew()
  append_menu_items(model, group, items, action_prefix)
  list(model = model, group = group)
}

##' GtkPopoverMenuBar requires every toplevel model item to be a submenu.
##' iNZight uses disabled gaction "placeholders" (Dataset/Variables/Plot with
##' no data) as toplevel entries — wrap those so GTK does not warn
##' "Don't know how to handle this item".
##' @noRd
normalize_menubar_toplevel <- function(items) {
  if (!length(items))
    return(items)
  nms <- names(items)
  out <- vector("list", length(items))
  for (i in seq_along(items)) {
    item <- items[[i]]
    if (is.list(item) && !is(item, "GComponent"))
      out[[i]] <- item
    else
      out[[i]] <- list(item)
  }
  if (!is.null(nms))
    names(out) <- nms
  out
}

##' Next unique stateful action name within a group builder pass
##' @noRd
.next_menu_state_name <- function(prefix = "st") {
  i <- get0(".menu_state_i", envir = .gmenu_env, ifnotfound = 0L)
  i <- as.integer(i) + 1L
  assign(".menu_state_i", i, envir = .gmenu_env)
  paste0(prefix, i)
}

append_menu_items <- function(menu, group, items, action_prefix = "gwa") {
  if (!length(items))
    return(invisible(NULL))
  section <- gMenuNew()
  has_items <- FALSE
  nms <- names(items)
  for (i in seq_along(items)) {
    item <- items[[i]]
    nm <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else NULL
    if (is(item, "GSeparator")) {
      if (has_items) {
        gMenuAppendSection(menu, NULL, section)
        section <- gMenuNew()
        has_items <- FALSE
      }
    } else if (is(item, "GAction")) {
      add_gaction_to_menu(section, group, item, action_prefix)
      has_items <- TRUE
    } else if (is.list(item) && !is(item, "GComponent")) {
      sub <- gMenuNew()
      append_menu_items(sub, group, item, action_prefix)
      label <- if (!is.null(nm)) nm else "Menu"
      gMenuAppendSubmenu(section, label, sub)
      has_items <- TRUE
    } else if (is(item, "GRadio")) {
      add_gradio_to_menu(section, group, item, action_prefix)
      has_items <- TRUE
    } else if (is(item, "GCheckbox")) {
      add_gcheckbox_to_menu(section, group, item, action_prefix)
      has_items <- TRUE
    } else if (is(item, "GComponent")) {
      warning("Arbitrary widget menu items not supported in GTK4 gmenu; skipped",
              call. = FALSE)
    } else {
      warning("Unrecognized menu item; skipped", call. = FALSE)
    }
  }
  if (has_items)
    gMenuAppendSection(menu, NULL, section)
  invisible(NULL)
}

add_gaction_to_menu <- function(menu, group, item, action_prefix = "gwa") {
  name <- item$action_name()
  label <- item$get_value()
  detailed <- paste0(action_prefix, ".", name)
  gMenuAppend(menu, label, detailed)
  ## Reuse existing Gio action if already registered under this name
  existing <- tryCatch(gSimpleActionGroupLookup(group, name), error = function(e) NULL)
  if (is.null(existing)) {
    sa <- gSimpleActionNew(name, NULL)
    ## capture item for activate
    local({
      act <- item
      gSignalConnectR(sa, "activate", function(a, p) {
        act$activate()
      })
    })
    gActionMapAddAction(group, sa)
    item$register_gio_action(sa)
  }
  invisible(NULL)
}

##' Map GRadio to one Gio action per option (Rgtk4 lacks usable GVariantType
##' for stateful string radio actions). Handlers still sync via set_index.
##' Menu labels get a bullet on the current selection; GMenuBar rebuilds on
##' change so the marker moves when the menu is reopened.
##' @noRd
add_gradio_to_menu <- function(menu, group, item, action_prefix = "gwa") {
  labels <- as.character(item$get_items())
  if (!length(labels))
    return(invisible(NULL))
  idx <- as.integer(item$get_index())[1]
  if (is.na(idx) || idx < 1L || idx > length(labels))
    idx <- 1L
  for (i in seq_along(labels)) {
    name <- .next_menu_state_name("radio")
    lab <- labels[i]
    ## Mark the currently selected option (no Gio radio attribute without VariantType)
    if (identical(as.integer(i), as.integer(idx)))
      lab <- paste0("\u2022 ", lab)
    sa <- gSimpleActionNew(name, NULL)
    local({
      radio <- item
      ii <- i
      gSignalConnectR(sa, "activate", function(a, param) {
        radio$set_index(ii)
      })
    })
    gActionMapAddAction(group, sa)
    gMenuAppend(menu, lab, paste0(action_prefix, ".", name))
  }
  invisible(NULL)
}

##' Map GCheckbox to a stateful Gio boolean action
##' @noRd
add_gcheckbox_to_menu <- function(menu, group, item, action_prefix = "gwa") {
  name <- .next_menu_state_name("check")
  checked <- isTRUE(as.logical(item$get_value())[1])
  sa <- gSimpleActionNewStateful(name, NULL, gVariantNewBoolean(checked))
  local({
    cb <- item
    gSignalConnectR(sa, "activate", function(a, param) {
      cur <- tryCatch(gVariantGetBoolean(gActionGetState(a)),
                      error = function(e) FALSE)
      new_val <- !isTRUE(as.logical(cur)[1])
      gSimpleActionSetState(a, gVariantNewBoolean(new_val))
      cb$set_value(new_val)
    })
  })
  gActionMapAddAction(group, sa)
  label <- as.character(item$get_items())[1]
  if (!nzchar(label))
    label <- "Toggle"
  gMenuAppend(menu, label, paste0(action_prefix, ".", name))
  invisible(NULL)
}

## Toplevel menu bar (GtkPopoverMenuBar + GMenuModel)
GMenuBar <- setRefClass(
  "GMenuBar",
  contains = "GWidget",
  fields = list(
    menu_list = "list",
    menu_model = "ANY",
    action_group = "ANY",
    action_prefix = "character",
    rebuilding = "logical"
  ),
  methods = list(
    initialize = function(toolkit = NULL, menu.list = list(), container = NULL, ...) {
      if (is(widget, "uninitializedField") || is.null(widget)) {
        initFields(
          menu_list = list(),
          action_prefix = "gwa",
          menu_model = NULL,
          action_group = NULL,
          rebuilding = FALSE
        )
        rebuild_menubar(menu.list)
        add_to_parent(container, .self, ...)
      }
      callSuper(toolkit)
    },
    ## Radio menu bullets are static labels; rebuild when GRadio changes
    ## so the marker tracks the current selection on next open.
    wire_radio_rebuild = function(items) {
      host <- .self
      walk <- function(x) {
        if (!length(x))
          return()
        for (item in x) {
          if (is(item, "GRadio")) {
            already <- tryCatch(
              exists("menu_rebuild_wired", envir = item$.e, inherits = FALSE) &&
                isTRUE(item$.e$menu_rebuild_wired),
              error = function(e) FALSE
            )
            if (!already) {
              tryCatch(item$.e$menu_rebuild_wired <- TRUE, error = function(e) NULL)
              o <- gWidgets2:::observer(item, function(h, ...) {
                if (!isTRUE(host$rebuilding))
                  host$rebuild_menubar()
              }, NULL)
              item$add_observer(o, item$change_signal)
            }
          } else if (is.list(item) && !is(item, "GComponent")) {
            walk(item)
          }
        }
      }
      walk(items)
      invisible(NULL)
    },
    rebuild_menubar = function(items = menu_list) {
      rebuilding <<- TRUE
      on.exit(rebuilding <<- FALSE)
      menu_list <<- items
      ## Store raw list (placeholders stay as gactions); normalize only for GTK.
      built <- build_gmenu_model(normalize_menubar_toplevel(items), action_prefix)
      menu_model <<- built$model
      action_group <<- built$group
      if (is(widget, "uninitializedField") || is.null(widget)) {
        widget <<- gtkPopoverMenuBarNewFromModel(menu_model)
        block <<- widget
      } else {
        try(gtkWidgetInsertActionGroup(widget, action_prefix, NULL), silent = TRUE)
        gtkPopoverMenuBarSetMenuModel(widget, menu_model)
      }
      gtkWidgetInsertActionGroup(widget, action_prefix, action_group)
      wire_radio_rebuild(items)
      invisible(NULL)
    },
    add_menu_items = function(sub_menu, items) {
      ## Compatibility with RGtk2 API: append into existing model
      append_value(items)
    },
    clear_menubar = function() {
      empty <- gMenuNew()
      menu_model <<- empty
      action_group <<- gSimpleActionGroupNew()
      menu_list <<- list()
      if (!is.null(widget) && !is(widget, "uninitializedField")) {
        try(gtkWidgetInsertActionGroup(widget, action_prefix, NULL), silent = TRUE)
        gtkPopoverMenuBarSetMenuModel(widget, menu_model)
        gtkWidgetInsertActionGroup(widget, action_prefix, action_group)
      }
      invisible(NULL)
    },
    get_value = function(...) menu_list,
    set_value = function(value, ...) {
      rebuild_menubar(value)
    },
    append_value = function(items) {
      menu_list <<- gWidgets2:::merge.list(menu_list, items)
      rebuild_menubar(menu_list)
    }
  )
)

## Popup menu (GtkPopoverMenu + GMenuModel)
GMenuPopup <- setRefClass(
  "GMenuPopup",
  contains = "GMenuBar",
  methods = list(
    initialize = function(toolkit = NULL, menu.list = list(), ...) {
      initFields(
        menu_list = list(),
        action_prefix = "gwa",
        menu_model = NULL,
        action_group = NULL,
        rebuilding = FALSE
      )
      rebuild_menubar(menu.list)
      widget <<- gtkPopoverMenuNewFromModel(menu_model)
      block <<- widget
      try(gtkPopoverSetHasArrow(widget, FALSE), silent = TRUE)
      callSuper(toolkit)
    },
    rebuild_menubar = function(items = menu_list) {
      rebuilding <<- TRUE
      on.exit(rebuilding <<- FALSE)
      built <- build_gmenu_model(items, action_prefix)
      menu_model <<- built$model
      action_group <<- built$group
      menu_list <<- items
      if (!is.null(widget) && !is(widget, "uninitializedField")) {
        parent_w <- tryCatch(gtkWidgetGetParent(widget), error = function(e) NULL)
        if (!is.null(parent_w))
          try(gtkWidgetInsertActionGroup(parent_w, action_prefix, NULL), silent = TRUE)
        gtkPopoverMenuSetMenuModel(widget, menu_model)
        if (!is.null(parent_w))
          gtkWidgetInsertActionGroup(parent_w, action_prefix, action_group)
      }
      wire_radio_rebuild(items)
      invisible(NULL)
    },
    attach_to = function(parent_widget) {
      if (is.null(parent_widget))
        return(invisible(NULL))
      ## Popover must have a parent; re-parent if needed
      cur <- tryCatch(gtkWidgetGetParent(widget), error = function(e) NULL)
      if (is.null(cur)) {
        gtkWidgetSetParent(widget, parent_widget)
      }
      gtkWidgetInsertActionGroup(parent_widget, action_prefix, action_group)
      invisible(NULL)
    },
    popup_at = function(parent_widget, x = 0L, y = 0L) {
      ## Refresh radio bullets before show
      rebuild_menubar(menu_list)
      attach_to(parent_widget)
      ## Position roughly under the click using offset relative to parent size
      tryCatch({
        ww <- as.integer(gtkWidgetGetWidth(parent_widget))
        wh <- as.integer(gtkWidgetGetHeight(parent_widget))
        if (is.na(ww) || ww < 1L) ww <- 1L
        if (is.na(wh) || wh < 1L) wh <- 1L
        x_off <- as.integer(x - ww / 2)
        y_off <- as.integer(-(wh - y))
        gtkPopoverSetOffset(widget, x_off, y_off)
      }, error = function(e) invisible(NULL))
      gtkPopoverPopup(widget)
      invisible(NULL)
    }
  )
)
