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

##' @noRd
build_gmenu_model <- function(items, action_prefix = "gwa") {
  model <- gMenuNew()
  group <- gSimpleActionGroupNew()
  append_menu_items(model, group, items, action_prefix)
  list(model = model, group = group)
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
    } else if (is(item, "GRadio") || is(item, "GCheckbox")) {
      warning("Radio/checkbox menu items not fully supported in GTK4 gmenu; skipped",
              call. = FALSE)
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

## Toplevel menu bar (GtkPopoverMenuBar + GMenuModel)
GMenuBar <- setRefClass(
  "GMenuBar",
  contains = "GWidget",
  fields = list(
    menu_list = "list",
    menu_model = "ANY",
    action_group = "ANY",
    action_prefix = "character"
  ),
  methods = list(
    initialize = function(toolkit = NULL, menu.list = list(), container = NULL, ...) {
      if (is(widget, "uninitializedField") || is.null(widget)) {
        initFields(
          menu_list = list(),
          action_prefix = "gwa",
          menu_model = NULL,
          action_group = NULL
        )
        rebuild_menubar(menu.list)
        add_to_parent(container, .self, ...)
      }
      callSuper(toolkit)
    },
    rebuild_menubar = function(items = menu_list) {
      built <- build_gmenu_model(items, action_prefix)
      menu_model <<- built$model
      action_group <<- built$group
      menu_list <<- items
      if (is(widget, "uninitializedField") || is.null(widget)) {
        widget <<- gtkPopoverMenuBarNewFromModel(menu_model)
        block <<- widget
      } else {
        try(gtkWidgetInsertActionGroup(widget, action_prefix, NULL), silent = TRUE)
        gtkPopoverMenuBarSetMenuModel(widget, menu_model)
      }
      gtkWidgetInsertActionGroup(widget, action_prefix, action_group)
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
        action_group = NULL
      )
      built <- build_gmenu_model(menu.list, action_prefix)
      menu_model <<- built$model
      action_group <<- built$group
      menu_list <<- menu.list
      widget <<- gtkPopoverMenuNewFromModel(menu_model)
      block <<- widget
      try(gtkPopoverSetHasArrow(widget, FALSE), silent = TRUE)
      callSuper(toolkit)
    },
    rebuild_menubar = function(items = menu_list) {
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
