##' @include GWidget.R
NULL

## MessageType: INFO=0 WARNING=1 QUESTION=2 ERROR=3
## ButtonsType: NONE=0 OK=1 CLOSE=2 CANCEL=3 YES_NO=4 OK_CANCEL=5
## ResponseType: OK=-5 CANCEL=-6 CLOSE=-7 YES=-8 NO=-9 ACCEPT=-3 DELETE_EVENT=-4

.dialog_icon_type <- function(icon) {
  icon <- match.arg(icon, c("info", "warning", "error", "question"))
  switch(icon, info = 0L, warning = 1L, question = 2L, error = 3L)
}

.dialog_parent_window <- function(parent) {
  if (is.null(parent))
    return(NULL)
  if (is(parent, "GWindow"))
    return(getWidget(parent))
  if (inherits(parent, "GComponent")) {
    w <- getWidget(parent)
    if (inherits(w, "GtkWindow"))
      return(w)
  }
  if (inherits(parent, "GtkWindow"))
    return(parent)
  NULL
}

## Hidden fallback so GtkDialog is never mapped without a transient parent.
.dialog_fallback_env <- new.env(parent = emptyenv())

.dialog_ensure_parent <- function(parent = NULL) {
  win <- .dialog_parent_window(parent)
  if (!is.null(win))
    return(win)
  fb <- .dialog_fallback_env$window
  if (is.null(fb) ||
      inherits(try(gtkWindowGetTitle(fb), silent = TRUE), "try-error")) {
    fb <- gtkWindowNew()
    gtkWindowSetTitle(fb, "gWidgets2Rgtk4")
    gtkWidgetSetVisible(fb, FALSE)
    .dialog_fallback_env$window <- fb
  }
  fb
}

GDialog <- setRefClass(
  "GDialog",
  contains = "GContainer",
  methods = list(
    initialize = function(toolkit = NULL, msg = "", title = "", icon = "info",
                          parent = NULL, ...) {
      .parent <- .dialog_ensure_parent(parent)
      widget <<- gtkMessageDialogNew(
        parent = .parent,
        flags = 1L, ## MODAL
        message_type = .dialog_icon_type(icon),
        buttons_type = get_buttons(),
        message = as.character(msg[1])
      )
      if (length(msg) > 1)
        gtkMessageDialogSetMarkup(widget, paste(msg, collapse = "\n"))
      modify_widget()
      gtkWindowSetTitle(widget, title)
      gtkDialogSetDefaultResponse(widget, -5L) ## OK
      initFields(block = widget)
      callSuper(toolkit)
    },
    get_buttons = function() 2L, ## CLOSE
    ok_response = function() NULL,
    cancel_response = function() NULL,
    modify_widget = function() {},
    run = function() {
      gtkWidgetSetVisible(widget, TRUE)
      response <- gtkDialogRun(widget)
      if (response %in% c(-5L, -3L, -8L)) {
        ret <- ok_response()
      } else {
        ret <- cancel_response()
      }
      gtkWindowDestroy(widget)
      invisible(ret)
    }
  )
)

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gmessage guiWidgetsToolkitRgtk4
.gmessage.guiWidgetsToolkitRgtk4 <- function(toolkit, msg, title = "message",
                                             icon = c("info", "warning", "error", "question"),
                                             parent = NULL, ...) {
  icon <- match.arg(icon)
  dlg <- GMessage$new(toolkit, msg, title, icon, parent, ...)
  dlg$run()
}

GMessage <- setRefClass("GMessage", contains = "GDialog",
                        methods = list(get_buttons = function() 1L)) ## OK

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gconfirm guiWidgetsToolkitRgtk4
.gconfirm.guiWidgetsToolkitRgtk4 <- function(toolkit, msg, title = "Confirm",
                                             icon = c("info", "warning", "error", "question"),
                                             parent = NULL, ...) {
  icon <- match.arg(icon)
  dlg <- GConfirm$new(toolkit, msg, title, icon, parent, ...)
  dlg$run()
}

GConfirm <- setRefClass(
  "GConfirm",
  contains = "GDialog",
  methods = list(
    ok_response = function() TRUE,
    cancel_response = function() FALSE,
    get_buttons = function() 5L ## OK_CANCEL
  )
)

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .ginput guiWidgetsToolkitRgtk4
.ginput.guiWidgetsToolkitRgtk4 <- function(toolkit, msg, text = "", title = "Input",
                                           icon = c("info", "warning", "error", "question"),
                                           parent = NULL, ...) {
  icon <- match.arg(icon)
  dlg <- GInput$new(toolkit, msg, title, icon, parent, ...)
  dlg$set_text(text)
  dlg$run()
}

GInput <- setRefClass(
  "GInput",
  contains = "GDialog",
  fields = list(entry = "ANY"),
  methods = list(
    get_buttons = function() 5L,
    ok_response = function() gtkEditableGetText(entry),
    cancel_response = function() character(0),
    modify_widget = function() {
      entry <<- gtkEntryNew()
      area <- gtkDialogGetContentArea(widget)
      gtkBoxAppend(area, entry)
      gSignalConnectR(entry, "activate", function(...) {
        gtkDialogResponse(widget, -5L)
      })
    },
    set_text = function(txt) {
      gtkEditableSetText(entry, as.character(txt)[1])
      gtkWidgetGrabFocus(entry)
    }
  )
)

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gbasicdialog guiWidgetsToolkitRgtk4
.gbasicdialog.guiWidgetsToolkitRgtk4 <- function(toolkit, title = "Dialog", parent = NULL,
                                                 do.buttons = TRUE, handler = NULL,
                                                 action = NULL, ...) {
  GBasicDialog$new(toolkit, title = title, parent = parent, do.buttons = do.buttons,
                   handler = handler, action = action, ...)
}

GBasicDialog <- setRefClass(
  "GBasicDialog",
  contains = "GContainer",
  fields = list(handler = "ANY", action = "ANY"),
  methods = list(
    initialize = function(toolkit = NULL, title = "Dialog", parent = NULL,
                          do.buttons = TRUE, handler = NULL, action = NULL, ...) {
      .parent <- .dialog_ensure_parent(parent)
      dlg <- gtkDialogNew()
      gtkWindowSetTitle(dlg, title)
      gtkWindowSetTransientFor(dlg, .parent)
      gtkWindowSetModal(dlg, TRUE)
      if (do.buttons) {
        gtkDialogAddButton(dlg, "OK", -5L)
        gtkDialogAddButton(dlg, "Cancel", -6L)
        gtkDialogSetDefaultResponse(dlg, -5L)
      }
      content <- gtkBoxNew(.GtkOrientation$VERTICAL, 5L)
      area <- gtkDialogGetContentArea(dlg)
      gtkBoxAppend(area, content)
      gtkWidgetSetHexpand(content, TRUE)
      gtkWidgetSetVexpand(content, TRUE)
      initFields(widget = content, block = dlg, handler = handler, action = action)
      gtkWindowPresent(dlg)
      callSuper(toolkit)
    },
    add_child = function(child, ...) {
      child_block <- getBlock(child)
      gtkWidgetSetHexpand(child_block, TRUE)
      gtkWidgetSetVexpand(child_block, TRUE)
      gtkBoxAppend(widget, child_block)
      child_bookkeeping(child)
    },
    dispose = function() gtkWindowDestroy(block),
    set_visible = function(...) {
      gtkWidgetSetVisible(block, TRUE)
      response <- gtkDialogRun(block)
      h <- list(obj = .self, action = action)
      ret <- FALSE
      if (response %in% c(-5L, -3L, -8L)) {
        if (!is.null(handler))
          handler(h)
        ret <- TRUE
      }
      try(gtkWindowDestroy(block), silent = TRUE)
      invisible(ret)
    }
  )
)

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .galert guiWidgetsToolkitRgtk4
.galert.guiWidgetsToolkitRgtk4 <- function(toolkit, msg, title = "message", delay = 3,
                                           parent = NULL, ...) {
  if (is(parent, "GWindow")) {
    parent$set_infobar(msg)
  } else {
    w <- gtkWindowNew()
    gtkWindowSetTitle(w, title)
    gtkWindowSetDefaultSize(w, 300L, 150L)
    l <- gtkLabelNew(paste(msg, collapse = "\n"))
    gtkWindowSetChild(w, l)
    .parent <- .dialog_parent_window(parent)
    if (!is.null(.parent))
      gtkWindowSetTransientFor(w, .parent)
    gtkWindowPresent(w)
    gTimeoutAdd(as.integer(delay * 1000), function() {
      try(gtkWindowDestroy(w), silent = TRUE)
      FALSE
    })
  }
  invisible(NULL)
}
