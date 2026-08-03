##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gfile guiWidgetsToolkitRgtk4
.gfile.guiWidgetsToolkitRgtk4 <- function(toolkit, text = "",
                                          type = c("open", "save", "selectdir"),
                                          initial.filename = NULL, initial.dir = getwd(),
                                          filter = list(), multi = FALSE, ...,
                                          parent = NULL) {
  type <- match.arg(type)
  ## action: 0 open, 1 save, 2 select folder
  action <- switch(type, open = 0L, save = 1L, selectdir = 2L)
  title <- if (nzchar(text)) text else switch(type, open = "Open", save = "Save",
                                              selectdir = "Select folder")
  ## Always give GTK a transient parent (avoids "mapped without a transient parent")
  parent_win <- .dialog_ensure_parent(parent)
  res <- gtkFileChooserDialogRun(parent = parent_win, title = title, action = action)
  ## ACCEPT / OK
  if (is.null(res) || is.null(res$file) || !(res$response %in% c(-3L, -5L)))
    return(character(0))
  as.character(res$file)
}

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gfilebrowse guiWidgetsToolkitRgtk4
.gfilebrowse.guiWidgetsToolkitRgtk4 <- function(toolkit, text = "", type = "open",
                                                initial.filename = NULL, initial.dir = getwd(),
                                                filter = list(), quote = TRUE,
                                                handler = NULL, action = NULL,
                                                container = NULL, ...) {
  GFileBrowse$new(toolkit, text = text, type = type, initial.filename = initial.filename,
                  initial.dir = initial.dir, filter = filter, quote = quote,
                  handler = handler, action = action, container = container, ...)
}

GFileBrowse <- setRefClass(
  "GFileBrowse",
  contains = "GWidget",
  fields = list(
    entry = "ANY",
    button = "ANY",
    file_type = "character",
    initial_dir = "character",
    file_filter = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL, text = "", type = "open",
                          initial.filename = NULL, initial.dir = getwd(),
                          filter = list(), quote = TRUE, handler = NULL,
                          action = NULL, container = NULL, ...) {
      entry <<- gtkEntryNew()
      if (!is.null(initial.filename))
        gtkEditableSetText(entry, as.character(initial.filename)[1])
      button <<- gtkButtonNewWithLabel("Browse...")
      block <<- gtkBoxNew(.GtkOrientation$HORIZONTAL, 5L)
      gtkWidgetSetHexpand(entry, TRUE)
      gtkBoxAppend(block, entry)
      gtkBoxAppend(block, button)
      widget <<- entry
      initFields(
        file_type = type,
        initial_dir = as.character(initial.dir)[1],
        file_filter = filter,
        change_signal = "activate"
      )
      gSignalConnectR(button, "clicked", function(...) {
        parent_win <- tryCatch(getTopLevel(.self), error = function(e) NULL)
        path <- .gfile.guiWidgetsToolkitRgtk4(
          toolkit, text = "", type = file_type,
          initial.filename = get_value(), initial.dir = initial_dir,
          filter = file_filter, parent = parent_win
        )
        if (length(path) && nzchar(path[1])) {
          set_value(path[1])
          invoke_change_handler()
        }
      })
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(...) as.character(gtkEditableGetText(entry)),
    set_value = function(value, ...) {
      gtkEditableSetText(entry, as.character(value)[1])
    }
  )
)
