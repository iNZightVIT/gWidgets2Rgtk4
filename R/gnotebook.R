##' @include GContainer.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gnotebook guiWidgetsToolkitRgtk4
.gnotebook.guiWidgetsToolkitRgtk4 <- function(toolkit, tab.pos = 3, container = NULL, ...) {
  GNotebook$new(toolkit, tab.pos, container = container, ...)
}

## tab.pos: 1=bottom, 2=left, 3=top, 4=right → GTK PositionType LEFT=0 RIGHT=1 TOP=2 BOTTOM=3
.tab_pos_to_gtk <- function(tab.pos) {
  c(3L, 0L, 2L, 1L)[as.integer(tab.pos)[1]]
}

GNotebook <- setRefClass(
  "GNotebook",
  contains = "GContainer",
  fields = list(page_labels = "character"),
  methods = list(
    initialize = function(toolkit = NULL, tab.pos = 3, container = NULL, ...) {
      if (is(widget, "uninitializedField"))
        make_widget(tab.pos)
      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },
    make_widget = function(tab.pos) {
      widget <<- gtkNotebookNew()
      ## Scroll arrows also get DropControllerMotion and cycle pages during
      ## any drag; keep scrollable but strip those controllers after pages add.
      gtkNotebookSetScrollable(widget, TRUE)
      gtkNotebookSetTabPos(widget, .tab_pos_to_gtk(tab.pos))
      initFields(block = widget, page_labels = character(0),
                 change_signal = "switch-page")
      ## Arrows may appear on map/allocate — strip again then.
      tryCatch(
        gSignalConnectR(widget, "map", function(w) {
          .dnd_notebook_disable_tab_hover_switch(w)
          NULL
        }),
        error = function(e) invisible(NULL)
      )
    },
    get_value = function(...) as.integer(gtkNotebookGetCurrentPage(widget)) + 1L,
    set_value = function(value, ...) {
      n <- as.integer(gtkNotebookGetNPages(widget))[1]
      page <- as.integer(value)[1]
      if (is.na(page) || is.na(n) || n < 1L)
        return(invisible(NULL))
      gtkNotebookSetCurrentPage(widget, max(0L, min(n, page) - 1L))
    },
    get_index = function(...) get_value(),
    set_index = function(value, ...) set_value(value),
    get_names = function(...) page_labels,
    set_names = function(value, ...) {
      page_labels <<- as.character(value)
      for (i in seq_along(page_labels)) {
        page <- gtkNotebookGetNthPage(widget, as.integer(i - 1L))
        if (!is.null(page))
          gtkNotebookSetTabLabelText(widget, page, page_labels[i])
      }
    },
    get_items = function(i, j, ..., drop = TRUE) {
      items <- children[i]
      if (drop && length(items) == 1) items[[1]] else items
    },
    get_length = function(...) as.integer(gtkNotebookGetNPages(widget)),
    ## Signature must match gWidgets2::add.GNotebook positional call:
    ##   obj$add_child(child, label, i, close.button, ...)
    make_label = function(child, label, close.button = FALSE, ...) {
      lab_txt <- as.character(label)[1]
      if (is.na(lab_txt)) lab_txt <- ""
      if (!isTRUE(as.logical(close.button)[1]))
        return(gtkLabelNew(lab_txt))
      box <- gtkBoxNew(.GtkOrientation$HORIZONTAL, 4L)
      gtkBoxAppend(box, gtkLabelNew(lab_txt))
      btn <- gtkButtonNew()
      gtkButtonSetIconName(btn, "window-close")
      try(gtkWidgetSetFocusOnClick(btn, FALSE), silent = TRUE)
      force(child)
      gSignalConnectR(btn, "clicked", function(...) {
        .self$remove_child(child)
      })
      gtkBoxAppend(box, btn)
      box
    },
    add_child = function(child, label = "", index = NULL, close.button = FALSE, ...) {
      label_widget <- make_label(child, label, close.button, ...)
      child_block <- getBlock(child)
      lab_chr <- as.character(label)[1]
      if (is.na(lab_chr)) lab_chr <- ""

      ## gWidgets2 may pass index=0 for an empty notebook (length == 0)
      if (is.null(index) || (length(index) == 1L && is.na(index))) {
        gtkNotebookAppendPage(widget, child_block, label_widget)
        page_labels <<- c(page_labels, lab_chr)
      } else {
        idx <- suppressWarnings(as.integer(index)[1])
        if (is.na(idx) || idx < 1L) {
          gtkNotebookPrependPage(widget, child_block, label_widget)
          page_labels <<- c(lab_chr, page_labels)
        } else {
          gtkNotebookInsertPage(widget, child_block, label_widget, as.integer(idx - 1L))
          page_labels <<- append(page_labels, lab_chr, after = idx - 1L)
        }
      }
      set_value(get_length())
      child_bookkeeping(child)
      ## GTK switches notebook pages when a drag hovers a tab; disable that
      ## so in-widget DnD does not flip tabs under the pointer.
      .dnd_notebook_disable_tab_hover_switch(widget)
    },
    remove_child = function(child) {
      for (i in seq_along(children)) {
        if (identical(children[[i]], child)) {
          tryCatch(gtkNotebookRemovePage(widget, as.integer(i - 1L)),
                   error = function(e) invisible(NULL))
          page_labels <<- page_labels[-i]
          children <<- children[-i]
          try(child$set_parent(NULL), silent = TRUE)
          break
        }
      }
    },
    dispose_child = function(child) {
      remove_child(child)
    },
    remove_current_page = function(...) {
      n <- get_length()
      if (is.na(n) || n < 1L)
        return(invisible(NULL))
      i <- get_value()
      if (is.na(i) || i < 1L || i > length(children))
        return(invisible(NULL))
      child <- children[[i]]
      remove_child(child)
      invisible(NULL)
    }
  )
)
