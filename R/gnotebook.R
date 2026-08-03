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
      n <- gtkNotebookGetNPages(widget)
      gtkNotebookSetCurrentPage(widget, min(n, as.integer(value)) - 1L)
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
    add_child = function(child, label = "", close.button = FALSE, index = NULL, ...) {
      lab <- gtkLabelNew(as.character(label)[1])
      child_block <- getBlock(child)
      if (is.null(index)) {
        gtkNotebookAppendPage(widget, child_block, lab)
        page_labels <<- c(page_labels, as.character(label)[1])
      } else if (as.integer(index) < 1) {
        gtkNotebookPrependPage(widget, child_block, lab)
        page_labels <<- c(as.character(label)[1], page_labels)
      } else {
        gtkNotebookInsertPage(widget, child_block, lab, as.integer(index) - 1L)
        idx <- as.integer(index)
        page_labels <<- append(page_labels, as.character(label)[1], after = idx - 1L)
      }
      set_value(get_length())
      child_bookkeeping(child)
      ## GTK switches notebook pages when a drag hovers a tab; disable that
      ## so in-widget DnD does not flip tabs under the pointer.
      .dnd_notebook_disable_tab_hover_switch(widget)
    },
    dispose_child = function(child) {
      ## remove page containing child
      for (i in seq_along(children)) {
        if (identical(children[[i]], child)) {
          gtkNotebookRemovePage(widget, as.integer(i - 1L))
          page_labels <<- page_labels[-i]
          children <<- children[-i]
          break
        }
      }
    }
  )
)
