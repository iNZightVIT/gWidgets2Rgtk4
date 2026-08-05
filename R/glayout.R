##' @include GContainer.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .glayout guiWidgetsToolkitRgtk4
.glayout.guiWidgetsToolkitRgtk4 <- function(toolkit, homogeneous = FALSE, spacing = 10,
                                            container = NULL, ...) {
  GLayout$new(toolkit = toolkit, homogeneous = homogeneous, spacing = spacing,
              container = container, ...)
}

GLayout <- setRefClass(
  "GLayout",
  contains = "GContainer",
  fields = list(child_positions = "list", nrows = "integer", ncols = "integer"),
  methods = list(
    initialize = function(toolkit = NULL, homogeneous = FALSE, spacing = 10,
                          container = NULL, ...) {
      widget <<- gtkGridNew()
      gtkGridSetColumnHomogeneous(widget, as.logical(homogeneous))
      gtkGridSetRowHomogeneous(widget, as.logical(homogeneous))
      gtkGridSetColumnSpacing(widget, as.integer(spacing))
      gtkGridSetRowSpacing(widget, as.integer(spacing))
      initFields(block = widget, child_positions = list(), nrows = 0L, ncols = 0L)
      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },
    get_dim = function(...) c(nrow = nrows, ncol = ncols),
    get_items = function(i, j, ..., drop = TRUE) {
      d <- get_dim()
      m <- matrix(list(), nrow = max(d[1], 1), ncol = max(d[2], 1))
      for (index in seq_along(child_positions)) {
        item <- child_positions[[index]]
        for (ii in item$x)
          for (jj in item$y)
            m[[ii, jj]] <- item$child
      }
      out <- m[i, j, drop = drop]
      if (length(out) == 1 && drop)
        out <- out[[1]]
      out
    },
    set_items = function(value, i, j, expand = FALSE, fill = FALSE, anchor = NULL) {
      if (missing(j)) {
        message("glayout: [ needs to have a column specified.")
        return()
      }
      if (missing(i))
        i <- nrows + 1L
      if (is.character(value))
        value <- glabel(value, toolkit = toolkit)
      child <- getBlock(value)
      expand <- getWithDefault(expand, getWithDefault(value$default_expand, FALSE))
      fill <- getWithDefault(fill, getWithDefault(value$default_fill, "both"))
      ## Grid mode (horizontal = NA): per-axis expand like gtk_table Attach
      set_child_expand_fill_anchor(child, expand = expand, fill = fill,
                                   anchor = anchor, horizontal = NA)
      ## GtkGrid is 0-based
      left <- as.integer(min(j) - 1L)
      top <- as.integer(min(i) - 1L)
      width <- as.integer(length(j))
      height <- as.integer(length(i))
      gtkGridAttach(widget, child, left, top, width, height)
      nrows <<- as.integer(max(nrows, max(i)))
      ncols <<- as.integer(max(ncols, max(j)))
      child_positions <<- c(child_positions, list(list(x = i, y = j, child = value)))
      child_bookkeeping(value)
    }
  )
)
