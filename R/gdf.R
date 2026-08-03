##' @include gtable.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gdf guiWidgetsToolkitRgtk4
.gdf.guiWidgetsToolkitRgtk4 <- function(toolkit, items = NULL,
                                        handler = NULL, action = NULL,
                                        container = NULL, ...) {
  GDf$new(toolkit, items = items, handler = handler, action = action,
          container = container, ...)
}

##' External-pointer address string for map keys
##' @noRd
.extptr_key <- function(x) {
  addr <- tryCatch(.Call("R_extptr_address", x, PACKAGE = "Rgtk4"),
                   error = function(e) NULL)
  if (is.null(addr))
    paste(capture.output(print(x)), collapse = "")
  else
    as.character(addr)
}

##' Minimal editable data-frame widget (ColumnView + EditableLabel)
##'
##' Implements the iNZight-facing surface: `set_frame` / `get_frame`,
##' `set_editable`, `remove_popup_menu`, `add_dnd_columns` (stub),
##' cell-change handlers. Full RGtk2 command-stack / coerce menus deferred.
##'
##' @rdname gWidgets2Rgtk4-package
GDf <- setRefClass(
  "GDf",
  contains = "GWidget",
  fields = list(
    items = "ANY",
    row_visible = "logical",
    editable_cols = "logical",
    string_list = "ANY",
    selection = "ANY",
    data_env = "ANY",
    cell_map = "ANY",
    column_objs = "list",
    col_names = "character",
    block_edit_notify = "logical"
  ),
  methods = list(
    initialize = function(toolkit = NULL, items = NULL,
                          handler = NULL, action = NULL,
                          container = NULL, ...) {
      view <- gtkColumnViewNew(NULL)
      gtkWidgetSetHexpand(view, TRUE)
      gtkWidgetSetVexpand(view, TRUE)

      scrolled <- gtkScrolledWindowNew()
      gtkScrolledWindowSetChild(scrolled, view)
      gtkScrolledWindowSetPolicy(scrolled, 1L, 1L)
      gtkWidgetSetHexpand(scrolled, TRUE)
      gtkWidgetSetVexpand(scrolled, TRUE)

      widget <<- view
      block <<- scrolled

      initFields(
        column_objs = list(),
        col_names = character(0),
        string_list = NULL,
        selection = NULL,
        data_env = new.env(parent = emptyenv()),
        cell_map = new.env(parent = emptyenv(), hash = TRUE),
        block_edit_notify = FALSE,
        change_signal = "changed",
        default_expand = TRUE,
        default_fill = TRUE
      )
      assign("self", .self, envir = data_env)

      frame <- items
      if (is.null(frame))
        frame <- data.frame(stringsAsFactors = FALSE)
      set_frame(frame)

      add_to_parent(container, .self, ...)
      if (is_handler(handler))
        handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },

    view_to_data = function(pos) {
      vis <- which(row_visible)
      if (!length(vis) || pos < 0L || pos >= length(vis))
        return(NA_integer_)
      as.integer(vis[pos + 1L])
    },
    data_to_view = function(idx) {
      m <- match(as.integer(idx), which(row_visible))
      ifelse(is.na(m), NA_integer_, as.integer(m - 1L))
    },

    clear_columns = function() {
      for (col in column_objs)
        try(gtkColumnViewRemoveColumn(widget, col), silent = TRUE)
      column_objs <<- list()
    },

    cell_text = function(row, col_idx) {
      val <- items[row, col_idx]
      if (length(val) != 1L)
        paste(as.character(val), collapse = ", ")
      else if (is.na(val))
        ""
      else
        as.character(val)
    },

    coerce_cell = function(text, col_idx) {
      old <- items[[col_idx]]
      text <- as.character(text)[1]
      if (is.null(old))
        return(text)
      if (is.logical(old)) {
        low <- tolower(text)
        if (low %in% c("t", "true", "1", "yes")) return(TRUE)
        if (low %in% c("f", "false", "0", "no")) return(FALSE)
        return(as.logical(text))
      }
      if (is.integer(old))
        return(suppressWarnings(as.integer(text)))
      if (is.numeric(old))
        return(suppressWarnings(as.numeric(text)))
      if (inherits(old, "Date"))
        return(tryCatch(as.Date(text), error = function(e) old[1]))
      if (is.factor(old)) {
        if (!text %in% levels(old))
          levels(old) <- c(levels(old), text)
        return(factor(text, levels = levels(old)))
      }
      text
    },

    commit_edit = function(row, col_idx, text) {
      if (is.na(row) || row < 1L || row > nrow(items))
        return(invisible(NULL))
      if (col_idx < 1L || col_idx > ncol(items))
        return(invisible(NULL))
      new_val <- coerce_cell(text, col_idx)
      old_val <- items[row, col_idx]
      same <- tryCatch(identical(old_val, new_val) ||
                         (is.na(old_val) && is.na(new_val)),
                       error = function(e) FALSE)
      if (isTRUE(same))
        return(invisible(NULL))
      items[row, col_idx] <<- new_val
      assign("df", items, envir = data_env)
      notify_observers(signal = change_signal,
                       i = row, j = col_idx, value = new_val)
      invisible(NULL)
    },

    make_column = function(col_idx, title, editable) {
      force(col_idx)
      force(title)
      force(editable)
      env <- data_env
      cmap <- cell_map
      factory <- gtkSignalListItemFactoryNew()

      if (isTRUE(editable)) {
        gSignalConnectR(factory, "setup", function(f, item) {
          lab <- gtkEditableLabelNew("")
          gtkListItemSetChild(item, lab)
          gSignalConnectR(lab, "notify::text", function(obj, ...) {
            host <- env$self
            if (is.null(host) || isTRUE(host$block_edit_notify))
              return()
            key <- .extptr_key(obj)
            meta <- if (exists(key, envir = cmap, inherits = FALSE))
              get(key, envir = cmap, inherits = FALSE)
            else
              NULL
            if (is.null(meta))
              return()
            txt <- tryCatch(gtkEditableGetText(obj), error = function(e) NULL)
            if (is.null(txt))
              return()
            host$commit_edit(meta$row, meta$col, txt)
          })
        })
        gSignalConnectR(factory, "bind", function(f, item) {
          lab <- gtkListItemGetChild(item)
          pos <- as.integer(gtkListItemGetPosition(item))
          obj <- env$self
          if (is.null(obj))
            return()
          row <- obj$view_to_data(pos)
          assign(.extptr_key(lab), list(row = row, col = col_idx), envir = cmap)
          obj$block_edit_notify <- TRUE
          gtkEditableSetText(lab, obj$cell_text(row, col_idx))
          obj$block_edit_notify <- FALSE
        })
      } else {
        gSignalConnectR(factory, "setup", function(f, item) {
          label <- gtkLabelNew("")
          gtkLabelSetXalign(label, 0)
          gtkListItemSetChild(item, label)
        })
        gSignalConnectR(factory, "bind", function(f, item) {
          label <- gtkListItemGetChild(item)
          pos <- as.integer(gtkListItemGetPosition(item))
          obj <- env$self
          if (is.null(obj))
            return()
          row <- obj$view_to_data(pos)
          gtkLabelSetText(label, obj$cell_text(row, col_idx))
        })
      }

      column <- gtkColumnViewColumnNew(as.character(title)[1], factory)
      gtkColumnViewColumnSetExpand(column, TRUE)
      column
    },

    make_columns = function() {
      clear_columns()
      if (is.null(items) || !ncol(items))
        return(invisible(NULL))
      nms <- col_names
      if (length(nms) != ncol(items))
        nms <- names(items)
      if (is.null(nms))
        nms <- paste0("V", seq_len(ncol(items)))
      ed <- editable_cols
      if (length(ed) != ncol(items))
        ed <- rep(TRUE, ncol(items))
      built <- list()
      for (k in seq_len(ncol(items))) {
        col <- make_column(k, nms[k], ed[k])
        gtkColumnViewAppendColumn(widget, col)
        built[[k]] <- col
      }
      column_objs <<- built
      col_names <<- as.character(nms)
      invisible(NULL)
    },

    rebuild_model = function() {
      n <- sum(row_visible)
      if (n < 1L)
        string_list <<- gtkStringListNew(NULL)
      else
        string_list <<- gtkStringListFromVector(as.character(seq_len(n)))
      selection <<- gtkSingleSelectionNew(string_list)
      ## Defaults are can_unselect=FALSE, autoselect=TRUE; clearing needs can_unselect.
      gtkSingleSelectionSetCanUnselect(selection, TRUE)
      gtkSingleSelectionSetAutoselect(selection, FALSE)
      gtkColumnViewSetModel(widget, selection)
      host <- .self
      gSignalConnectR(selection, "selection-changed", function(...) {
        host$notify_observers(signal = "selection-changed")
      })
    },

    ## --- iNZight / gWidgets API ---
    set_frame = function(value) {
      items <<- .as_items_df(value)
      n <- nrow(items)
      row_visible <<- rep(TRUE, max(0L, n))
      editable_cols <<- rep(TRUE, max(0L, ncol(items)))
      col_names <<- if (ncol(items)) names(items) else character(0)
      assign("df", items, envir = data_env)
      rebuild_model()
      make_columns()
      invisible(NULL)
    },
    get_frame = function() items,

    set_editable = function(value, j, ...) {
      value <- as.logical(value)[1]
      j <- as.integer(j)[1]
      ## j==0 is rownames in RGtk2; we have no rownames column — ignore
      if (is.na(j) || j < 1L)
        return(invisible(NULL))
      if (j > length(editable_cols))
        return(invisible(NULL))
      editable_cols[j] <<- value
      make_columns()
      invisible(NULL)
    },
    is_editable = function(j, ...) {
      j <- as.integer(j)[1]
      if (is.na(j) || j < 1L || j > length(editable_cols))
        return(FALSE)
      isTRUE(editable_cols[j])
    },
    get_editable = function(j, ...) is_editable(j, ...),

    remove_popup_menu = function() invisible(NULL),
    add_dnd_columns = function() {
      ## GTK4 DnD deferred; match RGtk2 side-effect of clearing header popups
      remove_popup_menu()
      invisible(NULL)
    },

    get_value = function(drop = TRUE, ...) {
      idx <- get_index()
      if (!length(idx))
        return(if (isTRUE(drop)) character(0) else items[FALSE, , drop = FALSE])
      items[idx, , drop = getWithDefault(drop, TRUE)]
    },
    set_value = function(value, ...) set_index(value),
    get_index = function(...) {
      if (is.null(selection))
        return(integer(0))
      pos <- as.integer(gtkSingleSelectionGetSelected(selection))
      if (length(pos) != 1L || is.na(pos) || pos < 0L)
        integer(0)
      else
        view_to_data(pos)
    },
    set_index = function(value, ...) {
      value <- as.integer(value)[1]
      if (is.na(value) || value < 1L) {
        if (!is.null(selection)) {
          gtkSingleSelectionSetCanUnselect(selection, TRUE)
          ## -1L -> guint G_MAXUINT == GTK_INVALID_LIST_POSITION
          gtkSingleSelectionSetSelected(selection, -1L)
          gtkSelectionModelUnselectAll(selection)
        }
        return(invisible(NULL))
      }
      view_pos <- data_to_view(value)
      if (is.na(view_pos))
        return(invisible(NULL))
      gtkSingleSelectionSetSelected(selection, view_pos)
      invisible(NULL)
    },

    get_items = function(i, j, ..., drop = TRUE) {
      if (missing(i) && missing(j))
        return(items)
      if (missing(i))
        items[, j, drop = getWithDefault(drop, TRUE)]
      else if (missing(j))
        items[i, , drop = getWithDefault(drop, TRUE)]
      else
        items[i, j, drop = getWithDefault(drop, TRUE)]
    },
    set_items = function(value, i, j, ...) {
      if (missing(i) && missing(j)) {
        set_frame(value)
      } else {
        if (missing(i))
          items[, j] <<- value
        else if (missing(j))
          items[i, ] <<- value
        else
          items[i, j] <<- value
        assign("df", items, envir = data_env)
        rebuild_model()
      }
      invisible(NULL)
    },

    get_length = function(...) ncol(items),
    get_dim = function(...) {
      c(rows = as.integer(sum(row_visible)), columns = as.integer(ncol(items)))
    },
    get_names = function(...) {
      if (length(col_names)) col_names else names(items)
    },
    set_names = function(value, ...) {
      value <- as.character(value)
      if (length(value) != ncol(items))
        return(invisible(NULL))
      names(items) <<- value
      col_names <<- value
      make_columns()
    },
    get_visible = function() row_visible,
    set_visible = function(value, ...) {
      n <- nrow(items)
      if (!n) {
        row_visible <<- logical(0)
        return(invisible(NULL))
      }
      row_visible <<- rep(as.logical(value), length.out = n)
      rebuild_model()
    },

    save_data = function(nm, where = .GlobalEnv) {
      assign(nm, get_frame(), envir = where)
      invisible(NULL)
    },

    add_handler_changed = function(handler, action = NULL, ...) {
      if (!is_handler(handler))
        return(invisible(NULL))
      o <- gWidgets2:::observer(.self, handler, action)
      invisible(add_observer(o, change_signal))
    },
    add_handler_selection_changed = function(handler, action = NULL, ...) {
      if (!is_handler(handler))
        return(invisible(NULL))
      o <- gWidgets2:::observer(.self, handler, action)
      invisible(add_observer(o, "selection-changed"))
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      add_handler_selection_changed(handler, action, ...)
    }
  )
)
