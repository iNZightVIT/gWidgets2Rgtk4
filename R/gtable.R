##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gtable guiWidgetsToolkitRgtk4
.gtable.guiWidgetsToolkitRgtk4 <- function(toolkit,
                                           items,
                                           multiple = FALSE,
                                           chosen.col = 1,
                                           icon.col = NULL,
                                           tooltip.col = NULL,
                                           handler = NULL, action = NULL,
                                           container = NULL, ...) {
  GTable$new(toolkit,
             items = items,
             multiple = multiple,
             chosen.col = chosen.col,
             icon.col = icon.col,
             tooltip.col = tooltip.col,
             handler = handler, action = action,
             container = container, ...)
}

##' Coerce value to a data.frame for gtable/gdf
##' @noRd
.as_items_df <- function(value) {
  if (is.null(value))
    return(data.frame(stringsAsFactors = FALSE))
  if (is.data.frame(value))
    return(value)
  if (is.vector(value))
    return(data.frame(Values = value, stringsAsFactors = FALSE))
  if (is.matrix(value))
    return(data.frame(value, stringsAsFactors = FALSE))
  as.data.frame(value, stringsAsFactors = FALSE)
}

##' Selection table widget (GtkColumnView)
##' @rdname gWidgets2Rgtk4-package
GTable <- setRefClass(
  "GTable",
  contains = "GWidget",
  fields = list(
    items = "ANY",
    row_visible = "logical",
    chosen_col = "integer",
    icon_col = "ANY",
    tooltip_col = "ANY",
    multiple = "logical",
    string_list = "ANY",
    selection = "ANY",
    data_env = "ANY",
    column_objs = "list",
    col_names = "character"
  ),
  methods = list(
    initialize = function(toolkit = NULL,
                          items = NULL,
                          multiple = FALSE,
                          chosen.col = 1,
                          icon.col = NULL,
                          tooltip.col = NULL,
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

      if (is.character(icon.col) && !is.null(items))
        icon.col <- match(icon.col, names(items))
      if (is.numeric(icon.col))
        icon.col <- as.integer(icon.col)
      if (is.character(tooltip.col) && !is.null(items))
        tooltip.col <- match(tooltip.col, names(items))
      if (is.numeric(tooltip.col))
        tooltip.col <- as.integer(tooltip.col)

      initFields(
        multiple = isTRUE(multiple),
        chosen_col = as.integer(chosen.col)[1],
        icon_col = icon.col,
        tooltip_col = tooltip.col,
        column_objs = list(),
        col_names = character(0),
        string_list = NULL,
        selection = NULL,
        data_env = new.env(parent = emptyenv()),
        change_signal = "selection-changed",
        default_expand = TRUE,
        default_fill = TRUE
      )
      assign("df", NULL, envir = data_env)
      assign("self", .self, envir = data_env)

      set_items(items)
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },

    ## Columns shown in the view (exclude icon/tooltip helper cols)
    display_columns = function() {
      if (is.null(items) || !ncol(items))
        return(integer(0))
      j <- seq_len(ncol(items))
      skip <- unique(na.omit(c(icon_col, tooltip_col)))
      setdiff(j, skip)
    },

    ## Map view row (0-based) -> data row (1-based)
    view_to_data = function(pos) {
      vis <- which(row_visible)
      if (!length(vis) || pos < 0L || pos >= length(vis))
        return(NA_integer_)
      as.integer(vis[pos + 1L])
    },

    ## Map data row (1-based) -> view row (0-based) or NA
    data_to_view = function(idx) {
      m <- match(as.integer(idx), which(row_visible))
      ifelse(is.na(m), NA_integer_, as.integer(m - 1L))
    },

    clear_columns = function() {
      cols <- column_objs
      for (col in cols)
        try(gtkColumnViewRemoveColumn(widget, col), silent = TRUE)
      column_objs <<- list()
    },

    make_column = function(col_idx, title) {
      force(col_idx)
      force(title)
      env <- data_env
      factory <- gtkSignalListItemFactoryNew()
      gSignalConnectR(factory, "setup", function(f, item) {
        label <- gtkLabelNew("")
        gtkLabelSetXalign(label, 0)
        gtkListItemSetChild(item, label)
      })
      gSignalConnectR(factory, "bind", function(f, item) {
        label <- gtkListItemGetChild(item)
        pos <- as.integer(gtkListItemGetPosition(item))
        df <- env$df
        self <- env$self
        if (is.null(df) || is.null(self))
          return()
        row <- self$view_to_data(pos)
        if (is.na(row) || row < 1L || row > nrow(df)) {
          gtkLabelSetText(label, "")
          return()
        }
        val <- df[row, col_idx]
        gtkLabelSetText(label, paste(as.character(val), collapse = ", "))
      })
      column <- gtkColumnViewColumnNew(as.character(title)[1], factory)
      gtkColumnViewColumnSetExpand(column, TRUE)
      column
    },

    make_columns = function() {
      clear_columns()
      cols <- display_columns()
      nms <- col_names
      if (!length(nms))
        nms <- names(items)[cols]
      if (length(nms) != length(cols))
        nms <- paste0("V", cols)
      built <- list()
      for (k in seq_along(cols)) {
        col <- make_column(cols[k], nms[k])
        gtkColumnViewAppendColumn(widget, col)
        built[[k]] <- col
      }
      column_objs <<- built
      col_names <<- as.character(nms)
    },

    rebuild_model = function() {
      n <- sum(row_visible)
      if (n < 1L) {
        string_list <<- gtkStringListNew(NULL)
      } else {
        string_list <<- gtkStringListFromVector(as.character(seq_len(n)))
      }
      if (isTRUE(multiple))
        selection <<- gtkMultiSelectionNew(string_list)
      else {
        selection <<- gtkSingleSelectionNew(string_list)
        ## Required for clearing selection (UnselectAll / INVALID position).
        ## Defaults are can_unselect=FALSE, autoselect=TRUE.
        configure_single_selection(selection)
      }
      gtkColumnViewSetModel(widget, selection)
      connect_selection_signal()
    },

    configure_single_selection = function(sel) {
      gtkSingleSelectionSetCanUnselect(sel, TRUE)
      gtkSingleSelectionSetAutoselect(sel, FALSE)
      invisible(sel)
    },

    clear_selection = function() {
      "Clear all selected rows (single or multi)"
      if (is.null(selection))
        return(invisible(NULL))
      if (isTRUE(multiple)) {
        gtkSelectionModelUnselectAll(selection)
      } else {
        ## Ensure can_unselect; without it UnselectAll/SetSelected(INVALID) are no-ops.
        configure_single_selection(selection)
        ## -1L casts to guint G_MAXUINT == GTK_INVALID_LIST_POSITION
        gtkSingleSelectionSetSelected(selection, -1L)
        gtkSelectionModelUnselectAll(selection)
      }
      invisible(NULL)
    },

    connect_selection_signal = function() {
      self <- .self
      gSignalConnectR(selection, "selection-changed", function(...) {
        self$notify_observers(signal = "selection-changed")
      })
    },

    get_selected_view = function() {
      "0-based view positions currently selected"
      if (is.null(selection))
        return(integer(0))
      if (isTRUE(multiple)) {
        n <- as.integer(gListModelGetNItems(string_list))
        if (is.na(n) || n < 1L)
          return(integer(0))
        which(vapply(seq_len(n) - 1L, function(i) {
          isTRUE(as.logical(gtkSelectionModelIsSelected(selection, as.integer(i))))
        }, logical(1))) - 1L
      } else {
        pos <- as.integer(gtkSingleSelectionGetSelected(selection))
        if (length(pos) != 1L || is.na(pos) || pos < 0L)
          integer(0)
        else
          pos
      }
    },

    set_selected_view = function(positions) {
      "Set selection from 0-based view positions"
      if (is.null(selection))
        return(invisible(NULL))
      positions <- as.integer(positions)
      positions <- positions[!is.na(positions) & positions >= 0L]
      block_handlers()
      on.exit(unblock_handlers())
      clear_selection()
      if (!length(positions))
        return(invisible(NULL))
      if (isTRUE(multiple)) {
        for (p in positions)
          gtkSelectionModelSelectItem(selection, p, FALSE)
      } else {
        gtkSingleSelectionSetSelected(selection, positions[1])
      }
      invisible(NULL)
    },

    ## --- gWidgets API ---
    get_value = function(drop = TRUE, ...) {
      idx <- get_index()
      if (!length(idx))
        return(if (isTRUE(drop)) character(0) else items[FALSE, , drop = FALSE])
      vals <- items[idx, , drop = FALSE]
      if (isTRUE(getWithDefault(drop, TRUE)))
        vals[, chosen_col, drop = TRUE]
      else
        vals
    },
    set_value = function(value, ...) {
      vals <- get_items(drop = TRUE)
      if (is.numeric(value) && !is.numeric(vals))
        ind <- as.integer(value)
      else
        ind <- match(value, vals)
      ind <- ind[!is.na(ind)]
      if (!length(ind))
        return(invisible(NULL))
      set_index(ind)
    },
    get_index = function(...) {
      view_pos <- get_selected_view()
      if (!length(view_pos))
        return(integer(0))
      as.integer(vapply(view_pos, view_to_data, integer(1)))
    },
    set_index = function(value, ...) {
      value <- as.integer(value)
      if (!length(value) || (length(value) == 1L && value < 1L)) {
        set_selected_view(integer(0))
        return(invisible(NULL))
      }
      view_pos <- data_to_view(value)
      view_pos <- view_pos[!is.na(view_pos)]
      set_selected_view(view_pos)
      invisible(NULL)
    },
    get_items = function(i, j, ..., drop = TRUE) {
      df <- items[, display_columns(), drop = FALSE]
      names(df) <- get_names()
      if (missing(i) && missing(j))
        return(df)
      if (missing(i))
        df[, j, drop = getWithDefault(drop, TRUE)]
      else if (missing(j))
        df[i, , drop = getWithDefault(drop, TRUE)]
      else
        df[i, j, drop = getWithDefault(drop, TRUE)]
    },
    set_items = function(value, i, j, ...) {
      block_handlers()
      on.exit(unblock_handlers())
      if (missing(i) && missing(j)) {
        items <<- .as_items_df(value)
        row_visible <<- rep(TRUE, max(0L, nrow(items)))
        assign("df", items, envir = data_env)
        col_names <<- names(items)[display_columns()]
        rebuild_model()
        make_columns()
      } else {
        if (missing(i))
          items[, j] <<- value
        else if (missing(j))
          items[i, ] <<- value
        else
          items[i, j] <<- value
        assign("df", items, envir = data_env)
        ## Force visible rows to rebind
        rebuild_model()
      }
      invisible(NULL)
    },
    get_length = function(...) length(display_columns()),
    get_dim = function(...) {
      c(rows = as.integer(sum(row_visible)), columns = length(display_columns()))
    },
    get_names = function(...) {
      if (length(col_names))
        col_names
      else
        names(items)[display_columns()]
    },
    set_names = function(value, ...) {
      value <- as.character(value)
      if (length(value) != length(display_columns()))
        return(invisible(NULL))
      col_names <<- value
      ## Update column titles by rebuilding columns (keeps model)
      make_columns()
    },
    get_visible = function() {
      if (!length(row_visible))
        logical(0)
      else
        row_visible
    },
    set_visible = function(value, ...) {
      ## Row filter (gWidgets2 semantics), not widget visibility
      n <- nrow(items)
      if (!n) {
        row_visible <<- logical(0)
        return(invisible(NULL))
      }
      row_visible <<- rep(as.logical(value), length.out = n)
      rebuild_model()
      invisible(NULL)
    },
    remove_popup_menu = function() {
      ## Header popups not installed by default in this backend
      invisible(NULL)
    },

    ## Handlers
    add_handler_changed = function(handler, action = NULL, ...) {
      ## Double-click / activate
      add_handler_activate(handler, action, ...)
    },
    add_handler_selection_changed = function(handler, action = NULL, ...) {
      if (!is_handler(handler))
        return(invisible(NULL))
      o <- gWidgets2:::observer(.self, handler, action)
      invisible(add_observer(o, "selection-changed"))
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      add_handler_selection_changed(handler, action, ...)
    },
    add_handler_double_clicked = function(handler, action = NULL, ...) {
      add_handler_activate(handler, action, ...)
    },
    add_handler_activate = function(handler, action = NULL, ...) {
      if (!is_handler(handler))
        return(invisible(NULL))
      o <- gWidgets2:::observer(.self, handler, action)
      add_observer(o, "activate")
      self <- .self
      gSignalConnectR(widget, "activate", function(v, position) {
        self$notify_observers(signal = "activate", position = position)
      })
      invisible(NULL)
    }
  )
)
