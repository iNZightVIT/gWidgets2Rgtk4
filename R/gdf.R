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
##' `set_editable`, column mutate helpers (`insert_column`,
##' `remove_column`, `replace_column`, `coerce_column`), header menus
##' via `SetHeaderMenu`, `add_dnd_columns` (header text drag sources),
##' cell-change handlers. Command-stack undo/redo is deferred
##' (`can_undo` / `undo` are stubs).
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
    block_edit_notify = "logical",
    dnd_columns_wanted = "logical",
    dnd_retry_id = "ANY",
    header_action_prefixes = "character",
    header_action_groups = "list",
    header_menu_fun = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL, items = NULL,
                          handler = NULL, action = NULL,
                          container = NULL, ...) {
      view <- gtkColumnViewNew(NULL)
      gtkWidgetSetHexpand(view, TRUE)
      gtkWidgetSetVexpand(view, TRUE)
      ## Built-in header reorder DnD fights column-name export drags.
      gtkColumnViewSetReorderable(view, FALSE)

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
        dnd_columns_wanted = FALSE,
        dnd_retry_id = NULL,
        header_action_prefixes = character(0),
        header_action_groups = list(),
        header_menu_fun = NULL,
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
      remove_popup_menu()
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
      add_popup(header_menu_fun)
      ## Column rebuild recreates header widgets; re-attach if enabled.
      if (isTRUE(dnd_columns_wanted)) {
        if (wire_column_dnd() < length(col_names))
          schedule_column_dnd_retry()
      }
      invisible(NULL)
    },

    refresh_after_mutate = function() {
      assign("df", items, envir = data_env)
      rebuild_model()
      make_columns()
      invisible(NULL)
    },

    insert_column = function(j, x, nm = "V") {
      "Insert column at position j (1-based). No undo stack."
      j <- as.integer(j)[1]
      nc <- ncol(items)
      nr <- nrow(items)
      if (is.na(j) || j < 1L)
        j <- 1L
      if (j > nc + 1L)
        j <- nc + 1L
      if (!nr)
        x <- x[0]
      else
        x <- rep(x, length.out = nr)
      nm <- as.character(nm)[1]
      if (!nzchar(nm))
        nm <- "V"
      new_col <- data.frame(setNames(list(x), nm), stringsAsFactors = FALSE,
                            check.names = FALSE)
      old_nms <- if (length(col_names) == nc && nc > 0L)
        col_names
      else if (nc > 0L)
        names(items)
      else
        character(0)
      old_ed <- if (length(editable_cols) == nc)
        editable_cols
      else
        rep(TRUE, nc)
      if (!nc) {
        items <<- new_col
        editable_cols <<- TRUE
        col_names <<- nm
      } else {
        before <- if (j <= 1L) NULL else items[, seq_len(j - 1L), drop = FALSE]
        after <- if (j > nc) NULL else items[, j:nc, drop = FALSE]
        items <<- cbind(before, new_col, after)
        ed_before <- if (j <= 1L) logical(0) else old_ed[seq_len(j - 1L)]
        ed_after <- if (j > nc) logical(0) else old_ed[j:nc]
        editable_cols <<- c(ed_before, TRUE, ed_after)
        nms_before <- if (j <= 1L) character(0) else old_nms[seq_len(j - 1L)]
        nms_after <- if (j > nc) character(0) else old_nms[j:nc]
        col_names <<- c(nms_before, nm, nms_after)
        names(items) <<- col_names
      }
      if (!length(row_visible) && nrow(items))
        row_visible <<- rep(TRUE, nrow(items))
      refresh_after_mutate()
      invisible(NULL)
    },

    remove_column = function(j) {
      "Remove column j (1-based). No undo stack."
      j <- as.integer(j)[1]
      nc <- ncol(items)
      if (is.na(j) || j < 1L || j > nc)
        return(invisible(NULL))
      items <<- items[, -j, drop = FALSE]
      editable_cols <<- editable_cols[-j]
      col_names <<- col_names[-j]
      if (ncol(items))
        names(items) <<- col_names
      refresh_after_mutate()
      invisible(NULL)
    },

    replace_column = function(j, x) {
      "Replace values in column j. No undo stack."
      j <- as.integer(j)[1]
      nc <- ncol(items)
      nr <- nrow(items)
      if (is.na(j) || j < 1L || j > nc)
        return(invisible(NULL))
      x <- rep(x, length.out = max(1L, nr))
      if (nr)
        x <- rep(x, length.out = nr)
      else
        x <- x[0]
      items[[j]] <<- x
      refresh_after_mutate()
      invisible(NULL)
    },

    coerce_column = function(j, coerce_with) {
      "Coerce column j with coerce_with (e.g. as.character). No undo stack."
      j <- as.integer(j)[1]
      if (is.na(j) || j < 1L || j > ncol(items))
        return(invisible(NULL))
      if (!is.function(coerce_with))
        stop("coerce_with must be a function", call. = FALSE)
      items[[j]] <<- coerce_with(items[[j]])
      refresh_after_mutate()
      invisible(NULL)
    },

    can_undo = function() {
      "Command-stack undo deferred"
      FALSE
    },
    undo = function() {
      "Command-stack undo deferred"
      invisible(NULL)
    },

    default_popup_menu = function(col_index) {
      "Header menu: rename, insert/delete, coerce, editable"
      force(col_index)
      host <- .self
      j <- as.integer(col_index)[1]
      x <- if (!is.null(items) && j >= 1L && j <= ncol(items)) items[[j]] else NULL
      nm <- if (j >= 1L && j <= length(get_names())) get_names()[j] else ""

      type_of <- function(v) {
        if (is.null(v)) return("other")
        if (is.character(v)) return("character")
        if (is.factor(v)) return("factor")
        if (is.numeric(v)) return("numeric")
        if (is.logical(v)) return("logical")
        "other"
      }
      types <- c("other", "character", "factor", "numeric", "logical")
      sel <- match(type_of(x), types)
      if (is.na(sel)) sel <- 1L

      list(
        gaction("Rename column", handler = function(h, ...) {
          out <- ginput(gettext("New column name:"), text = nm,
                        title = gettext("Rename column"), parent = host)
          if (is.character(out) && nzchar(out)) {
            nms <- host$get_names()
            if (j >= 1L && j <= length(nms)) {
              nms[j] <- out
              host$set_names(nms)
            }
          }
        }),
        gseparator(),
        gaction("Insert column...", handler = function(h, ...) {
          nr <- max(0L, nrow(host$items))
          host$insert_column(j, character(nr), "Replace me")
        }),
        gaction("Delete column", handler = function(h, ...) {
          host$remove_column(j)
        }),
        gseparator(),
        gradio(types, selected = sel, handler = function(h, ...) {
          ind <- svalue(h$obj, index = TRUE)
          if (isTRUE(ind > 1L))
            host$coerce_column(j, get(sprintf("as.%s", types[ind]), mode = "function"))
        }),
        gseparator(),
        gcheckbox("Editable", checked = host$is_editable(j),
                  handler = function(h, ...) {
                    host$set_editable(svalue(h$obj), j)
                  })
      )
    },

    add_popup_menu = function(menulist) {
      f <- function(...) menulist
      add_popup(f)
    },

    add_popup = function(menu_fun = NULL) {
      if (is.null(menu_fun))
        menu_fun <- .self$default_popup_menu
      header_menu_fun <<- menu_fun
      remove_popup_menu()
      if (!length(column_objs))
        return(invisible(NULL))
      prefixes <- character(0)
      groups <- list()
      for (k in seq_along(column_objs)) {
        prefix <- paste0("gdh", k)
        built <- build_gmenu_model(menu_fun(k), action_prefix = prefix)
        gtkColumnViewColumnSetHeaderMenu(column_objs[[k]], built$model)
        gtkWidgetInsertActionGroup(widget, prefix, built$group)
        prefixes <- c(prefixes, prefix)
        groups[[k]] <- built$group
      }
      header_action_prefixes <<- prefixes
      header_action_groups <<- groups
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

    remove_popup_menu = function() {
      for (col in column_objs)
        try(gtkColumnViewColumnSetHeaderMenu(col, NULL), silent = TRUE)
      for (prefix in header_action_prefixes)
        try(gtkWidgetInsertActionGroup(widget, prefix, NULL), silent = TRUE)
      header_action_prefixes <<- character(0)
      header_action_groups <<- list()
      invisible(NULL)
    },

    ## Attach text DragSources to ColumnView header title widgets so
    ## dragging a header exports the column name (iNZight V1/V2 boxes).
    ## Native SetHeaderMenu stays installed; after DnD strips GestureClick
    ## we restore secondary-click menus via .dnd_attach_header_menu_click.
    wire_column_dnd = function() {
      .dnd_prepare_columnview_headers_for_dnd(widget)
      titles <- .dnd_columnview_header_titles(widget)
      nms <- col_names
      if (!length(nms) && !is.null(items) && ncol(items))
        nms <- names(items)
      if (!length(titles) || !length(nms))
        return(0L)
      n <- min(length(titles), length(nms))
      for (k in seq_len(n)) {
        ## Fresh env per iteration so prepare closures don't share nm.
        local({
          nm <- as.character(nms[k])[1]
          .dnd_attach_text_source(titles[[k]], function() nm)
        })
      }
      ## Restore header menus (secondary click) after GestureClick strip
      n_menu <- min(n, length(column_objs), length(header_action_prefixes),
                    length(header_action_groups))
      for (k in seq_len(n_menu)) {
        local({
          kk <- k
          model <- tryCatch(
            gtkColumnViewColumnGetHeaderMenu(column_objs[[kk]]),
            error = function(e) NULL
          )
          .dnd_attach_header_menu_click(
            titles[[kk]], model, widget,
            header_action_prefixes[kk], header_action_groups[[kk]]
          )
        })
      }
      as.integer(n)
    },

    schedule_column_dnd_retry = function() {
      if (!is.null(dnd_retry_id))
        return(invisible(NULL))
      host <- .self
      ## Headers appear after realize/layout; one short retry is enough.
      dnd_retry_id <<- tryCatch(
        gTimeoutAdd(50L, function() {
          host$dnd_retry_id <- NULL
          if (isTRUE(host$dnd_columns_wanted))
            host$wire_column_dnd()
          FALSE
        }),
        error = function(e) NULL
      )
      invisible(NULL)
    },

    add_dnd_columns = function() {
      ## Keep / restore header menus so secondary-click works after DnD
      ## strips ColumnViewTitle GestureClick.
      dnd_columns_wanted <<- TRUE
      if (!length(header_action_groups) && length(column_objs))
        add_popup(header_menu_fun)
      n <- wire_column_dnd()
      if (n < 1L && length(col_names))
        schedule_column_dnd_retry()
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
