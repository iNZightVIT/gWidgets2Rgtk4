##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gtree guiWidgetsToolkitRgtk4
.gtree.guiWidgetsToolkitRgtk4 <- function(toolkit,
                                          offspring = NULL, offspring.data = NULL,
                                          chosen.col = 1, offspring.col = 2,
                                          icon.col = NULL, tooltip.col = NULL,
                                          multiple = FALSE,
                                          handler = NULL, action = NULL,
                                          container = NULL, ...) {
  GTree$new(toolkit,
            offspring = offspring, offspring.data = offspring.data,
            chosen.col = chosen.col, offspring.col = offspring.col,
            icon.col = icon.col, tooltip.col = tooltip.col,
            multiple = multiple,
            handler = handler, action = action, container = container, ...)
}

##' Coerce column name/index to integer column index (or NULL)
##' @noRd
.tree_col_index <- function(val, items) {
  if (is.null(val))
    return(NULL)
  if (is.character(val)) {
    if (!is.element(val, names(items)))
      return(NULL)
    val <- match(val, names(items))
  }
  if (is.numeric(val))
    as.integer(val)
  else
    NULL
}

##' Hierarchical tree widget (GtkColumnView + GtkTreeListModel)
##'
##' Offspring rows are loaded lazily via \code{offspring(path, offspring.data)}.
##' Node state lives in an R environment keyed by string ids (GtkStringList).
##'
##' @rdname gWidgets2Rgtk4-package
GTree <- setRefClass(
  "GTree",
  contains = "GWidget",
  fields = list(
    offspring_fun = "ANY",
    offspring_data = "ANY",
    chosen_col = "ANY",
    offspring_col = "ANY",
    icon_col = "ANY",
    tooltip_col = "ANY",
    multiple = "logical",
    nodes = "ANY",
    next_id = "integer",
    root_list = "ANY",
    tree_model = "ANY",
    selection = "ANY",
    column_objs = "list",
    col_names = "character",
    display_cols = "integer",
    children_cache = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = NULL,
                          offspring = NULL, offspring.data = NULL,
                          chosen.col = 1, offspring.col = 2,
                          icon.col = NULL, tooltip.col = NULL,
                          multiple = FALSE,
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

      items0 <- offspring(c(), offspring.data)
      if (!is.data.frame(items0))
        items0 <- as.data.frame(items0, stringsAsFactors = FALSE)

      chosen.col <- .tree_col_index(chosen.col, items0)
      offspring.col <- .tree_col_index(offspring.col, items0)
      icon.col <- .tree_col_index(icon.col, items0)
      tooltip.col <- .tree_col_index(tooltip.col, items0)
      if (is.null(chosen.col))
        chosen.col <- 1L
      if (is.null(offspring.col))
        offspring.col <- 2L

      skip <- unique(na.omit(c(chosen.col, offspring.col, icon.col, tooltip.col)))
      disp <- setdiff(seq_len(ncol(items0)), skip)

      initFields(
        offspring_fun = offspring,
        offspring_data = offspring.data,
        chosen_col = chosen.col,
        offspring_col = offspring.col,
        icon_col = icon.col,
        tooltip_col = tooltip.col,
        multiple = isTRUE(multiple),
        nodes = new.env(parent = emptyenv()),
        next_id = 1L,
        children_cache = new.env(parent = emptyenv()),
        root_list = NULL,
        tree_model = NULL,
        selection = NULL,
        column_objs = list(),
        display_cols = as.integer(disp),
        col_names = character(0),
        change_signal = "selection-changed",
        default_expand = TRUE,
        default_fill = TRUE
      )

      ## Visible names: chosen key first, then extra display columns
      nms <- names(items0)
      shown <- c(chosen_col, disp)
      col_names <<- as.character(nms[shown])

      rebuild_tree()
      make_columns()
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },

    ## --- node registry -------------------------------------------------
    alloc_id = function() {
      id <- sprintf("n%d", next_id)
      next_id <<- next_id + 1L
      id
    },

    register_rows = function(df, parent_path = character(0)) {
      "Register offspring rows; return character vector of node ids"
      if (is.null(df) || !nrow(df))
        return(character(0))
      ids <- character(nrow(df))
      for (i in seq_len(nrow(df))) {
        id <- alloc_id()
        key <- as.character(df[i, chosen_col])[1]
        path <- c(parent_path, key)
        has_off <- FALSE
        if (!is.null(offspring_col) && offspring_col <= ncol(df))
          has_off <- isTRUE(as.logical(df[i, offspring_col])[1])
        row_vals <- as.list(df[i, , drop = FALSE])
        assign(id, list(
          path = path,
          key = key,
          has_offspring = has_off,
          row = row_vals,
          sibling_index = as.integer(i)
        ), envir = nodes)
        ids[i] <- id
      }
      ids
    },

    get_node = function(id) {
      if (!nzchar(id) || !exists(id, envir = nodes, inherits = FALSE))
        return(NULL)
      get(id, envir = nodes, inherits = FALSE)
    },

    ## --- model ---------------------------------------------------------
    create_children = function(item) {
      "GtkTreeListModel create_func: item is GtkStringObject (node id)"
      id <- tryCatch(gtkStringObjectGetString(item), error = function(e) NULL)
      if (is.null(id) || !nzchar(id))
        return(NULL)
      id <- as.character(id)[1]
      node <- get_node(id)
      if (is.null(node) || !isTRUE(node$has_offspring))
        return(NULL)
      ## Cache child *ids* (not GListModels): create_func is transfer-full and
      ## may be called more than once for the same item.
      child_ids <- NULL
      if (exists(id, envir = children_cache, inherits = FALSE))
        child_ids <- get(id, envir = children_cache, inherits = FALSE)
      if (is.null(child_ids)) {
        df <- offspring_fun(node$path, offspring_data)
        if (!is.data.frame(df))
          df <- as.data.frame(df, stringsAsFactors = FALSE)
        child_ids <- register_rows(df, parent_path = node$path)
        assign(id, child_ids, envir = children_cache)
      }
      if (!length(child_ids))
        return(NULL)
      gtkStringListFromVector(child_ids)
    },

    rebuild_tree = function() {
      "Rebuild root + TreeListModel from offspring(c(), ...)"
      nodes <<- new.env(parent = emptyenv())
      children_cache <<- new.env(parent = emptyenv())
      next_id <<- 1L

      df <- offspring_fun(c(), offspring_data)
      if (!is.data.frame(df))
        df <- as.data.frame(df, stringsAsFactors = FALSE)
      root_ids <- register_rows(df, parent_path = character(0))
      if (!length(root_ids)) {
        ## GtkStringListFromVector requires non-empty; use placeholder then clear
        root_list <<- gtkStringListNew(NULL)
      } else {
        root_list <<- gtkStringListFromVector(root_ids)
      }

      self <- .self
      tree_model <<- gtkTreeListModelNew(
        root_list,
        FALSE, ## passthrough: list items are TreeListRow
        FALSE, ## autoexpand
        function(item) self$create_children(item)
      )

      if (isTRUE(multiple))
        selection <<- gtkMultiSelectionNew(tree_model)
      else {
        selection <<- gtkSingleSelectionNew(tree_model)
        gtkSingleSelectionSetCanUnselect(selection, TRUE)
        gtkSingleSelectionSetAutoselect(selection, FALSE)
      }
      gtkColumnViewSetModel(widget, selection)
      connect_selection_signal()
      clear_selection()
    },

    connect_selection_signal = function() {
      self <- .self
      gSignalConnectR(selection, "selection-changed", function(...) {
        self$notify_observers(signal = "selection-changed")
      })
    },

    ## --- columns -------------------------------------------------------
    clear_columns = function() {
      for (col in column_objs)
        try(gtkColumnViewRemoveColumn(widget, col), silent = TRUE)
      column_objs <<- list()
    },

    make_key_column = function(title) {
      host <- .self
      factory <- gtkSignalListItemFactoryNew()
      gSignalConnectR(factory, "setup", function(f, list_item) {
        expander <- gtkTreeExpanderNew()
        box <- gtkBoxNew(0L, 4L) ## horizontal
        icon <- gtkImageNew()
        label <- gtkLabelNew("")
        gtkLabelSetXalign(label, 0)
        gtkBoxAppend(box, icon)
        gtkBoxAppend(box, label)
        gtkTreeExpanderSetChild(expander, box)
        gtkListItemSetChild(list_item, expander)
      })
      gSignalConnectR(factory, "bind", function(f, list_item) {
        expander <- gtkListItemGetChild(list_item)
        row <- gtkListItemGetItem(list_item)
        if (is.null(row))
          return()
        gtkTreeExpanderSetListRow(expander, row)
        box <- gtkTreeExpanderGetChild(expander)
        ## box children: image, label
        icon <- gtkWidgetGetFirstChild(box)
        label <- gtkWidgetGetNextSibling(icon)
        item <- gtkTreeListRowGetItem(row)
        id <- tryCatch(gtkStringObjectGetString(item), error = function(e) "")
        node <- host$get_node(id)
        txt <- if (is.null(node)) id else node$key
        gtkLabelSetText(label, as.character(txt)[1])
        ## Optional icon
        icol <- host$icon_col
        if (!is.null(icol) && !is.null(node)) {
          stock <- as.character(node$row[[icol]])[1]
          iname <- tryCatch(
            .getStockIconByName(host$toolkit, stock),
            error = function(e) stock
          )
          if (length(iname) && nzchar(iname[1])) {
            gtkImageSetFromIconName(icon, iname[1])
            gtkWidgetSetVisible(icon, TRUE)
          } else {
            gtkWidgetSetVisible(icon, FALSE)
          }
        } else {
          gtkWidgetSetVisible(icon, FALSE)
        }
        ## Tooltip
        tcol <- host$tooltip_col
        if (!is.null(tcol) && !is.null(node)) {
          tip <- as.character(node$row[[tcol]])[1]
          if (length(tip) && !is.na(tip))
            gtkWidgetSetTooltipText(expander, tip)
        }
      })
      column <- gtkColumnViewColumnNew(as.character(title)[1], factory)
      gtkColumnViewColumnSetExpand(column, TRUE)
      column
    },

    make_data_column = function(col_idx, title) {
      force(col_idx)
      host <- .self
      factory <- gtkSignalListItemFactoryNew()
      gSignalConnectR(factory, "setup", function(f, list_item) {
        label <- gtkLabelNew("")
        gtkLabelSetXalign(label, 0)
        gtkListItemSetChild(list_item, label)
      })
      gSignalConnectR(factory, "bind", function(f, list_item) {
        label <- gtkListItemGetChild(list_item)
        row <- gtkListItemGetItem(list_item)
        if (is.null(row)) {
          gtkLabelSetText(label, "")
          return()
        }
        item <- gtkTreeListRowGetItem(row)
        id <- tryCatch(gtkStringObjectGetString(item), error = function(e) "")
        node <- host$get_node(id)
        if (is.null(node)) {
          gtkLabelSetText(label, "")
          return()
        }
        val <- node$row[[col_idx]]
        gtkLabelSetText(label, paste(as.character(val), collapse = ", "))
      })
      column <- gtkColumnViewColumnNew(as.character(title)[1], factory)
      gtkColumnViewColumnSetExpand(column, TRUE)
      column
    },

    make_columns = function() {
      clear_columns()
      built <- list()
      ## Key / expander column
      key_title <- if (length(col_names)) col_names[1] else "Key"
      built[[1]] <- make_key_column(key_title)
      gtkColumnViewAppendColumn(widget, built[[1]])
      ## Extra display columns
      if (length(display_cols)) {
        for (k in seq_along(display_cols)) {
          title <- if (length(col_names) >= k + 1L) col_names[k + 1L]
                   else paste0("V", display_cols[k])
          col <- make_data_column(display_cols[k], title)
          gtkColumnViewAppendColumn(widget, col)
          built[[length(built) + 1L]] <- col
        }
      }
      column_objs <<- built
    },

    ## --- selection helpers ---------------------------------------------
    clear_selection = function() {
      if (is.null(selection))
        return(invisible(NULL))
      if (isTRUE(multiple)) {
        gtkSelectionModelUnselectAll(selection)
      } else {
        gtkSingleSelectionSetCanUnselect(selection, TRUE)
        gtkSingleSelectionSetAutoselect(selection, FALSE)
        gtkSingleSelectionSetSelected(selection, -1L)
        gtkSelectionModelUnselectAll(selection)
      }
      invisible(NULL)
    },

    get_selected_positions = function() {
      "0-based positions in the flattened TreeListModel"
      if (is.null(selection) || is.null(tree_model))
        return(integer(0))
      if (isTRUE(multiple)) {
        n <- as.integer(gListModelGetNItems(tree_model))
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

    position_to_id = function(pos) {
      row <- gtkTreeListModelGetRow(tree_model, as.integer(pos))
      if (is.null(row))
        return(NA_character_)
      item <- gtkTreeListRowGetItem(row)
      id <- tryCatch(gtkStringObjectGetString(item), error = function(e) NA_character_)
      if (length(id) != 1L || is.na(id))
        NA_character_
      else
        as.character(id)[1]
    },

    get_selected_ids = function() {
      pos <- get_selected_positions()
      if (!length(pos))
        return(character(0))
      ids <- vapply(pos, function(p) position_to_id(p), character(1))
      ids[!is.na(ids)]
    },

    ## Integer path among siblings (1-based), matching RGtk2 walk
    id_to_index_path = function(id) {
      node <- get_node(id)
      if (is.null(node))
        return(integer(0))
      idxs <- integer(0)
      cur_path <- node$path
      while (length(cur_path)) {
        ## Find node with this exact path
        found <- NULL
        for (cand in ls(envir = nodes, all.names = TRUE)) {
          nd <- get_node(cand)
          if (!is.null(nd) && identical(as.character(nd$path), as.character(cur_path))) {
            found <- nd
            break
          }
        }
        if (is.null(found))
          break
        idxs <- c(as.integer(found$sibling_index), idxs)
        cur_path <- cur_path[-length(cur_path)]
      }
      as.integer(idxs)
    },

    ## --- gWidgets API --------------------------------------------------
    get_value = function(drop = TRUE, ...) {
      "Return path(s) by chosen key; drop=TRUE => tip only; drop=NULL => full path"
      ids <- get_selected_ids()
      if (!length(ids))
        return(character(0))
      out <- lapply(ids, function(id) {
        node <- get_node(id)
        if (is.null(node)) character(0) else as.character(node$path)
      })
      ## gWidgets2 NextMethod often passes drop=NULL; treat NULL like FALSE (full path)
      if (isTRUE(drop))
        out <- lapply(out, utils::tail, n = 1L)
      if (length(out) == 1L)
        out[[1]]
      else
        out
    },

    set_value = function(value, ...) {
      "Select by key path (character). character(0) clears."
      if (!length(value)) {
        clear_selection()
        return(invisible(NULL))
      }
      ## Find node whose path matches value (as full path or tip)
      value <- as.character(value)
      match_id <- NULL
      for (id in ls(envir = nodes, all.names = TRUE)) {
        node <- get_node(id)
        if (is.null(node)) next
        if (identical(node$path, value) ||
            (length(value) == 1L && identical(utils::tail(node$path, 1L), value))) {
          match_id <- id
          break
        }
      }
      if (is.null(match_id)) {
        ## May need to expand ancestors first — walk path via offspring keys
        expand_and_select_path(value)
      } else {
        select_id(match_id)
      }
      invisible(NULL)
    },

    get_index = function(...) {
      "Integer vector path (1-based sibling positions)"
      ids <- get_selected_ids()
      if (!length(ids))
        return(integer(0))
      out <- lapply(ids, function(id) id_to_index_path(id))
      if (length(out) == 1L)
        out[[1]]
      else
        out
    },

    set_index = function(value, ...) {
      "Select by integer path; empty clears"
      if (!length(value) || (is.character(value) && !nzchar(paste(value, collapse = "")))) {
        clear_selection()
        return(invisible(NULL))
      }
      if (!is.list(value))
        value <- list(as.integer(value))
      else
        value <- lapply(value, as.integer)
      clear_selection()
      for (path in value)
        expand_and_select_index(path)
      invisible(NULL)
    },

    expand_and_select_index = function(path) {
      "path: 1-based integer vector of sibling positions"
      path <- as.integer(path)
      if (!length(path) || anyNA(path) || any(path < 1L))
        return(invisible(NULL))
      ## Ensure each ancestor is expanded in the flat TreeListModel, then
      ## resolve the target id from the children_cache id vectors.
      parent_ids <- {
        n <- as.integer(gListModelGetNItems(root_list))
        vapply(seq_len(n) - 1L, function(i) {
          as.character(gtkStringObjectGetString(
            gListModelGetObject(root_list, as.integer(i))))[1]
        }, character(1))
      }
      id <- NA_character_
      for (depth in seq_along(path)) {
        idx <- path[depth]
        if (idx < 1L || idx > length(parent_ids))
          return(invisible(NULL))
        id <- as.character(parent_ids[idx])[1]
        if (depth < length(path)) {
          create_children(gtkStringObjectNew(id))
          ensure_expanded(id)
          parent_ids <- get(id, envir = children_cache, inherits = FALSE)
          if (is.null(parent_ids) || !length(parent_ids))
            return(invisible(NULL))
          parent_ids <- as.character(parent_ids)
        }
      }
      if (!is.na(id))
        select_id(id)
      invisible(NULL)
    },

    expand_and_select_path = function(keys) {
      "keys: character vector of chosen-col values from root"
      keys <- as.character(keys)
      if (!length(keys))
        return(invisible(NULL))
      parent_ids <- {
        n <- as.integer(gListModelGetNItems(root_list))
        vapply(seq_len(n) - 1L, function(i) {
          as.character(gtkStringObjectGetString(
            gListModelGetObject(root_list, as.integer(i))))[1]
        }, character(1))
      }
      id <- NA_character_
      for (depth in seq_along(keys)) {
        found <- FALSE
        for (cand in parent_ids) {
          node <- get_node(cand)
          if (!is.null(node) && identical(as.character(node$key)[1], keys[depth])) {
            id <- as.character(cand)[1]
            found <- TRUE
            if (depth < length(keys)) {
              create_children(gtkStringObjectNew(id))
              ensure_expanded(id)
              parent_ids <- get(id, envir = children_cache, inherits = FALSE)
              if (is.null(parent_ids) || !length(parent_ids))
                return(invisible(NULL))
              parent_ids <- as.character(parent_ids)
            }
            break
          }
        }
        if (!found)
          return(invisible(NULL))
      }
      if (!is.na(id))
        select_id(id)
      invisible(NULL)
    },

    ensure_expanded = function(id) {
      "Expand TreeListRow for id if present in flat model"
      id <- as.character(id)[1]
      n <- as.integer(gListModelGetNItems(tree_model))
      for (i in seq_len(n) - 1L) {
        if (identical(position_to_id(i), id)) {
          row <- gtkTreeListModelGetRow(tree_model, as.integer(i))
          if (!is.null(row) && isTRUE(as.logical(gtkTreeListRowIsExpandable(row))))
            gtkTreeListRowSetExpanded(row, TRUE)
          return(invisible(NULL))
        }
      }
      invisible(NULL)
    },

    select_id = function(id) {
      id <- as.character(id)[1]
      node <- get_node(id)
      if (is.null(node))
        return(invisible(NULL))
      ## Expand each ancestor so the target appears in the flat model
      if (length(node$path) > 1L) {
        for (depth in seq_len(length(node$path) - 1L)) {
          prefix <- node$path[seq_len(depth)]
          anc_id <- NA_character_
          for (cand in ls(envir = nodes, all.names = TRUE)) {
            nd <- get_node(cand)
            if (!is.null(nd) && identical(as.character(nd$path), as.character(prefix))) {
              anc_id <- cand
              break
            }
          }
          if (is.na(anc_id))
            return(invisible(NULL))
          create_children(gtkStringObjectNew(anc_id))
          ensure_expanded(anc_id)
        }
      }
      n <- as.integer(gListModelGetNItems(tree_model))
      pos <- NA_integer_
      for (i in seq_len(n) - 1L) {
        if (identical(position_to_id(i), id)) {
          pos <- i
          break
        }
      }
      if (is.na(pos))
        return(invisible(NULL))
      block_handlers()
      on.exit(unblock_handlers())
      clear_selection()
      if (isTRUE(multiple)) {
        gtkSelectionModelSelectItem(selection, as.integer(pos), FALSE)
      } else {
        gtkSingleSelectionSetSelected(selection, as.integer(pos))
      }
      invisible(NULL)
    },

    get_items = function(i, j, ..., drop = TRUE) {
      "Row data for selection (display columns + key)"
      ids <- get_selected_ids()
      if (!length(ids))
        return(character(0))
      shown <- c(chosen_col, display_cols)
      rows <- lapply(ids, function(id) {
        node <- get_node(id)
        if (is.null(node))
          return(NULL)
        vals <- lapply(shown, function(ci) {
          v <- node$row[[ci]]
          if (is.null(v)) NA else v[[1]]
        })
        names(vals) <- get_names()
        vals
      })
      rows <- Filter(Negate(is.null), rows)
      if (!length(rows))
        return(character(0))
      out <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
      if (isTRUE(getWithDefault(drop, FALSE)))
        out[[1]]
      else
        out
    },

    set_items = function(value, i, j, ...) {
      stop(gettext("One sets items at construction through the offspring function"),
           call. = FALSE)
    },

    get_names = function() {
      if (length(col_names))
        col_names
      else
        character(0)
    },

    set_names = function(value) {
      value <- as.character(value)
      if (length(value) != length(c(chosen_col, display_cols)))
        return(invisible(NULL))
      col_names <<- value
      make_columns()
    },

    update_widget = function(...) {
      "Recompute root offspring; clear selection"
      block_handlers()
      on.exit(unblock_handlers())
      rebuild_tree()
      ## Columns already exist; rebound via model change
      invisible(NULL)
    },

    set_multiple = function(value) {
      ## Capture selection before flipping the mode flag (get_index branches on it)
      cur <- get_index()
      multiple <<- isTRUE(value)
      ## Rebuild selection model with same tree
      if (is.null(tree_model))
        return(invisible(NULL))
      if (isTRUE(multiple))
        selection <<- gtkMultiSelectionNew(tree_model)
      else {
        selection <<- gtkSingleSelectionNew(tree_model)
        gtkSingleSelectionSetCanUnselect(selection, TRUE)
        gtkSingleSelectionSetAutoselect(selection, FALSE)
      }
      gtkColumnViewSetModel(widget, selection)
      connect_selection_signal()
      if (length(cur))
        set_index(cur)
      invisible(NULL)
    },

    ## Handlers
    add_handler_changed = function(handler, action = NULL, ...) {
      add_handler_activate(handler, action, ...)
    },
    add_handler_selection_changed = function(handler, action = NULL, ...) {
      if (!is_handler(handler))
        return(invisible(NULL))
      o <- gWidgets2:::observer(.self, handler, action)
      invisible(add_observer(o, "selection-changed"))
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      ## iNZight uses clicked ≈ selection changed for gtree
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
