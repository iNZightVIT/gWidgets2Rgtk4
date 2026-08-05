##' @include gtree.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gvarbrowser guiWidgetsToolkitRgtk4
.gvarbrowser.guiWidgetsToolkitRgtk4 <- function(toolkit,
                                                handler = NULL,
                                                action = "summary",
                                                container = NULL, ...) {
  GVarBrowser$new(toolkit,
                  handler = handler, action = action,
                  container = container, ...)
}

##' Digest helper for workspace / nested objects
##' @noRd
.varbrowser_digest <- function(x) {
  tryCatch(digest::digest(x), error = function(e) {
    paste(class(x)[1], length(x), sep = ":")
  })
}

##' Workspace variable browser (GtkColumnView + GtkTreeListModel)
##'
##' Categories come from \code{gWidgets2:::gvarbrowser_default_classes}.
##' Workspace changes are watched via \code{WSWatcherModel} + \code{gtimer}.
##' Category children sync incrementally when possible; filter changes force
##' a full rebuild. Expansion and selection are restored across updates.
##'
##' @rdname gWidgets2Rgtk4-package
GVarBrowser <- setRefClass(
  "GVarBrowser",
  contains = "GWidget",
  fields = list(
    ws_model = "ANY",
    filter_classes = "list",
    filter_name = "character",
    other_label = "character",
    timer = "ANY",
    use_timer = "logical",
    nodes = "ANY",
    next_id = "integer",
    root_list = "ANY",
    tree_model = "ANY",
    selection = "ANY",
    children_cache = "ANY",
    column_objs = "list",
    category_ids = "character",
    force_full_rebuild = "logical"
  ),
  methods = list(
    initialize = function(toolkit = NULL,
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

      ws_model <<- gWidgets2:::WSWatcherModel$new()
      o <- gWidgets2:::Observer$new(function(self) self$update_view(), obj = .self)
      ws_model$add_observer(o)

      initFields(
        filter_classes = gWidgets2:::gvarbrowser_default_classes,
        filter_name = "",
        other_label = "Others",
        use_timer = TRUE,
        nodes = new.env(parent = emptyenv()),
        next_id = 1L,
        children_cache = new.env(parent = emptyenv()),
        root_list = NULL,
        tree_model = NULL,
        selection = NULL,
        column_objs = list(),
        category_ids = character(0),
        force_full_rebuild = FALSE,
        change_signal = "selection-changed",
        default_expand = TRUE,
        default_fill = TRUE
      )

      make_columns()
      rebuild_model()
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)

      timer <<- gtimer(1000, function(...) .self$ws_model$update_state(),
                       start = TRUE)
      update_view()
      callSuper(toolkit)
      ## After callSuper so .e exists for set_attr in add_drop_source
      add_drop_source(handler = function(h, ...) {
        l <- list(
          name = svalue(h$obj),
          obj = svalue(h$obj, drop = FALSE)
        )
        class(l) <- c("gvarbrowser_dropdata", class(l))
        l
      }, data.type = "object")
    },

    start_timer = function() if (isTRUE(use_timer)) timer$start_timer(),
    stop_timer = function() timer$stop_timer(),
    adjust_timer = function(ms) {
      if (missing(ms)) {
        n <- length(ls(envir = .GlobalEnv))
        ms <- 1000 * floor(log(5 + n, 5))
      }
      timer$set_interval(as.integer(ms))
    },
    set_filter_name = function(value) {
      filter_name <<- as.character(value)[1]
      force_full_rebuild <<- TRUE
      update_view()
    },
    set_filter_classes = function(value) {
      filter_classes <<- value
      force_full_rebuild <<- TRUE
      update_view()
    },

    alloc_id = function() {
      id <- sprintf("v%d", next_id)
      next_id <<- next_id + 1L
      id
    },

    get_node = function(id) {
      if (!nzchar(id) || !exists(id, envir = nodes, inherits = FALSE))
        return(NULL)
      get(id, envir = nodes, inherits = FALSE)
    },

    make_columns = function() {
      for (col in column_objs)
        try(gtkColumnViewRemoveColumn(widget, col), silent = TRUE)
      column_objs <<- list()

      ## Do not capture `nodes` by value — rebuild_model replaces that env.
      host <- .self
      ## Object column (expander + icon + name)
      factory <- gtkSignalListItemFactoryNew()
      gSignalConnectR(factory, "setup", function(f, list_item) {
        expander <- gtkTreeExpanderNew()
        box <- gtkBoxNew(0L, 4L)
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
        if (is.null(row)) return()
        gtkTreeExpanderSetListRow(expander, row)
        box <- gtkTreeExpanderGetChild(expander)
        icon <- gtkWidgetGetFirstChild(box)
        label <- gtkWidgetGetNextSibling(icon)
        item <- gtkTreeListRowGetItem(row)
        id <- tryCatch(gtkStringObjectGetString(item), error = function(e) "")
        node <- host$get_node(id)
        if (is.null(node)) {
          gtkLabelSetText(label, id)
          gtkWidgetSetVisible(icon, FALSE)
          return()
        }
        gtkLabelSetText(label, node$label)
        if (isTRUE(node$is_category)) {
          ## Bold via Pango markup
          gtkLabelSetMarkup(label, sprintf("<b>%s</b>", node$label))
          gtkWidgetSetVisible(icon, FALSE)
        } else {
          if (length(node$icon) && nzchar(node$icon[1]) &&
              isTRUE(gtk_image_apply_icon(icon, node$icon[1]))) {
            gtkWidgetSetVisible(icon, TRUE)
          } else {
            gtkWidgetSetVisible(icon, FALSE)
          }
        }
      })
      col1 <- gtkColumnViewColumnNew(gettext("Object"), factory)
      gtkColumnViewColumnSetExpand(col1, TRUE)
      gtkColumnViewAppendColumn(widget, col1)

      ## Description column
      factory2 <- gtkSignalListItemFactoryNew()
      gSignalConnectR(factory2, "setup", function(f, list_item) {
        label <- gtkLabelNew("")
        gtkLabelSetXalign(label, 0)
        gtkListItemSetChild(list_item, label)
      })
      gSignalConnectR(factory2, "bind", function(f, list_item) {
        label <- gtkListItemGetChild(list_item)
        row <- gtkListItemGetItem(list_item)
        if (is.null(row)) {
          gtkLabelSetText(label, "")
          return()
        }
        item <- gtkTreeListRowGetItem(row)
        id <- tryCatch(gtkStringObjectGetString(item), error = function(e) "")
        node <- host$get_node(id)
        gtkLabelSetText(label, if (is.null(node)) "" else node$summary)
      })
      col2 <- gtkColumnViewColumnNew(gettext("Description"), factory2)
      gtkColumnViewColumnSetExpand(col2, TRUE)
      gtkColumnViewAppendColumn(widget, col2)

      column_objs <<- list(col1, col2)
    },

    icon_for = function(x) {
      nm <- tryCatch(
        .stockIconFromObject(toolkit, x),
        error = function(e) ""
      )
      mapped <- tryCatch(
        .getStockIconByName(toolkit, nm),
        error = function(e) nm
      )
      as.character(mapped)[1]
    },

    register_category = function(label, sibling_index = NA_integer_) {
      id <- alloc_id()
      assign(id, list(
        label = label,
        summary = "",
        icon = "",
        is_category = TRUE,
        has_offspring = TRUE,
        path = character(0),
        name = label,
        category = label,
        sibling_index = as.integer(sibling_index),
        digest = ""
      ), envir = nodes)
      id
    },

    register_object = function(name, x, parent_path, category,
                               sibling_index = NA_integer_) {
      id <- alloc_id()
      path <- c(parent_path, name)
      ## Recurse into named lists
      has_off <- is.list(x) && !is.null(attr(x, "names")) && length(x) > 0
      assign(id, list(
        label = name,
        summary = tryCatch(gWidgets2:::short_summary(x), error = function(e) class(x)[1]),
        icon = icon_for(x),
        is_category = FALSE,
        has_offspring = has_off,
        path = path,
        name = name,
        category = category,
        sibling_index = as.integer(sibling_index),
        digest = .varbrowser_digest(x)
      ), envir = nodes)
      id
    },

    objects_for_category = function(cat_label) {
      if (identical(cat_label, gettext(other_label)) || identical(cat_label, other_label)) {
        klasses <- unlist(filter_classes)
        objs <- ws_model$get_by_function(function(y) {
          !(length(Filter(function(x) is(y, x), klasses)) > 0)
        })
      } else {
        klasses <- filter_classes[[cat_label]]
        if (is.null(klasses))
          klasses <- character(0)
        objs <- ws_model$get_by_function(function(y) {
          length(Filter(function(x) is(y, x), klasses)) > 0
        })
      }
      nms <- names(objs)
      if (nzchar(filter_name))
        nms <- grep(filter_name, nms, value = TRUE)
      nms <- sort(nms)
      if (!length(nms))
        return(list())
      objs[nms]
    },

    create_children = function(item) {
      id <- tryCatch(gtkStringObjectGetString(item), error = function(e) NULL)
      if (is.null(id) || !nzchar(id))
        return(NULL)
      node <- get_node(id)
      if (is.null(node) || !isTRUE(node$has_offspring))
        return(NULL)

      ## Categories must never return NULL when empty — that makes the
      ## TreeListRow permanently non-expandable. Use an empty StringList
      ## instead, and refresh when the cache is missing or still empty.
      if (isTRUE(node$is_category)) {
        child_ids <- NULL
        if (exists(id, envir = children_cache, inherits = FALSE))
          child_ids <- get(id, envir = children_cache, inherits = FALSE)
        if (is.null(child_ids) || !length(child_ids)) {
          objs <- objects_for_category(node$label)
          nms <- names(objs)
          child_ids <- character(0)
          for (i in seq_along(nms)) {
            child_ids <- c(child_ids,
                           register_object(nms[i], objs[[nms[i]]], character(0),
                                           node$label, sibling_index = i))
          }
          assign(id, child_ids, envir = children_cache)
        }
        if (!length(child_ids))
          return(gtkStringListNew(NULL))
        return(gtkStringListFromVector(child_ids))
      }

      child_ids <- NULL
      if (exists(id, envir = children_cache, inherits = FALSE))
        child_ids <- get(id, envir = children_cache, inherits = FALSE)

      if (is.null(child_ids)) {
        child_ids <- character(0)
        obj <- tryCatch(
          gWidgets2:::get_object_from_string(node$path),
          error = function(e) NULL
        )
        if (!is.null(obj) && is.list(obj) && !is.null(names(obj))) {
          nms <- names(obj)
          for (i in seq_along(nms)) {
            child_ids <- c(child_ids,
                           register_object(nms[i], obj[[nms[i]]], node$path,
                                           node$category, sibling_index = i))
          }
        }
        assign(id, child_ids, envir = children_cache)
      }

      if (!length(child_ids))
        return(NULL)
      gtkStringListFromVector(child_ids)
    },

    rebuild_model = function() {
      nodes <<- new.env(parent = emptyenv())
      children_cache <<- new.env(parent = emptyenv())
      next_id <<- 1L

      cat_labels <- c(names(filter_classes), gettext(other_label))
      cat_ids <- character(length(cat_labels))
      for (i in seq_along(cat_labels))
        cat_ids[i] <- register_category(cat_labels[i], sibling_index = i)
      category_ids <<- cat_ids
      root_list <<- gtkStringListFromVector(cat_ids)

      self <- .self
      tree_model <<- gtkTreeListModelNew(
        root_list, FALSE, FALSE,
        function(item) self$create_children(item)
      )
      selection <<- gtkMultiSelectionNew(tree_model)
      gtkColumnViewSetModel(widget, selection)
      self2 <- .self
      gSignalConnectR(selection, "selection-changed", function(...) {
        self2$notify_observers(signal = "selection-changed")
      })
    },

    ## --- expansion / selection snapshot --------------------------------
    position_to_id = function(pos) {
      row <- tryCatch(gtkTreeListModelGetRow(tree_model, as.integer(pos)),
                      error = function(e) NULL)
      if (is.null(row))
        return(NA_character_)
      item <- gtkTreeListRowGetItem(row)
      tryCatch(as.character(gtkStringObjectGetString(item))[1],
               error = function(e) NA_character_)
    },

    snapshot_ui_state = function() {
      expanded_cats <- character(0)
      selected_paths <- list()
      if (is.null(tree_model) || is.null(selection))
        return(list(expanded_cats = expanded_cats, selected_paths = selected_paths))
      n <- as.integer(gListModelGetNItems(tree_model))
      if (is.na(n) || n < 1L)
        return(list(expanded_cats = expanded_cats, selected_paths = selected_paths))
      for (i in seq_len(n) - 1L) {
        row <- tryCatch(gtkTreeListModelGetRow(tree_model, as.integer(i)),
                        error = function(e) NULL)
        if (is.null(row)) next
        id <- position_to_id(i)
        node <- get_node(id)
        if (is.null(node)) next
        if (isTRUE(node$is_category) &&
            isTRUE(as.logical(gtkTreeListRowGetExpanded(row))))
          expanded_cats <- c(expanded_cats, node$label)
        if (isTRUE(as.logical(gtkSelectionModelIsSelected(selection, as.integer(i)))) &&
            !isTRUE(node$is_category) && length(node$path))
          selected_paths[[length(selected_paths) + 1L]] <- node$path
      }
      list(expanded_cats = unique(expanded_cats), selected_paths = selected_paths)
    },

    find_row_for_id = function(id) {
      id <- as.character(id)[1]
      if (!nzchar(id) || is.null(tree_model))
        return(NULL)
      n <- as.integer(gListModelGetNItems(tree_model))
      for (i in seq_len(n) - 1L) {
        if (identical(position_to_id(i), id))
          return(gtkTreeListModelGetRow(tree_model, as.integer(i)))
      }
      NULL
    },

    ensure_expanded = function(id) {
      row <- find_row_for_id(id)
      if (!is.null(row) && isTRUE(as.logical(gtkTreeListRowIsExpandable(row))))
        gtkTreeListRowSetExpanded(row, TRUE)
      invisible(NULL)
    },

    restore_ui_state = function(state) {
      if (is.null(state) || is.null(tree_model))
        return(invisible(NULL))
      ## Expand categories by label
      for (lab in state$expanded_cats) {
        for (cid in category_ids) {
          node <- get_node(cid)
          if (!is.null(node) && identical(node$label, lab)) {
            ## Ensure children exist then expand
            create_children(gtkStringObjectNew(cid))
            ensure_expanded(cid)
            break
          }
        }
      }
      ## Reselect by object path
      for (path in state$selected_paths) {
        if (length(path))
          expand_and_select_path(path, clear_first = FALSE)
      }
      invisible(NULL)
    },

    ## --- incremental category sync -------------------------------------
    replace_string_list_ids = function(slist, new_ids) {
      if (is.null(slist))
        return(invisible(NULL))
      n_old <- tryCatch(as.integer(gListModelGetNItems(slist))[1],
                        error = function(e) 0L)
      if (!is.na(n_old) && n_old > 0L) {
        for (i in seq_len(n_old) - 1L)
          try(gtkStringListRemove(slist, 0L), silent = TRUE)
      }
      for (id in as.character(new_ids))
        try(gtkStringListAppend(slist, id), silent = TRUE)
      invisible(NULL)
    },

    sync_category_children = function(cat_id) {
      node <- get_node(cat_id)
      if (is.null(node) || !isTRUE(node$is_category))
        return(invisible(NULL))
      ## Ensure a cache entry exists (may be empty from an earlier probe)
      if (!exists(cat_id, envir = children_cache, inherits = FALSE))
        assign(cat_id, character(0), envir = children_cache)

      old_ids <- as.character(get(cat_id, envir = children_cache, inherits = FALSE))
      old_by_name <- list()
      for (oid in old_ids) {
        nd <- get_node(oid)
        if (!is.null(nd))
          old_by_name[[nd$name]] <- oid
      }

      objs <- objects_for_category(node$label)
      nms <- names(objs)
      new_ids <- character(length(nms))
      for (i in seq_along(nms)) {
        nm <- nms[i]
        x <- objs[[nm]]
        dgest <- .varbrowser_digest(x)
        reuse <- old_by_name[[nm]]
        if (!is.null(reuse)) {
          old_nd <- get_node(reuse)
          if (!is.null(old_nd) && identical(old_nd$digest, dgest)) {
            old_nd$sibling_index <- as.integer(i)
            assign(reuse, old_nd, envir = nodes)
            new_ids[i] <- reuse
            next
          }
          ## Digest changed: drop nested cache and replace node
          if (exists(reuse, envir = children_cache, inherits = FALSE))
            rm(list = reuse, envir = children_cache)
        }
        new_ids[i] <- register_object(nm, x, character(0), node$label,
                                      sibling_index = i)
      }
      ## Drop nodes no longer present
      removed <- setdiff(old_ids, new_ids)
      for (rid in removed) {
        if (exists(rid, envir = children_cache, inherits = FALSE))
          rm(list = rid, envir = children_cache)
        if (exists(rid, envir = nodes, inherits = FALSE))
          rm(list = rid, envir = nodes)
      }
      assign(cat_id, new_ids, envir = children_cache)

      ## Update the live child model whenever GTK already created one
      ## (not only when expanded — empty probes leave an empty StringList).
      row <- find_row_for_id(cat_id)
      if (!is.null(row)) {
        kids <- tryCatch(gtkTreeListRowGetChildren(row), error = function(e) NULL)
        if (!is.null(kids))
          replace_string_list_ids(kids, new_ids)
      }
      invisible(NULL)
    },

    incremental_update = function() {
      for (cid in category_ids) {
        ## Make sure every category has been probed so empty→nonempty
        ## transitions can splice into an existing child StringList.
        if (!exists(cid, envir = children_cache, inherits = FALSE))
          create_children(gtkStringObjectNew(cid))
        sync_category_children(cid)
      }
      invisible(NULL)
    },

    update_view = function(...) {
      stop_timer()
      adjust_timer()
      block_handlers()
      on.exit({
        unblock_handlers()
        start_timer()
        force_full_rebuild <<- FALSE
      })
      state <- snapshot_ui_state()
      do_full <- isTRUE(force_full_rebuild) || is.null(root_list) ||
        is.null(tree_model) || !length(category_ids)
      if (do_full) {
        rebuild_model()
      } else {
        incremental_update()
      }
      restore_ui_state(state)
      invisible(NULL)
    },

    ## --- selection helpers ---------------------------------------------
    walk_selected = function() {
      if (is.null(selection) || is.null(tree_model))
        return(list())
      n <- as.integer(gListModelGetNItems(tree_model))
      out <- list()
      for (i in seq_len(n) - 1L) {
        if (!isTRUE(as.logical(gtkSelectionModelIsSelected(selection, as.integer(i)))))
          next
        row <- gtkTreeListModelGetRow(tree_model, as.integer(i))
        item <- gtkTreeListRowGetItem(row)
        id <- tryCatch(gtkStringObjectGetString(item), error = function(e) "")
        if (!nzchar(id) || !exists(id, envir = nodes, inherits = FALSE))
          next
        node <- get(id, envir = nodes, inherits = FALSE)
        if (isTRUE(node$is_category))
          next
        out[[length(out) + 1L]] <- node$path
      }
      out
    },

    get_selected_ids = function() {
      if (is.null(selection) || is.null(tree_model))
        return(character(0))
      n <- as.integer(gListModelGetNItems(tree_model))
      ids <- character(0)
      for (i in seq_len(n) - 1L) {
        if (!isTRUE(as.logical(gtkSelectionModelIsSelected(selection, as.integer(i)))))
          next
        id <- position_to_id(i)
        node <- get_node(id)
        if (is.null(node) || isTRUE(node$is_category))
          next
        ids <- c(ids, id)
      }
      ids
    },

    clear_selection = function() {
      if (!is.null(selection))
        try(gtkSelectionModelUnselectAll(selection), silent = TRUE)
      invisible(NULL)
    },

    select_id = function(id, clear_first = TRUE) {
      id <- as.character(id)[1]
      node <- get_node(id)
      if (is.null(node))
        return(invisible(NULL))
      ## Expand category (and nested ancestors) so the row is in the flat model
      if (length(node$path) >= 1L && nzchar(node$category)) {
        for (cid in category_ids) {
          cat <- get_node(cid)
          if (!is.null(cat) && identical(cat$label, node$category)) {
            create_children(gtkStringObjectNew(cid))
            ensure_expanded(cid)
            break
          }
        }
      }
      if (length(node$path) > 1L) {
        parent_path <- node$path[-length(node$path)]
        ## Ensure each nested ancestor under the category is expanded
        for (depth in seq_along(parent_path)) {
          sub <- parent_path[seq_len(depth)]
          for (cand in ls(envir = nodes, all.names = TRUE)) {
            nd <- get_node(cand)
            if (!is.null(nd) && identical(as.character(nd$path), as.character(sub))) {
              create_children(gtkStringObjectNew(cand))
              ensure_expanded(cand)
              break
            }
          }
        }
      }
      if (isTRUE(clear_first))
        clear_selection()
      n <- as.integer(gListModelGetNItems(tree_model))
      for (i in seq_len(n) - 1L) {
        if (identical(position_to_id(i), id)) {
          gtkSelectionModelSelectItem(selection, as.integer(i), FALSE)
          break
        }
      }
      invisible(NULL)
    },

    ## Path of object names (no category). Expand owning category then children.
    expand_and_select_path = function(keys, clear_first = TRUE) {
      keys <- as.character(keys)
      if (!length(keys))
        return(invisible(NULL))
      ## Find which category owns the tip / root key
      tip <- keys[1]
      cat_id <- NA_character_
      for (cid in category_ids) {
        objs <- objects_for_category(get_node(cid)$label)
        if (tip %in% names(objs)) {
          cat_id <- cid
          break
        }
      }
      if (is.na(cat_id)) {
        ## Already registered somewhere?
        for (id in ls(envir = nodes, all.names = TRUE)) {
          nd <- get_node(id)
          if (!is.null(nd) && identical(nd$path, keys)) {
            select_id(id, clear_first = clear_first)
            return(invisible(NULL))
          }
        }
        return(invisible(NULL))
      }
      create_children(gtkStringObjectNew(cat_id))
      ensure_expanded(cat_id)
      parent_ids <- get(cat_id, envir = children_cache, inherits = FALSE)
      id <- NA_character_
      for (depth in seq_along(keys)) {
        found <- FALSE
        for (cand in as.character(parent_ids)) {
          nd <- get_node(cand)
          if (!is.null(nd) && identical(as.character(nd$name)[1], keys[depth])) {
            id <- cand
            found <- TRUE
            if (depth < length(keys)) {
              create_children(gtkStringObjectNew(id))
              ensure_expanded(id)
              parent_ids <- get(id, envir = children_cache, inherits = FALSE)
              if (is.null(parent_ids) || !length(parent_ids))
                return(invisible(NULL))
            }
            break
          }
        }
        if (!found)
          return(invisible(NULL))
      }
      if (!is.na(id))
        select_id(id, clear_first = clear_first)
      invisible(NULL)
    },

    ## Sibling index path dropping the category level (RGtk2 parity)
    id_to_index_path = function(id) {
      node <- get_node(id)
      if (is.null(node) || isTRUE(node$is_category))
        return(integer(0))
      idxs <- integer(0)
      cur_path <- node$path
      while (length(cur_path)) {
        found <- NULL
        for (cand in ls(envir = nodes, all.names = TRUE)) {
          nd <- get_node(cand)
          if (!is.null(nd) && !isTRUE(nd$is_category) &&
              identical(as.character(nd$path), as.character(cur_path))) {
            found <- nd
            break
          }
        }
        if (is.null(found) || is.na(found$sibling_index))
          break
        idxs <- c(as.integer(found$sibling_index), idxs)
        cur_path <- cur_path[-length(cur_path)]
      }
      as.integer(idxs)
    },

    get_value = function(drop = TRUE, ...) {
      paths <- walk_selected()
      if (!length(paths))
        return(character(0))
      drop <- getWithDefault(drop, TRUE)
      if (is.null(drop) || isTRUE(drop)) {
        out <- lapply(paths, function(x) {
          bits <- sapply(x, function(i) {
            if (grepl("\\s", i)) sprintf("'%s'", i) else i
          })
          paste(bits, collapse = "$")
        })
        if (length(out) == 1L) out[[1]] else out
      } else {
        lapply(paths, gWidgets2:::get_object_from_string)
      }
    },

    set_value = function(value, ...) {
      "Select by object name or name path (character). Empty clears."
      if (!length(value) || (is.character(value) && !nzchar(paste(value, collapse = "")))) {
        clear_selection()
        return(invisible(NULL))
      }
      value <- as.character(value)
      ## Single string may be 'a$b' form
      if (length(value) == 1L && grepl("$", value, fixed = TRUE)) {
        bits <- strsplit(value, "$", fixed = TRUE)[[1]]
        bits <- gsub("^'|'$", "", bits)
        expand_and_select_path(bits)
      } else {
        expand_and_select_path(value)
      }
      invisible(NULL)
    },

    get_index = function(...) {
      ids <- get_selected_ids()
      if (!length(ids))
        return(integer(0))
      out <- lapply(ids, function(id) id_to_index_path(id))
      if (length(out) == 1L)
        out[[1]]
      else
        out
    },

    get_items = function(i, j, ..., drop = TRUE) {
      paths <- walk_selected()
      if (!length(paths))
        return(character(0))
      if (isTRUE(drop) && length(paths) == 1L)
        paths[[1]]
      else
        paths
    },

    set_items = function(value, i, j, ...) invisible(NULL),

    add_handler_changed = function(handler, action = NULL, ...) {
      add_handler_activate(handler, action, ...)
    },
    add_handler_selection_changed = function(handler, action = NULL, ...) {
      if (!is_handler(handler))
        return(invisible(NULL))
      o <- gWidgets2:::observer(.self, handler, action)
      invisible(add_observer(o, "selection-changed"))
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
