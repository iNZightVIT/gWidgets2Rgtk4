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

##' Workspace variable browser (GtkColumnView + GtkTreeListModel)
##'
##' Categories come from \code{gWidgets2:::gvarbrowser_default_classes}.
##' Workspace changes are watched via \code{WSWatcherModel} + \code{gtimer}.
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
    column_objs = "list"
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
        change_signal = "selection-changed",
        default_expand = TRUE,
        default_fill = TRUE
      )

      make_columns()
      rebuild_model()
      ## DnD stub — drop source deferred with rest of DnD
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)

      timer <<- gtimer(1000, function(...) .self$ws_model$update_state(),
                       start = TRUE)
      update_view()
      callSuper(toolkit)
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
      update_view()
    },
    set_filter_classes = function(value) {
      filter_classes <<- value
      update_view()
    },

    alloc_id = function() {
      id <- sprintf("v%d", next_id)
      next_id <<- next_id + 1L
      id
    },

    make_columns = function() {
      for (col in column_objs)
        try(gtkColumnViewRemoveColumn(widget, col), silent = TRUE)
      column_objs <<- list()

      env <- nodes
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
        node <- if (nzchar(id) && exists(id, envir = env, inherits = FALSE))
          get(id, envir = env, inherits = FALSE) else NULL
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
          if (length(node$icon) && nzchar(node$icon[1])) {
            gtkImageSetFromIconName(icon, node$icon[1])
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
        node <- if (nzchar(id) && exists(id, envir = env, inherits = FALSE))
          get(id, envir = env, inherits = FALSE) else NULL
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

    register_category = function(label) {
      id <- alloc_id()
      assign(id, list(
        label = label,
        summary = "",
        icon = "",
        is_category = TRUE,
        has_offspring = TRUE,
        path = character(0),
        name = label,
        category = label
      ), envir = nodes)
      id
    },

    register_object = function(name, x, parent_path, category) {
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
        category = category
      ), envir = nodes)
      id
    },

    create_children = function(item) {
      id <- tryCatch(gtkStringObjectGetString(item), error = function(e) NULL)
      if (is.null(id) || !nzchar(id))
        return(NULL)
      node <- if (exists(id, envir = nodes, inherits = FALSE))
        get(id, envir = nodes, inherits = FALSE) else NULL
      if (is.null(node) || !isTRUE(node$has_offspring))
        return(NULL)

      child_ids <- NULL
      if (exists(id, envir = children_cache, inherits = FALSE))
        child_ids <- get(id, envir = children_cache, inherits = FALSE)

      if (is.null(child_ids)) {
        child_ids <- character(0)
        if (isTRUE(node$is_category)) {
          cat_label <- node$label
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
          for (nm in nms) {
            child_ids <- c(child_ids,
                           register_object(nm, objs[[nm]], character(0), cat_label))
          }
        } else {
          obj <- tryCatch(
            gWidgets2:::get_object_from_string(node$path),
            error = function(e) NULL
          )
          if (!is.null(obj) && is.list(obj) && !is.null(names(obj))) {
            for (nm in names(obj)) {
              child_ids <- c(child_ids,
                             register_object(nm, obj[[nm]], node$path, node$category))
            }
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

      cat_ids <- c(
        vapply(names(filter_classes), register_category, character(1)),
        register_category(gettext(other_label))
      )
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

    update_view = function(...) {
      stop_timer()
      adjust_timer()
      ## Full rebuild (MVP). Incremental sync can come later if needed.
      block_handlers()
      on.exit({
        unblock_handlers()
        start_timer()
      })
      rebuild_model()
      invisible(NULL)
    },

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
      ## Not implemented: open/select by name
      invisible(NULL)
    },

    get_index = function(...) {
      paths <- walk_selected()
      if (!length(paths))
        return(numeric(0))
      ## Index among siblings is not tracked the same way; return empty for MVP
      ## (RGtk2 returned path indices dropping the category). Leave stub.
      if (length(paths) == 1L)
        seq_along(paths[[1]])
      else
        lapply(paths, seq_along)
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
