##' @include GContainer.R
NULL

##' toolkit constructor for ggroup
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .ggroup guiWidgetsToolkitRgtk4
.ggroup.guiWidgetsToolkitRgtk4 <- function(toolkit, horizontal = TRUE, spacing = 5,
                                           use.scrollwindow = FALSE, container = NULL, ...) {
  GGroup$new(toolkit, horizontal, spacing = spacing,
             use.scrollwindow = use.scrollwindow, container, ...)
}

GGroupBase <- setRefClass(
  "GGroupBase",
  contains = "GContainer",
  fields = list(horizontal = "logical"),
  methods = list(
    make_widget = function(...) {},
    add_child = function(child, expand, fill, anchor, ...) {
      toolkit_child <- getBlock(child)
      theArgs <- list(...)
      expand <- getWithDefault(expand, getWithDefault(child$default_expand, FALSE))
      if (!is.null(theArgs$align))
        theArgs$anchor <- theArgs$align
      anchor <- getWithDefault(anchor, NULL)
      if (!is.null(anchor)) {
        a <- (as.numeric(anchor) + 1) / 2
        a[2] <- 1 - a[2]
        set_child_align(toolkit_child, getWidget(child), a)
      }
      if (expand) {
        fill <- getWithDefault(fill, getWithDefault(child$default_fill,
                                                    ifelse(is.null(anchor), "both", "")))
      } else {
        fill <- getWithDefault(fill, FALSE)
      }
      padding <- getWithDefault(theArgs$padding, 0L)
      set_child_expand_fill_anchor(toolkit_child, expand = expand, fill = fill,
                                   anchor = if (is.null(anchor)) NULL else {
                                     a <- (as.numeric(anchor) + 1) / 2
                                     a[2] <- 1 - a[2]
                                     a
                                   },
                                   horizontal = horizontal, padding = padding)
      ## Re-apply align if set without expand path duplicating
      gtkBoxAppend(widget, toolkit_child)
      child_bookkeeping(child)
    },
    remove_child = function(child) {
      children <<- Filter(function(x) !identical(x, child), children)
      child$set_parent(NULL)
      gtkBoxRemove(widget, getBlock(child))
    },
    add_spring = function() {
      spring <- gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L)
      gtkWidgetSetHexpand(spring, TRUE)
      gtkWidgetSetVexpand(spring, TRUE)
      gtkBoxAppend(widget, spring)
    },
    add_space = function(value) {
      box <- gtkBoxNew(.GtkOrientation$HORIZONTAL, 0L)
      value <- as.integer(value)[1]
      if (horizontal)
        gtkWidgetSetSizeRequest(box, value, -1L)
      else
        gtkWidgetSetSizeRequest(box, -1L, value)
      gtkBoxAppend(widget, box)
    },
    get_items = function(i, j, ..., drop = TRUE) {
      out <- children[i]
      if (drop && length(out) == 1)
        out[[1]]
      else
        out
    },
    get_value = function(...) gtkBoxGetSpacing(widget),
    set_value = function(value, ...) {
      gtkBoxSetSpacing(widget, as.integer(value)[1])
    },
    set_borderwidth = function(value, ...) {
      m <- as.integer(value)[1]
      gtkWidgetSetMarginTop(widget, m)
      gtkWidgetSetMarginBottom(widget, m)
      gtkWidgetSetMarginStart(widget, m)
      gtkWidgetSetMarginEnd(widget, m)
    },
    set_size = function(value) {
      tmp <- getBlock(.self)
      value <- as.integer(value)
      gtkWidgetSetSizeRequest(tmp, value[1], value[2])
    }
  )
)

GGroup <- setRefClass(
  "GGroup",
  contains = "GGroupBase",
  methods = list(
    initialize = function(toolkit = NULL, horizontal = TRUE, spacing = 5,
                          use.scrollwindow = FALSE, container = NULL, ...) {
      horizontal <<- horizontal
      if (is(widget, "uninitializedField"))
        make_widget(use.scrollwindow, spacing)
      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },
    make_widget = function(use.scrollwindow, spacing) {
      orient <- if (horizontal) .GtkOrientation$HORIZONTAL else .GtkOrientation$VERTICAL
      widget <<- gtkBoxNew(orient, as.integer(spacing))
      gtkBoxSetHomogeneous(widget, FALSE)
      set_value(spacing)
      use.scrollwindow <- as.character(use.scrollwindow)
      if (use.scrollwindow != "FALSE") {
        block <<- gtkScrolledWindowNew()
        if (use.scrollwindow == "x")
          gtkScrolledWindowSetPolicy(block, .GtkPolicyType$AUTOMATIC, .GtkPolicyType$NEVER)
        else if (use.scrollwindow == "y")
          gtkScrolledWindowSetPolicy(block, .GtkPolicyType$NEVER, .GtkPolicyType$AUTOMATIC)
        else
          gtkScrolledWindowSetPolicy(block, .GtkPolicyType$AUTOMATIC, .GtkPolicyType$AUTOMATIC)
        gtkScrolledWindowSetChild(block, widget)
      } else {
        block <<- widget
      }
    }
  )
)
