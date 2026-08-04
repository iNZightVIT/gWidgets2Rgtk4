##' @include GContainer.R
NULL

##' toolkit constructor for ggroup
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .ggroup guiWidgetsToolkitRgtk4
.ggroup.guiWidgetsToolkitRgtk4 <- function(toolkit, horizontal = TRUE, spacing = 5,
                                           use.scrollwindow = FALSE, container = NULL,
                                           padding = NULL, margin = NULL, border = NULL,
                                           ...) {
  GGroup$new(toolkit, horizontal, spacing = spacing,
             use.scrollwindow = use.scrollwindow, container = container,
             padding = padding, margin = margin, border = border, ...)
}

GGroupBase <- setRefClass(
  "GGroupBase",
  contains = "GContainer",
  fields = list(
    horizontal = "logical",
    .box_padding = "ANY",
    .box_margin = "ANY",
    .box_border = "ANY",
    .box_css_class = "character",
    .box_css_provider = "ANY"
  ),
  methods = list(
    make_widget = function(...) {},
    init_box_model = function(padding = NULL, margin = NULL, border = NULL) {
      if (is(.box_padding, "uninitializedField") || is.null(.box_padding))
        .box_padding <<- 0L
      if (is(.box_margin, "uninitializedField") || is.null(.box_margin))
        .box_margin <<- 0L
      if (is(.box_border, "uninitializedField") || is.null(.box_border))
        .box_border <<- 0L
      if (is(.box_css_class, "uninitializedField"))
        .box_css_class <<- ""
      if (!is.null(padding))
        .box_padding <<- padding
      if (!is.null(margin))
        .box_margin <<- margin
      if (!is.null(border))
        .box_border <<- as.integer(border)[1]
      apply_box_model(.self)
      invisible(NULL)
    },
    get_padding = function(...) .box_padding,
    set_padding = function(value, ...) {
      .box_padding <<- if (is.null(value)) 0L else value
      apply_box_model(.self)
      invisible(NULL)
    },
    get_margin = function(...) .box_margin,
    set_margin = function(value, ...) {
      .box_margin <<- if (is.null(value)) 0L else value
      apply_box_model(.self)
      invisible(NULL)
    },
    get_border = function(...) .box_border,
    set_border = function(value, ...) {
      .box_border <<- if (is.null(value)) 0L else as.integer(value)[1]
      apply_box_model(.self)
      invisible(NULL)
    },
    ## Deprecated: GTK2 border-width ≡ CSS padding
    set_borderwidth = function(value, ...) {
      warning("'set_borderwidth' is deprecated; use 'set_padding' instead",
              call. = FALSE)
      set_padding(value, ...)
    },
    add_child = function(child, expand, fill, anchor, ...) {
      toolkit_child <- getBlock(child)
      theArgs <- list(...)
      expand <- getWithDefault(expand, getWithDefault(child$default_expand, FALSE))
      ## align= is a historical alias for anchor=
      anchor <- getWithDefault(anchor, theArgs$align)
      anchor <- getWithDefault(anchor, NULL)
      if (expand) {
        fill <- getWithDefault(fill, getWithDefault(child$default_fill,
                                                    ifelse(is.null(anchor), "both", "")))
      } else {
        fill <- getWithDefault(fill, FALSE)
      }
      padding <- getWithDefault(theArgs$padding, 0L)
      ## Pass raw gWidgets [-1,1] anchors; helper owns GTK mapping
      set_child_expand_fill_anchor(toolkit_child, expand = expand, fill = fill,
                                   anchor = anchor,
                                   horizontal = horizontal, padding = padding)
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
    get_length = function(...) length(children),
    get_value = function(...) as.integer(gtkBoxGetSpacing(widget)),
    set_value = function(value, ...) {
      gtkBoxSetSpacing(widget, as.integer(value)[1])
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
                          use.scrollwindow = FALSE, container = NULL,
                          padding = NULL, margin = NULL, border = NULL, ...) {
      horizontal <<- horizontal
      if (is(widget, "uninitializedField"))
        make_widget(use.scrollwindow, spacing)
      init_box_model(padding = padding, margin = margin, border = border)
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
