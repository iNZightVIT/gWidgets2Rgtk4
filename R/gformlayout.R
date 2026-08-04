##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gformlayout guiWidgetsToolkitRgtk4
.gformlayout.guiWidgetsToolkitRgtk4 <- function(toolkit, align = "left", spacing = 5,
                                                container = NULL, ...) {
  GFormLayout$new(toolkit, align, spacing, container = container, ...)
}

GFormLayout <- setRefClass(
  "GFormLayout",
  contains = "GContainer",
  fields = list(
    align = "character",
    spacing = "numeric",
    nrows = "integer",
    labels = "list"
  ),
  methods = list(
    initialize = function(toolkit = NULL, align = "left", spacing = 5,
                          container = NULL, ...) {
      initFields(align = align, spacing = as.numeric(spacing), nrows = 0L,
                 labels = list())
      widget <<- gtkGridNew()
      gtkGridSetColumnSpacing(widget, as.integer(spacing))
      gtkGridSetRowSpacing(widget, as.integer(spacing))
      block <<- widget
      add_to_parent(container, .self)
      callSuper(toolkit, ...)
    },
    add_child = function(child, expand = NULL, fill = NULL, anchor = NULL, ..., label = "") {
      add_row(label, child, expand, fill, anchor, ...)
    },
    add_row = function(label, child, expand = NULL, fill = NULL, anchor = NULL, ...) {
      row <- nrows
      lab_text <- as.character(label)[1]
      lab <- gtkLabelNew(lab_text)
      halign <- switch(align, left = .GtkAlign$START, right = .GtkAlign$END, .GtkAlign$END)
      gtkWidgetSetHalign(lab, halign)
      child_widget <- getBlock(child)
      gtkWidgetSetHexpand(child_widget, TRUE)
      gtkGridAttach(widget, lab, 0L, as.integer(row), 1L, 1L)
      gtkGridAttach(widget, child_widget, 1L, as.integer(row), 1L, 1L)
      nrows <<- as.integer(row + 1L)
      if (nzchar(lab_text))
        labels[[lab_text]] <<- lab
      child_bookkeeping(child)
    },
    get_length = function(...) nrows,
    get_labels = function() labels,
    ## Set font on a row label identified by its text (RGtk2 compatibility)
    set_font = function(label_value, value) {
      lab <- labels[[as.character(label_value)[1]]]
      if (is.null(lab))
        return(invisible(NULL))
      ## Temporary component-like apply: unique class on this GtkLabel
      class_name <- .next_font_class()
      provider <- gtkCssProviderNew()
      decls <- if (is.character(value) && length(value) == 1L && is.null(names(value))) {
        as.character(value)[1]
      } else {
        font_spec_to_css(value)
      }
      rule <- .font_class_rule(class_name, decls)
      gtkCssProviderLoadFromData(provider, rule, -1L)
      gtkWidgetAddCssClass(lab, class_name)
      ctx <- gtkWidgetGetStyleContext(lab)
      gtkStyleContextAddProvider(ctx, provider, 800L)
      invisible(NULL)
    },
    set_label_tooltip = function(label_value, value) {
      lab <- labels[[as.character(label_value)[1]]]
      if (!is.null(lab))
        gtkWidgetSetTooltipText(lab, paste(value, collapse = "\n"))
      invisible(NULL)
    }
  )
)
