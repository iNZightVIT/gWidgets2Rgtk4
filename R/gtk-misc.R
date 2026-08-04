##' @include misc.R
NULL

##' Stop getWidget recursion on native GTK objects
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method getWidget RGtkObject
getWidget.RGtkObject <- function(obj) obj

##' Stop getBlock recursion on native GTK objects
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method getBlock RGtkObject
getBlock.RGtkObject <- function(obj) obj

## GTK4 Align: FILL=0, START=1, END=2, CENTER=3
.GtkAlign <- list(FILL = 0L, START = 1L, END = 2L, CENTER = 3L)

## GTK4 Orientation: HORIZONTAL=0, VERTICAL=1
.GtkOrientation <- list(HORIZONTAL = 0L, VERTICAL = 1L)

## GTK4 PolicyType: ALWAYS=0, AUTOMATIC=1, NEVER=2
.GtkPolicyType <- list(ALWAYS = 0L, AUTOMATIC = 1L, NEVER = 2L)

## Map gWidgets expand/fill/anchor onto GTK4 widget properties.
## expand/fill/anchor use gWidgets conventions; child is a GtkWidget.
set_child_expand_fill_anchor <- function(child, expand = FALSE, fill = NULL,
                                         anchor = NULL, horizontal = TRUE,
                                         padding = 0L) {
  expand <- isTRUE(expand)

  if (!is.null(anchor)) {
    ## gWidgets anchor in [-1,1]^2 → GTK align
    ax <- as.numeric(anchor[1])
    ay <- as.numeric(anchor[2])
    ## flip y: gWidgets +1 is top, GTK START is top
    halign <- if (ax < -0.33) .GtkAlign$START else if (ax > 0.33) .GtkAlign$END else .GtkAlign$CENTER
    valign <- if (ay > 0.33) .GtkAlign$START else if (ay < -0.33) .GtkAlign$END else .GtkAlign$CENTER
    gtkWidgetSetHalign(child, halign)
    gtkWidgetSetValign(child, valign)
  }

  fill_h <- FALSE
  fill_v <- FALSE
  if (expand) {
    if (is.null(fill))
      fill <- if (is.null(anchor)) "both" else ""
    if (is.logical(fill)) {
      fill_h <- fill
      fill_v <- fill
    } else if (is.character(fill)) {
      fill <- tolower(fill[1])
      fill_h <- fill %in% c("both", "x", "TRUE", "true")
      fill_v <- fill %in% c("both", "y", "TRUE", "true")
      if (identical(fill, "true") || identical(fill, "TRUE") || fill == "both") {
        fill_h <- TRUE
        fill_v <- TRUE
      }
    }
    if (horizontal) {
      gtkWidgetSetHexpand(child, TRUE)
      if (fill_h && is.null(anchor))
        gtkWidgetSetHalign(child, .GtkAlign$FILL)
      if (fill_v)
        gtkWidgetSetVexpand(child, TRUE)
    } else {
      gtkWidgetSetVexpand(child, TRUE)
      if (fill_v && is.null(anchor))
        gtkWidgetSetValign(child, .GtkAlign$FILL)
      if (fill_h)
        gtkWidgetSetHexpand(child, TRUE)
    }
  }

  padding <- as.integer(padding)[1]
  if (!is.na(padding) && padding > 0L) {
    gtkWidgetSetMarginStart(child, padding)
    gtkWidgetSetMarginEnd(child, padding)
    gtkWidgetSetMarginTop(child, padding)
    gtkWidgetSetMarginBottom(child, padding)
  }

  invisible(expand)
}

## Character fill → logical for historical callers
fill_to_logical <- function(fill, horizontal = TRUE) {
  if (is.null(fill))
    return(FALSE)
  if (is.logical(fill))
    return(fill)
  if (is.character(fill)) {
    fill <- tolower(fill[1])
    if (fill == "both")
      return(TRUE)
    if (fill == "x" && horizontal)
      return(TRUE)
    if (fill == "y" && !horizontal)
      return(TRUE)
  }
  FALSE
}

## Map R class to GObject type string (for models / stores)
RtoGObjectConversion <- function(x) {
  if (is(x, "factor"))
    "gchararray"
  else if (is(x, "integer"))
    "gint"
  else if (is(x, "numeric"))
    "gdouble"
  else if (is(x, "logical"))
    "gboolean"
  else
    "gchararray"
}

## Stock / legacy icon name → GTK4 icon theme name
.stock_to_icon_name <- c(
  "ok" = "emblem-ok",
  "gtk-ok" = "emblem-ok",
  "cancel" = "process-stop",
  "gtk-cancel" = "process-stop",
  "close" = "window-close",
  "gtk-close" = "window-close",
  "quit" = "application-exit",
  "gtk-quit" = "application-exit",
  "open" = "document-open",
  "gtk-open" = "document-open",
  "save" = "document-save",
  "gtk-save" = "document-save",
  "save-as" = "document-save-as",
  "gtk-save-as" = "document-save-as",
  "new" = "document-new",
  "gtk-new" = "document-new",
  "delete" = "edit-delete",
  "gtk-delete" = "edit-delete",
  "clear" = "edit-clear",
  "gtk-clear" = "edit-clear",
  "cut" = "edit-cut",
  "gtk-cut" = "edit-cut",
  "copy" = "edit-copy",
  "gtk-copy" = "edit-copy",
  "paste" = "edit-paste",
  "gtk-paste" = "edit-paste",
  "find" = "edit-find",
  "gtk-find" = "edit-find",
  "help" = "help-browser",
  "gtk-help" = "help-browser",
  "home" = "go-home",
  "gtk-home" = "go-home",
  "add" = "list-add",
  "gtk-add" = "list-add",
  "remove" = "list-remove",
  "gtk-remove" = "list-remove",
  "yes" = "emblem-ok",
  "gtk-yes" = "emblem-ok",
  "no" = "process-stop",
  "gtk-no" = "process-stop",
  "apply" = "emblem-ok",
  "gtk-apply" = "emblem-ok",
  "refresh" = "view-refresh",
  "gtk-refresh" = "view-refresh",
  "info" = "dialog-information",
  "gtk-info" = "dialog-information",
  "dialog-ok" = "emblem-ok",
  "dialog-error" = "dialog-error",
  "dialog-warning" = "dialog-warning",
  "dialog-question" = "dialog-question",
  "dialog-information" = "dialog-information"
)

stock_to_icon_name <- function(name) {
  if (is.null(name) || length(name) < 1L || is.na(name[1]) || !nzchar(name[1]))
    return(NULL)
  name <- as.character(name)[1]
  nms <- names(.stock_to_icon_name)
  if (name %in% nms)
    return(unname(.stock_to_icon_name[name]))
  ## strip gtk- / gw- prefixes and retry
  bare <- sub("^(gtk|gw)-", "", name)
  if (bare %in% nms)
    return(unname(.stock_to_icon_name[bare]))
  ## already looks like an icon theme name
  name
}

## ---------------------------------------------------------------------------
## Container CSS box model (padding / margin / border)
## ---------------------------------------------------------------------------

.box_css_env <- new.env(parent = emptyenv())
.box_css_env$id <- 0L

## Normalize to CSS order: top, right, bottom, left.
normalize_css_sides <- function(value, default = 0L) {
  if (is.null(value))
    return(rep(as.integer(default)[1], 4L))
  v <- as.integer(value)
  if (anyNA(v))
    v[is.na(v)] <- as.integer(default)[1]
  if (length(v) == 1L)
    return(rep(v[1], 4L))
  if (length(v) == 2L)
    return(c(v[1], v[2], v[1], v[2]))
  if (length(v) >= 4L)
    return(v[1:4])
  stop("padding/margin must be length 1, 2, or 4", call. = FALSE)
}

## Apply GTK margins; value is CSS-order sides (top, right, bottom, left).
set_widget_margins <- function(w, value) {
  if (is.null(w) || is(w, "uninitializedField"))
    return(invisible(NULL))
  sides <- normalize_css_sides(value, 0L)
  gtkWidgetSetMarginTop(w, sides[1])
  gtkWidgetSetMarginEnd(w, sides[2])
  gtkWidgetSetMarginBottom(w, sides[3])
  gtkWidgetSetMarginStart(w, sides[4])
  invisible(NULL)
}

.box_model_css_decls <- function(padding, border) {
  decls <- character()
  pad <- normalize_css_sides(padding, 0L)
  if (any(pad > 0L))
    decls <- c(decls, sprintf("padding: %dpx %dpx %dpx %dpx",
                              pad[1], pad[2], pad[3], pad[4]))
  b <- as.integer(border)[1]
  if (!is.na(b) && b > 0L)
    decls <- c(decls, sprintf("border: %dpx solid", b))
  if (!length(decls))
    return("")
  paste0(paste(decls, collapse = "; "), ";")
}

.next_box_css_class <- function() {
  .box_css_env$id <- as.integer(.box_css_env$id + 1L)
  sprintf("gw-box-%d", .box_css_env$id)
}

## Padding/border CSS on inner widget; margins on outer block.
apply_box_model <- function(comp) {
  pad <- tryCatch(comp$.box_padding, error = function(e) 0L)
  mar <- tryCatch(comp$.box_margin, error = function(e) 0L)
  bor <- tryCatch(comp$.box_border, error = function(e) 0L)
  if (is.null(pad) || (length(pad) == 1L && is.na(pad)))
    pad <- 0L
  if (is.null(mar) || (length(mar) == 1L && is.na(mar)))
    mar <- 0L
  if (is.null(bor) || length(bor) < 1L || is.na(bor[1]))
    bor <- 0L

  blk <- tryCatch(getBlock(comp), error = function(e) NULL)
  set_widget_margins(blk, mar)

  w <- tryCatch({
    if (is.null(comp$widget) || is(comp$widget, "uninitializedField"))
      NULL
    else
      getWidget(comp$widget)
  }, error = function(e) NULL)
  if (is.null(w))
    return(invisible(NULL))

  cls <- tryCatch(comp$.box_css_class, error = function(e) NULL)
  needs <- is.null(cls) || length(cls) == 0L || is.na(cls[1]) || !nzchar(cls[1])
  if (needs) {
    comp$.box_css_class <- .next_box_css_class()
    comp$.box_css_provider <- gtkCssProviderNew()
    gtkWidgetAddCssClass(w, comp$.box_css_class)
    ctx <- gtkWidgetGetStyleContext(w)
    gtkStyleContextAddProvider(ctx, comp$.box_css_provider, 800L)
  }

  decls <- .box_model_css_decls(pad, bor)
  rule <- if (nzchar(decls))
    sprintf(".%s { %s }", comp$.box_css_class, decls)
  else
    sprintf(".%s { }", comp$.box_css_class)
  gtkCssProviderLoadFromData(comp$.box_css_provider, rule, -1L)
  invisible(NULL)
}
