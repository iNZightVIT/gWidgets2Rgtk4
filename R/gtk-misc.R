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

## Parse gWidgets fill (NULL/logical/character) into per-axis flags.
.parse_fill_axes <- function(fill) {
  fill_h <- FALSE
  fill_v <- FALSE
  if (is.null(fill))
    return(list(h = FALSE, v = FALSE))
  if (is.logical(fill)) {
    fill_h <- isTRUE(fill)
    fill_v <- isTRUE(fill)
  } else if (is.character(fill)) {
    fill <- tolower(fill[1])
    if (identical(fill, "true") || identical(fill, "both")) {
      fill_h <- TRUE
      fill_v <- TRUE
    } else {
      fill_h <- fill %in% c("x")
      fill_v <- fill %in% c("y")
    }
  }
  list(h = fill_h, v = fill_v)
}

## gWidgets anchor in [-1,1]^2 → GTK Align (flip y: gWidgets +1 is top).
.anchor_to_align <- function(anchor) {
  ax <- as.numeric(anchor[1])
  ay <- as.numeric(anchor[2])
  list(
    h = if (ax < -0.33) .GtkAlign$START else if (ax > 0.33) .GtkAlign$END else .GtkAlign$CENTER,
    v = if (ay > 0.33) .GtkAlign$START else if (ay < -0.33) .GtkAlign$END else .GtkAlign$CENTER
  )
}

## Map gWidgets expand/fill/anchor onto GTK4 widget properties.
##
## GTK4 compute_expand propagates child expand up the tree. Setting the
## *cross-axis* expand flag (e.g. vexpand inside a horizontal GtkBox) makes
## toolbars/menubars steal vertical space from content — the "skinny tall
## buttons" failure mode. Match GTK2 packStart: expand only along the box
## axis; use halign/valign FILL for fill (including cross-axis stretch within
## the allocation). Pin expand=FALSE explicitly so children cannot poison
## parents via compute_expand.
##
## horizontal: TRUE = horizontal box, FALSE = vertical box, NA = GtkGrid
## (expand on each axis fill requests, as gtk_table Attach did).
set_child_expand_fill_anchor <- function(child, expand = FALSE, fill = NULL,
                                         anchor = NULL, horizontal = TRUE,
                                         padding = 0L) {
  expand <- isTRUE(expand)
  grid <- isTRUE(is.na(horizontal))

  if (expand) {
    if (is.null(fill))
      fill <- if (is.null(anchor)) "both" else ""
  }
  axes <- .parse_fill_axes(fill)
  fill_h <- axes$h
  fill_v <- axes$v

  if (grid) {
    ## GtkGrid / historical gtk_table Attach: expand only on axes fill requests
    if (expand) {
      gtkWidgetSetHexpand(child, fill_h)
      gtkWidgetSetVexpand(child, fill_v)
    } else {
      gtkWidgetSetHexpand(child, FALSE)
      gtkWidgetSetVexpand(child, FALSE)
    }
  } else if (horizontal) {
    gtkWidgetSetHexpand(child, expand)
    ## Never vexpand from box packing — blocks toolbar/menubar poison
    gtkWidgetSetVexpand(child, FALSE)
  } else {
    gtkWidgetSetVexpand(child, expand)
    gtkWidgetSetHexpand(child, FALSE)
  }

  ## Align: FILL wins on axes being filled; else anchor; else leave default
  an <- if (!is.null(anchor)) .anchor_to_align(anchor) else NULL
  if (fill_h)
    gtkWidgetSetHalign(child, .GtkAlign$FILL)
  else if (!is.null(an))
    gtkWidgetSetHalign(child, an$h)

  if (fill_v)
    gtkWidgetSetValign(child, .GtkAlign$FILL)
  else if (!is.null(an))
    gtkWidgetSetValign(child, an$v)

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
## Keystroke helpers (GtkEventControllerKey)
## ---------------------------------------------------------------------------

## GDK modifier bits (subset used by gWidgets handlers)
.GdkModifier <- list(
  SHIFT = 1L,
  LOCK = 2L,
  CONTROL = 4L,
  ALT = 8L,
  BUTTON1 = 256L,
  BUTTON2 = 512L,
  BUTTON3 = 1024L,
  SUPER = 67108864L,
  HYPER = 134217728L,
  META = 268435456L
)

.keyval_to_string <- function(keyval) {
  keyval <- as.integer(keyval)[1]
  if (is.na(keyval))
    return("")
  uni <- tryCatch(as.integer(gdkKeyvalToUnicode(keyval))[1], error = function(e) 0L)
  if (!is.na(uni) && uni > 0L)
    return(intToUtf8(uni))
  nm <- tryCatch(as.character(gdkKeyvalName(keyval))[1], error = function(e) "")
  if (is.null(nm) || is.na(nm)) "" else nm
}

.gdk_state_to_modifier <- function(state) {
  state <- as.integer(state)[1]
  if (is.na(state) || state == 0L)
    return(NULL)
  mods <- character()
  if (bitwAnd(state, .GdkModifier$SHIFT) != 0L) mods <- c(mods, "shift")
  if (bitwAnd(state, .GdkModifier$CONTROL) != 0L) mods <- c(mods, "control")
  if (bitwAnd(state, .GdkModifier$ALT) != 0L) mods <- c(mods, "mod1")
  if (bitwAnd(state, .GdkModifier$SUPER) != 0L ||
      bitwAnd(state, .GdkModifier$META) != 0L) mods <- c(mods, "super")
  if (!length(mods)) NULL else mods
}

## Best-effort widget size: allocation → size-request → (-1,-1)
.widget_get_size <- function(w) {
  if (is.null(w) || is(w, "uninitializedField"))
    return(c(width = -1L, height = -1L))
  aw <- tryCatch(as.integer(gtkWidgetGetAllocatedWidth(w))[1], error = function(e) NA_integer_)
  ah <- tryCatch(as.integer(gtkWidgetGetAllocatedHeight(w))[1], error = function(e) NA_integer_)
  if (!is.na(aw) && !is.na(ah) && aw > 0L && ah > 0L)
    return(c(width = aw, height = ah))
  ww <- tryCatch(as.integer(gtkWidgetGetWidth(w))[1], error = function(e) NA_integer_)
  wh <- tryCatch(as.integer(gtkWidgetGetHeight(w))[1], error = function(e) NA_integer_)
  if (!is.na(ww) && !is.na(wh) && ww > 0L && wh > 0L)
    return(c(width = ww, height = wh))
  req <- tryCatch(gtkWidgetGetSizeRequest(w), error = function(e) NULL)
  if (!is.null(req)) {
    rw <- as.integer(req$width)[1]
    rh <- as.integer(req$height)[1]
    if (!is.na(rw) && !is.na(rh) && (rw > 0L || rh > 0L))
      return(c(width = max(rw, -1L), height = max(rh, -1L)))
  }
  c(width = -1L, height = -1L)
}

##' Set a pointer (hand) cursor on a component (hover polish).
##' @param obj A gWidgets GComponent.
##' @param name GDK cursor name (default \code{"pointer"}).
##' @export
setPointerCursor <- function(obj, name = "pointer") {
  w <- if (is(obj, "GComponent")) obj$style_widget() else getWidget(obj)
  if (!is.null(w))
    tryCatch(gtkWidgetSetCursorFromName(w, as.character(name)[1]),
             error = function(e) invisible(NULL))
  invisible(NULL)
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
