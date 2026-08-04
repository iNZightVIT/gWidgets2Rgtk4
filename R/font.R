##' @include dnd.R
NULL

## ---------------------------------------------------------------------------
## Font / CSS styling for GTK4
##
## Widget look uses GtkCssProvider (modifyFont etc. removed in GTK4).
## Portable font<- maps to CSS; css<- / loadCss / addCssClass are toolkit escapes.
## ---------------------------------------------------------------------------

.font_env <- new.env(parent = emptyenv())
.font_env$id <- 0L
.font_env$display_provider <- NULL
.font_env$display_css <- ""
.font_env$app_css_loaded <- FALSE

## Pango-compatible scale factors (same as RGtk2 pangoManuals.R)
.PangoScale <- c(
  "xx-large" = 1.2 * 1.2 * 1.2,
  "x-large"  = 1.2 * 1.2,
  "large"    = 1.2,
  "medium"   = 1,
  "small"    = 1 / 1.2,
  "x-small"  = 1 / (1.2 * 1.2),
  "xx-small" = 1 / (1.2 * 1.2 * 1.2)
)

## Default maps (RGtk2 / gWidgets2 shaped); options() may overlay
.default_font_family <- c(
  sans = "sans-serif",
  helvetica = "Helvetica",
  times = "serif",
  monospace = "monospace"
)

.default_font_weight <- c(
  light = "300",
  normal = "normal",
  medium = "500",
  bold = "bold",
  heavy = "800"
)

.default_font_style <- c(
  normal = "normal",
  oblique = "oblique",
  italic = "italic"
)

## PangoWeight / PangoStyle integers for GtkTextTag
.PangoWeight <- c(
  ultralight = 200L, light = 300L, normal = 400L, bold = 700L,
  ultrabold = 800L, heavy = 900L, medium = 500L
)

.PangoStyle <- c(normal = 0L, oblique = 1L, italic = 2L)

.font_map_option <- function(option_name, defaults) {
  opt <- getOption(option_name)
  if (is.null(opt))
    return(defaults)
  out <- defaults
  if (!is.null(names(opt))) {
    for (nm in names(opt))
      out[[nm]] <- unname(opt[[nm]])
  }
  out
}

.color_to_css <- function(val) {
  val <- as.character(val)[1]
  if (grepl("^#", val) || grepl("^rgb", val, ignore.case = TRUE))
    return(val)
  rgb <- tryCatch(grDevices::col2rgb(val), error = function(e) NULL)
  if (is.null(rgb))
    return(val)
  sprintf("#%02x%02x%02x", rgb[1], rgb[2], rgb[3])
}

##' Map a gWidgets2 font specification to CSS declarations (no selector).
##' @noRd
font_spec_to_css <- function(value) {
  if (is.null(value) || (is.character(value) && length(value) == 1L &&
                         is.null(names(value)))) {
    return("")
  }
  if (!is.list(value))
    value <- as.list(value)
  value <- value[names(value) != "css" & nzchar(names(value))]
  if (!length(value))
    return("")

  family_map <- .font_map_option("gWidgets2Rgtk4.font.family", .default_font_family)
  weight_map <- .font_map_option("gWidgets2Rgtk4.font.weight", .default_font_weight)
  style_map <- .font_map_option("gWidgets2Rgtk4.font.style", .default_font_style)
  scale_base <- as.numeric(getOption("gWidgets2Rgtk4.font.scale.base", 10))[1]
  if (is.na(scale_base) || scale_base <= 0)
    scale_base <- 10

  decls <- character()
  for (key in names(value)) {
    val <- value[[key]]
    if (is.null(val) || (length(val) == 1L && is.na(val)))
      next
    val <- val[1]
    switch(key,
           "weight" = {
             w <- tolower(as.character(val))
             decls <- c(decls, sprintf("font-weight: %s",
                                       if (w %in% names(weight_map)) weight_map[[w]] else w))
           },
           "style" = {
             s <- tolower(as.character(val))
             decls <- c(decls, sprintf("font-style: %s",
                                       if (s %in% names(style_map)) style_map[[s]] else s))
           },
           "family" = {
             fam <- as.character(val)
             keyf <- tolower(fam)
             resolved <- if (keyf %in% names(family_map)) family_map[[keyf]] else fam
             decls <- c(decls, sprintf("font-family: \"%s\"", resolved))
           },
           "size" = {
             decls <- c(decls, sprintf("font-size: %spt", as.numeric(val)))
           },
           "scale" = {
             sc <- tolower(as.character(val))
             fac <- if (sc %in% names(.PangoScale)) .PangoScale[[sc]] else 1
             decls <- c(decls, sprintf("font-size: %spt", round(scale_base * fac, 2)))
           },
           "color" = ,
           "foreground" = {
             decls <- c(decls, sprintf("color: %s", .color_to_css(val)))
           },
           "background" = {
             decls <- c(decls, sprintf("background-color: %s", .color_to_css(val)))
           },
           ## ignore unknown keys
           NULL
           )
  }
  if (!length(decls))
    return("")
  paste0(paste(decls, collapse = "; "), ";")
}

.next_font_class <- function() {
  .font_env$id <- as.integer(.font_env$id + 1L)
  sprintf("gw-font-%d", .font_env$id)
}

.combine_css_decls <- function(font_decls, extra_decls, css_field) {
  parts <- c(font_decls, css_field, extra_decls)
  parts <- parts[!is.null(parts) & nzchar(as.character(parts))]
  if (!length(parts))
    return("")
  ## normalize trailing semicolons
  parts <- vapply(parts, function(p) {
    p <- trimws(as.character(p)[1])
    if (!nzchar(p)) return("")
    if (!grepl(";\\s*$", p)) paste0(p, ";") else p
  }, character(1))
  paste(parts[nzchar(parts)], collapse = " ")
}

##' Build a CSS rule for a per-widget class (styles widget and descendant labels).
##' @noRd
.font_class_rule <- function(class_name, decls) {
  decls <- trimws(as.character(decls)[1])
  if (!nzchar(decls))
    return("")
  if (!grepl(";\\s*$", decls))
    decls <- paste0(decls, ";")
  sprintf(".%s, .%s label { %s }", class_name, class_name, decls)
}

##' Ensure a GComponent has a CssProvider + unique class on its style target.
##' @noRd
.ensure_css_state <- function(comp) {
  cls <- tryCatch(comp$.css_class, error = function(e) NULL)
  needs <- is.null(cls) || length(cls) == 0L || is.na(cls[1]) || !nzchar(cls[1])
  if (needs) {
    comp$.css_class <- .next_font_class()
    comp$.css_provider <- gtkCssProviderNew()
    if (is.null(comp$.css_font_decls) || length(comp$.css_font_decls) == 0L ||
        is.na(comp$.css_font_decls[1]))
      comp$.css_font_decls <- ""
    if (is.null(comp$.css_extra_decls) || length(comp$.css_extra_decls) == 0L ||
        is.na(comp$.css_extra_decls[1]))
      comp$.css_extra_decls <- ""
    if (is.null(comp$.font_info))
      comp$.font_info <- list()
    target <- comp$style_widget()
    if (!is.null(target)) {
      gtkWidgetAddCssClass(target, comp$.css_class)
      ctx <- gtkWidgetGetStyleContext(target)
      gtkStyleContextAddProvider(ctx, comp$.css_provider, 800L)
    }
  }
  invisible(NULL)
}

##' Reload the per-widget CssProvider from stored declaration pieces.
##' @noRd
.refresh_widget_css <- function(comp) {
  .ensure_css_state(comp)
  decls <- .combine_css_decls(comp$.css_font_decls, comp$.css_extra_decls, NULL)
  rule <- .font_class_rule(comp$.css_class, decls)
  if (!nzchar(rule))
    rule <- sprintf(".%s { }", comp$.css_class)
  gtkCssProviderLoadFromData(comp$.css_provider, rule, -1L)
  invisible(NULL)
}

##' Apply raw CSS declarations via the component's provider (used by css<-).
##' @noRd
.apply_extra_css <- function(comp, decls) {
  .ensure_css_state(comp)
  comp$.css_extra_decls <- if (is.null(decls)) "" else as.character(decls)[1]
  .refresh_widget_css(comp)
}

##' Load or replace the display-level stylesheet.
##'
##' @param css Character CSS (selectors + rules). Replaces previous \code{loadCss} content.
##' @export
loadCss <- function(css) {
  css <- paste(as.character(css), collapse = "\n")
  if (is.null(.font_env$display_provider)) {
    .font_env$display_provider <- gtkCssProviderNew()
    display <- gdkDisplayGetDefault()
    ## Below per-widget (800) so font<- / css<- win
    gtkStyleContextAddProviderForDisplay(display, .font_env$display_provider, 600L)
  }
  .font_env$display_css <- css
  gtkCssProviderLoadFromData(.font_env$display_provider, css, -1L)
  invisible(NULL)
}

##' Ensure options("gWidgets2Rgtk4.css") is applied once.
##' @noRd
.ensure_app_css <- function() {
  if (isTRUE(.font_env$app_css_loaded))
    return(invisible(NULL))
  .font_env$app_css_loaded <- TRUE
  sheet <- getOption("gWidgets2Rgtk4.css")
  if (!is.null(sheet) && nzchar(paste(sheet, collapse = "")))
    loadCss(sheet)
  invisible(NULL)
}

.style_target <- function(obj) {
  if (is(obj, "GComponent"))
    obj$style_widget()
  else
    getWidget(obj)
}

##' Add a CSS class to a gWidgets2Rgtk4 component's style target.
##' @param obj A gWidgets component (GComponent).
##' @param class_name CSS class name (without leading dot).
##' @export
addCssClass <- function(obj, class_name) {
  w <- .style_target(obj)
  if (!is.null(w))
    gtkWidgetAddCssClass(w, as.character(class_name)[1])
  invisible(NULL)
}

##' Remove a CSS class from a gWidgets2Rgtk4 component's style target.
##' @inheritParams addCssClass
##' @export
removeCssClass <- function(obj, class_name) {
  w <- .style_target(obj)
  if (!is.null(w))
    gtkWidgetRemoveCssClass(w, as.character(class_name)[1])
  invisible(NULL)
}

##' Get or set raw CSS declarations on a component.
##'
##' Declarations only (no selector), e.g. \code{"padding: 6px; color: red;"}.
##' Composes with \code{font<-} on the same per-widget CSS provider.
##'
##' @param obj A gWidgets component.
##' @param value CSS declarations (no selector), e.g. \code{"padding: 6px;"}.
##' @export
css <- function(obj) UseMethod("css")

##' @export
##' @rdname css
css.default <- function(obj) {
  if (is(obj, "GComponent"))
    obj$get_css()
  else
    ""
}

##' @export
##' @rdname css
"css<-" <- function(obj, value) UseMethod("css<-")

##' @export
##' @rdname css
"css<-.default" <- function(obj, value) {
  if (is(obj, "GComponent"))
    obj$set_css(value)
  obj
}
