##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gtext guiWidgetsToolkitRgtk4
.gtext.guiWidgetsToolkitRgtk4 <- function(toolkit, text = NULL, width = NULL, height = 300,
                                          font.attr = NULL, wrap = TRUE, handler = NULL,
                                          action = NULL, container = NULL, ...) {
  GText$new(toolkit, text = text, width = width, height = height, font.attr = font.attr,
            wrap = wrap, handler = handler, action = action, container = container, ...)
}

GText <- setRefClass(
  "GText",
  contains = "GWidget",
  fields = list(buffer = "ANY", font_attr = "list", tag_table = "ANY"),
  methods = list(
    initialize = function(toolkit = NULL, text = NULL, width = NULL, height = 300,
                          font.attr = NULL, wrap = TRUE, handler = NULL, action = NULL,
                          container = NULL, ...) {
      widget <<- gtkTextViewNew()
      buffer <<- gtkTextViewGetBuffer(widget)
      tag_table <<- gtkTextBufferGetTagTable(buffer)
      ## WrapMode: NONE=0 CHAR=1 WORD=2 WORD_CHAR=3
      gtkTextViewSetWrapMode(widget, if (isTRUE(wrap)) 2L else 0L)
      block <<- gtkScrolledWindowNew()
      gtkScrolledWindowSetPolicy(block, .GtkPolicyType$AUTOMATIC, .GtkPolicyType$AUTOMATIC)
      if (!is.null(width))
        gtkWidgetSetSizeRequest(block, as.integer(width), as.integer(height))
      else if (!is.null(height))
        gtkWidgetSetSizeRequest(block, -1L, as.integer(height))
      gtkScrolledWindowSetChild(block, widget)
      font_attr <<- if (is.null(font.attr)) list() else as.list(font.attr)
      if (!is.null(text))
        set_value(text)
      if (length(font_attr) > 0L && !is.null(text) && nzchar(paste(text, collapse = ""))) {
        ## Apply default font.attr to initial buffer content
        start <- gtkTextBufferGetStartIter(buffer)
        end <- gtkTextBufferGetEndIter(buffer)
        for (nm in names(font_attr)) {
          tag_nm <- get_tag_name(nm, font_attr[[nm]])
          gtkTextBufferApplyTagByName(buffer, tag_nm, start, end)
        }
      }
      initFields(change_signal = "changed", default_expand = TRUE, default_fill = TRUE)
      gSignalConnectR(buffer, "changed", function(...) {
        notify_observers(signal = "changed")
      })
      connected_signals[["changed"]] <<- TRUE
      add_to_parent(container, .self, ...)
      handler_id <<- add_handler_changed(handler, action)
      callSuper(toolkit)
    },
    get_value = function(drop = FALSE, ...) {
      txt <- tryCatch({
        start <- gtkTextBufferGetStartIter(buffer)
        end <- gtkTextBufferGetEndIter(buffer)
        as.character(gtkTextBufferGetText(buffer, start, end, TRUE))
      }, error = function(e) "")
      txt
    },
    set_value = function(value, ...) {
      value <- paste(value, collapse = "\n")
      gtkTextBufferSetText(buffer, value, -1L)
    },
    ## Create/lookup a TextTag for one font attribute
    get_tag_name = function(name, value) {
      value <- value[1]
      prop <- name
      tag_val <- value
      switch(name,
             "weight" = {
               w <- tolower(as.character(value))
               tag_val <- if (w %in% names(.PangoWeight)) .PangoWeight[[w]] else 400L
               prop <- "weight"
             },
             "style" = {
               s <- tolower(as.character(value))
               tag_val <- if (s %in% names(.PangoStyle)) .PangoStyle[[s]] else 0L
               prop <- "style"
             },
             "size" = {
               if (is.numeric(value)) {
                 tag_val <- as.numeric(value)
               } else {
                 sc <- tolower(as.character(value))
                 fac <- if (sc %in% names(.PangoScale)) .PangoScale[[sc]] else 1
                 tag_val <- 12 * fac
               }
               prop <- "size-points"
             },
             "scale" = {
               sc <- tolower(as.character(value))
               tag_val <- if (sc %in% names(.PangoScale)) .PangoScale[[sc]] else as.numeric(value)
               prop <- "scale"
             },
             "color" = {
               tag_val <- .color_to_css(value)
               prop <- "foreground"
             },
             "foreground" = {
               tag_val <- .color_to_css(value)
               prop <- "foreground"
             },
             "background" = {
               tag_val <- .color_to_css(value)
               prop <- "background"
             },
             "family" = {
               fam <- as.character(value)
               keyf <- tolower(fam)
               family_map <- .font_map_option("gWidgets2Rgtk4.font.family", .default_font_family)
               tag_val <- if (keyf %in% names(family_map)) family_map[[keyf]] else fam
               prop <- "family"
             },
             {
               prop <- name
               tag_val <- value
             })

      nm <- sprintf("%s-%s", prop, paste(tag_val, collapse = ""))
      existing <- gtkTextTagTableLookup(tag_table, nm)
      if (is.null(existing)) {
        tt <- gtkTextTagNew(nm)
        if (prop %in% c("weight", "style")) {
          gObjectSetEnum(tt, prop, as.integer(tag_val))
        } else if (prop %in% c("size-points", "scale")) {
          gObjectSetDouble(tt, prop, as.numeric(tag_val))
        } else {
          gObjectSetString(tt, prop, as.character(tag_val))
        }
        gtkTextTagTableAdd(tag_table, tt)
      }
      nm
    },
    ## Selection or entire buffer (RGtk2 behaviour)
    set_font = function(value) {
      if (is.null(value))
        return(invisible(NULL))
      ## Raw CSS string → TextView chrome
      if (is.character(value) && length(value) == 1L && is.null(names(value))) {
        callSuper(value)
        return(invisible(NULL))
      }
      if (!is.list(value))
        value <- as.list(value)
      css_extra <- value$css
      value$css <- NULL
      if (length(value) == 0L && (is.null(css_extra) || !nzchar(as.character(css_extra)[1])))
        return(invisible(NULL))

      if (length(value) > 0L) {
        bounds <- gtkTextBufferGetSelectionBounds(buffer)
        has_sel <- isTRUE(as.logical(bounds$result))
        if (!has_sel) {
          start <- gtkTextBufferGetStartIter(buffer)
          end <- gtkTextBufferGetEndIter(buffer)
          gtkTextBufferRemoveAllTags(buffer, start, end)
        } else {
          start <- bounds$start
          end <- bounds$end
        }
        for (i in names(value)) {
          tag_nm <- get_tag_name(i, value[[i]])
          gtkTextBufferApplyTagByName(buffer, tag_nm, start, end)
        }
        .font_info <<- value
      }
      if (!is.null(css_extra) && nzchar(as.character(css_extra)[1]))
        callSuper(list(css = css_extra))
      invisible(NULL)
    },
    insert_text = function(value, where = "end", font.attr = NULL, do.newline = TRUE, ...) {
      if (is.null(value) || !nzchar(paste(value, collapse = "")))
        return()
      value <- paste(value, collapse = "\n")
      if (isTRUE(do.newline))
        value <- paste0(value, "\n")

      where <- match.arg(where, c("end", "beginning", "at.cursor"))
      iter <- switch(where,
                     "end" = gtkTextBufferGetEndIter(buffer),
                     "beginning" = gtkTextBufferGetStartIter(buffer),
                     gtkTextBufferGetIterAtMark(buffer, gtkTextBufferGetInsert(buffer)))

      start_off <- gtkTextIterGetOffset(iter)
      gtkTextBufferInsert(buffer, iter, value, -1L)

      fa <- font.attr
      if (is.null(fa) || length(fa) == 0L) {
        fa <- font_attr
      } else {
        fa <- as.list(fa)
        if (length(font_attr) > 0L)
          fa <- gWidgets2:::merge.list(font_attr, fa)
      }
      if (length(fa) > 0L) {
        start <- gtkTextBufferGetStartIter(buffer)
        gtkTextIterSetOffset(start, as.integer(start_off))
        end <- gtkTextBufferGetStartIter(buffer)
        gtkTextIterSetOffset(end, as.integer(start_off + nchar(value, type = "chars")))
        for (i in names(fa)) {
          tag_nm <- get_tag_name(i, fa[[i]])
          gtkTextBufferApplyTagByName(buffer, tag_nm, start, end)
        }
      }

      if (identical(where, "end")) {
        end <- gtkTextBufferGetEndIter(buffer)
        tryCatch(
          gtkTextViewScrollToIter(widget, end, 0, TRUE, 0, 1),
          error = function(e) invisible(NULL)
        )
      }
      invisible(NULL)
    },
    connect_to_toolkit_signal = function(signal, decorator, emitter = .self$handler_widget()) {
      if (identical(signal, "changed"))
        return()
      callSuper(signal, decorator, emitter)
    },
    style_widget = function() widget
  )
)
