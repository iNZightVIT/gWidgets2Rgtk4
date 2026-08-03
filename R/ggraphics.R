##' @include GWidget.R
NULL

##' Toolkit constructor for an embeddable graphics device
##'
##' Uses [unigd::ugd()] as a cross-platform R graphics device and blits
##' rendered PNGs into a GTK4 `GtkPicture`. Not interactive (no rubber-band
##' / locator); a later httpgd + WebView path is planned once iNZight boots.
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .ggraphics guiWidgetsToolkitRgtk4
.ggraphics.guiWidgetsToolkitRgtk4 <- function(toolkit,
                                              width = dpi * 6, height = dpi * 6,
                                              dpi = 75, ps = 12,
                                              handler = NULL, action = NULL,
                                              container = NULL, ...) {
  GGraphics$new(toolkit,
                width = width, height = height, dpi = dpi, ps = ps,
                handler = handler, action = action, container = container, ...)
}

##' Graphics device widget (unigd + GtkPicture)
##' @rdname gWidgets2Rgtk4-package
GGraphics <- setRefClass(
  "GGraphics",
  contains = "GWidget",
  fields = list(
    device_number = "ANY",
    dpi = "numeric",
    ps = "numeric",
    default_width = "numeric",
    default_height = "numeric",
    poll_id = "ANY",
    last_upid = "ANY",
    last_hsize = "ANY",
    last_view_w = "ANY",
    last_view_h = "ANY",
    render_dir = "character"
  ),
  methods = list(
    initialize = function(toolkit = NULL,
                          width = dpi * 6, height = dpi * 6,
                          dpi = 75, ps = 12,
                          handler = NULL, action = NULL,
                          container = NULL, ...) {
      if (!requireNamespace("unigd", quietly = TRUE))
        stop("ggraphics requires the 'unigd' package", call. = FALSE)

      w_px <- as.integer(max(1, round(width)))
      h_px <- as.integer(max(1, round(height)))

      pic <- gtkPictureNew()
      gtkPictureSetCanShrink(pic, TRUE)
      ## 1 = GTK_CONTENT_FIT_CONTAIN
      gtkPictureSetContentFit(pic, 1L)
      gtkWidgetSetHexpand(pic, TRUE)
      gtkWidgetSetVexpand(pic, TRUE)
      gtkWidgetSetSizeRequest(pic, w_px, h_px)

      widget <<- pic
      block <<- widget

      initFields(
        dpi = dpi,
        ps = ps,
        default_width = w_px,
        default_height = h_px,
        last_upid = -1L,
        last_hsize = -1L,
        last_view_w = -1L,
        last_view_h = -1L,
        render_dir = tempfile("ggraphics_"),
        poll_id = NULL
      )
      dir.create(render_dir, showWarnings = FALSE)

      unigd::ugd(width = w_px, height = h_px, pointsize = ps)
      ## Keep the "unigd" name — unigd::ugd_*() requires names(which) == "unigd"
      device_number <<- grDevices::dev.cur()

      start_poll()
      self <- .self
      gSignalConnectR(widget, "destroy", function(w) {
        self$teardown_device()
      })

      add_to_parent(container, .self, ...)
      callSuper(toolkit)
    },

    ## Named device id for unigd APIs
    device_which = function() {
      wh <- as.integer(device_number)
      names(wh) <- "unigd"
      wh
    },

    device_alive = function() {
      dn <- as.integer(device_number)
      dn %in% as.integer(grDevices::dev.list())
    },

    start_poll = function() {
      ## Poll unigd state + widget size; blit when the plot or size changes.
      poll_id <<- gTimeoutAdd(200L, function() {
        if (!is_extant()) {
          teardown_device()
          return(FALSE)
        }
        sync_from_device()
        TRUE
      })
    },

    stop_poll = function() {
      if (!is.null(poll_id) && !is(poll_id, "uninitializedField")) {
        try(gSourceRemove(poll_id), silent = TRUE)
        poll_id <<- NULL
      }
    },

    teardown_device = function() {
      stop_poll()
      if (device_alive()) {
        try(unigd::ugd_close(which = device_which()), silent = TRUE)
      }
      if (nzchar(render_dir) && dir.exists(render_dir))
        unlink(render_dir, recursive = TRUE)
    },

    view_size = function() {
      w <- tryCatch(as.integer(gtkWidgetGetWidth(widget)), error = function(e) 0L)
      h <- tryCatch(as.integer(gtkWidgetGetHeight(widget)), error = function(e) 0L)
      if (is.na(w) || w < 50L) w <- as.integer(default_width)
      if (is.na(h) || h < 50L) h <- as.integer(default_height)
      c(width = w, height = h)
    },

    sync_from_device = function(force = FALSE) {
      if (!device_alive())
        return(invisible(FALSE))
      st <- tryCatch(unigd::ugd_state(which = device_which()),
                     error = function(e) NULL)
      sz <- view_size()
      changed <- isTRUE(force) ||
        is.null(st) ||
        !identical(as.integer(st$upid), as.integer(last_upid)) ||
        !identical(as.integer(st$hsize), as.integer(last_hsize)) ||
        !identical(as.integer(sz["width"]), as.integer(last_view_w)) ||
        !identical(as.integer(sz["height"]), as.integer(last_view_h))
      if (!changed)
        return(invisible(FALSE))
      if (!is.null(st)) {
        last_upid <<- as.integer(st$upid)
        last_hsize <<- as.integer(st$hsize)
      }
      last_view_w <<- as.integer(sz["width"])
      last_view_h <<- as.integer(sz["height"])
      ## No pages yet — leave blank
      if (!is.null(st) && isTRUE(st$hsize < 1L))
        return(invisible(FALSE))
      refresh(sz["width"], sz["height"])
    },

    refresh = function(width = NULL, height = NULL) {
      if (!device_alive())
        return(invisible(FALSE))
      if (is.null(width) || is.null(height)) {
        sz <- view_size()
        width <- sz["width"]
        height <- sz["height"]
      }
      png <- tryCatch(
        unigd::ugd_render(
          width = as.integer(width),
          height = as.integer(height),
          as = "png",
          which = device_which()
        ),
        error = function(e) NULL
      )
      if (is.null(png) || !length(png))
        return(invisible(FALSE))
      f <- file.path(render_dir, "current.png")
      writeBin(png, f)
      gtkPictureSetFilename(widget, f)
      invisible(TRUE)
    },

    set_visible = function(value, ...) {
      ## Match gWidgets2RGtk2: visible<-TRUE activates this device (notebook tabs).
      if (isTRUE(as.logical(value)) && device_alive()) {
        grDevices::dev.set(as.integer(device_number))
        sync_from_device(force = TRUE)
      }
      invisible(value)
    },

    ## Handlers deferred — interactivity / rubber-band not in this spike
    add_handler_changed = function(handler, action = NULL, ...) {
      if (is_handler(handler))
        invisible(NULL)
    },
    add_handler_clicked = function(handler, action = NULL, ...) {
      if (is_handler(handler))
        invisible(NULL)
    },
    add_handler_mouse_motion = function(handler, action = NULL, ...) {
      if (is_handler(handler))
        invisible(NULL)
    }
  )
)
