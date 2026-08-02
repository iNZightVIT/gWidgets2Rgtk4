##' @include GWidget.R
NULL

##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .gtimer guiWidgetsToolkitRgtk4
.gtimer.guiWidgetsToolkitRgtk4 <- function(toolkit, ms, FUN, data = NULL,
                                           one.shot = FALSE, start = TRUE) {
  GTimer$new(toolkit, ms, FUN, data = data, one.shot = one.shot, start = start)
}

##' Timer class
##' @rdname gWidgets2Rgtk4-package
GTimer <- setRefClass(
  "GTimer",
  fields = list(
    oneShot = "logical",
    started = "logical",
    interval = "integer",
    data = "ANY",
    FUN = "ANY",
    FUN_wrapper = "ANY",
    ID = "ANY"
  ),
  methods = list(
    initialize = function(toolkit = guiToolkit(), ms, FUN = function(...) {},
                          data = NULL, one.shot = FALSE, start = TRUE) {
      f <- function() {
        FUN(data)
        if (oneShot) {
          stop_timer()
          FALSE
        } else {
          TRUE
        }
      }
      initFields(
        started = FALSE,
        interval = as.integer(ms),
        oneShot = one.shot,
        data = data,
        FUN = FUN,
        FUN_wrapper = f
      )
      if (start)
        start_timer()
      callSuper()
    },
    set_interval = function(ms) {
      interval <<- as.integer(ms)
    },
    start_timer = function() {
      if (!started) {
        ID <<- gTimeoutAdd(interval, FUN_wrapper)
      }
      started <<- TRUE
    },
    stop_timer = function() {
      if (started && !is.null(ID) && !is(ID, "uninitializedField")) {
        try(gSourceRemove(ID), silent = TRUE)
      }
      started <<- FALSE
    }
  )
)
