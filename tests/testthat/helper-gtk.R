## Shared helpers for GTK-backed tests.

has_display <- function() {
  interactive() || nzchar(Sys.getenv("DISPLAY")) || nzchar(Sys.getenv("WAYLAND_DISPLAY"))
}

skip_if_no_display <- function() {
  skip_if_not(has_display(), "GTK requires a display")
}

## Respond to a modal GtkDialog from an idle callback so gtkDialogRun returns.
respond_dialog <- function(dialog, response = -5L, delay_ms = 50L) {
  gTimeoutAdd(as.integer(delay_ms), function() {
    try(gtkDialogResponse(dialog, as.integer(response)), silent = TRUE)
    FALSE
  })
}
