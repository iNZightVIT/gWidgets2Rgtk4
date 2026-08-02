.onLoad <- function(libname, pkgname) {
  ## GTK init + R event-loop integration (Rgtk4)
  Rgtk4::gtkInit()
  Rgtk4::gtkStartEventLoop()
}

.onAttach <- function(libname, pkgname) {
  ## Icon loading lands with icons.R in Phase 1
}
