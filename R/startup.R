.onLoad <- function(libname, pkgname) {
  ## GTK init + R event-loop integration (Rgtk4)
  Rgtk4::gtkInit()
  Rgtk4::gtkStartEventLoop()
}

.onAttach <- function(libname, pkgname) {
  load_gwidget_icons()
}
