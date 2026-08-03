## Smoke demo for the Rgtk4 toolkit backend of gWidgets2.
## Also see: demo(chrome), demo(controls), demo(containers), demo(dialogs), demo(misc)
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 smoke", width = 360, height = 280)
g <- gvbox(container = w, spacing = 8)
glabel("Phase 1+2 smoke dialog", container = g)
e <- gedit("edit me", container = g)
cb <- gcheckbox("checked?", checked = TRUE, container = g)
out <- glabel("", container = g)

gbutton("Update label", container = g, handler = function(h, ...) {
  svalue(out) <- sprintf("edit=%s; check=%s", svalue(e), svalue(cb))
})

gbutton("Open chrome demo…", container = g, handler = function(h, ...) {
  demo("chrome", package = "gWidgets2Rgtk4", character.only = TRUE)
})

gbutton("Close", container = g, handler = function(h, ...) {
  dispose(w)
})

message("Smoke window open. Other demos: chrome, controls, containers, dialogs, misc.")
