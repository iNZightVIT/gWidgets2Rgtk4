## Notebook, paned, expand, and layout containers.
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 containers", visible = FALSE, width = 520, height = 400)
nb <- gnotebook(container = w)

## Tab: paned groups
pg <- gpanedgroup(horizontal = TRUE, container = nb, label = "paned")
left <- gvbox(container = pg)
right <- gvbox(container = pg)
glabel("Left pane", container = left)
gtext("Edit me in the left pane.", height = 120, container = left)
glabel("Right pane", container = right)
gbutton("Ping", container = right, handler = function(h, ...) {
  galert("Right pane button", parent = w)
})

## Tab: expand + frame
ex <- gvbox(container = nb, label = "expand")
fr <- gframe("Frame", container = ex)
glabel("Inside a gframe", container = fr)
eg <- gexpandgroup("Click to expand", horizontal = FALSE, container = ex)
glabel("Hidden until expanded", container = eg)
gbutton("OK", container = eg)

## Tab: form layout
fl_tab <- gvbox(container = nb, label = "form")
fl <- gformlayout(container = fl_tab)
gedit(label = "Name", container = fl)
gcombobox(c("A", "B"), label = "Choice", container = fl)
gbutton("Close window", container = fl_tab, handler = function(h, ...) dispose(w))

visible(w) <- TRUE
svalue(nb) <- 1
message("Containers demo open — switch notebook tabs.")
