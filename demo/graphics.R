## Embeddable graphics via unigd + GtkPicture (Phase 2 spike).
require(gWidgets2)
options(guiToolkit = "Rgtk4")

if (!requireNamespace("unigd", quietly = TRUE))
  stop("This demo needs the unigd package: install.packages('unigd')")

w <- gwindow("gWidgets2Rgtk4 graphics", visible = FALSE, width = 520, height = 420)
g <- gvbox(container = w)
glabel("Plot goes to the unigd device; the pane refreshes via PNG blit.", container = g)
gg <- ggraphics(width = 480, height = 320, container = g, expand = TRUE)

btn_row <- ggroup(container = g)
gbutton("plot(1:10)", container = btn_row, handler = function(...) {
  visible(gg) <- TRUE
  plot(1:10, main = "ggraphics / unigd")
})
gbutton("hist(rnorm)", container = btn_row, handler = function(...) {
  visible(gg) <- TRUE
  hist(rnorm(200), col = "steelblue", main = "histogram")
})
gbutton("Refresh", container = btn_row, handler = function(...) {
  gg$sync_from_device(force = TRUE)
})

visible(w) <- TRUE
