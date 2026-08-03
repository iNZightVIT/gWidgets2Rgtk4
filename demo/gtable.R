## Selection table (GtkColumnView).
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 gtable", visible = FALSE, width = 520, height = 400)
g <- gvbox(container = w)
glabel("Click a row; selection appears below.", container = g)
tbl <- gtable(mtcars[1:10, c("mpg", "cyl", "hp", "wt")], container = g, expand = TRUE)
sb <- gstatusbar("Ready", container = w)
addHandlerSelectionChanged(tbl, handler = function(h, ...) {
  svalue(sb) <- sprintf("selected: %s", paste(svalue(h$obj), collapse = ", "))
})
addHandlerDoubleclick(tbl, handler = function(h, ...) {
  svalue(sb) <- sprintf("activated: %s", paste(svalue(h$obj), collapse = ", "))
})
visible(w) <- TRUE
