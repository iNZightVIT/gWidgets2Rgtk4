## Editable data frame (ColumnView + EditableLabel).
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 gdf", visible = FALSE, width = 520, height = 400)
g <- gvbox(container = w)
glabel("Double-click a cell to edit (EditableLabel).", container = g)
gd <- gdf(mtcars[1:8, c("mpg", "cyl", "hp", "wt")], container = g, expand = TRUE)
sb <- gstatusbar("Ready", container = w)
addHandlerChanged(gd, handler = function(h, ...) {
  svalue(sb) <- sprintf("changed [%s, %s] -> %s", h$i, h$j, h$value)
})
visible(w) <- TRUE
