## Retest: gdf column header drag (after GestureClick strip fix).
##   cd gWidgets2Rgtk4 && Rscript -e 'devtools::load_all("."); source("demo/gdf-column-dnd.R")'
##
## Expect:
## 1. Console: headers=3 drag_sources=3 gesture_clicks_on_titles=0
## 2. Press-and-drag a column header — drag icon/cursor should appear
## 3. Drop on V1 — entry shows column name; status updates
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gdf column DnD", visible = FALSE, width = 560, height = 360)
sb <- gstatusbar("Drag a column header onto V1", container = w)
pg <- gpanedgroup(horizontal = TRUE, container = w)

left <- gvbox(container = pg, spacing = 4)
glabel("gdf — drag column headers", container = left)
gd <- gdf(mtcars[1:5, c("mpg", "cyl", "hp")], container = left, expand = TRUE)
gd$add_dnd_columns()

right <- gvbox(container = pg, spacing = 6)
glabel("Drop target (iNZight-shaped)", container = right)
fl <- gformlayout(container = right)
v1 <- gedit("", label = "V1", container = fl)
addDropTarget(v1, handler = function(h, ...) {
  svalue(h$obj) <- as.character(h$dropdata)[1]
  svalue(sb) <- sprintf("dropped: %s", svalue(h$obj))
})

gbutton("Re-call add_dnd_columns()", container = right, handler = function(...) {
  gd$add_dnd_columns()
  svalue(sb) <- "add_dnd_columns() called again"
})
gbutton("set_frame refresh + add_dnd", container = right, handler = function(...) {
  gd$set_frame(mtcars[1:5, c("mpg", "cyl", "hp", "wt")])
  gd$add_dnd_columns()
  svalue(sb) <- "frame refreshed; DnD re-wired"
})

.report_wiring <- function() {
  titles <- gWidgets2Rgtk4:::.dnd_columnview_header_titles(gd$widget)
  n_src <- 0L
  n_click <- 0L
  for (t in titles) {
    m <- gtkWidgetObserveControllers(t)
    n <- as.integer(gListModelGetNItems(m))[1]
    for (i in seq_len(n) - 1L) {
      ctrl <- gListModelGetObject(m, as.integer(i))
      if (gWidgets2Rgtk4:::.dnd_is_drag_source(ctrl))
        n_src <- n_src + 1L
      if (gWidgets2Rgtk4:::.dnd_is_gesture_click(ctrl))
        n_click <- n_click + 1L
    }
  }
  hdr <- gWidgets2Rgtk4:::.dnd_columnview_header_row(gd$widget)
  n_hdr_drag <- 0L
  if (!is.null(hdr)) {
    m <- gtkWidgetObserveControllers(hdr)
    n <- as.integer(gListModelGetNItems(m))[1]
    for (i in seq_len(n) - 1L) {
      ctrl <- gListModelGetObject(m, as.integer(i))
      if (gWidgets2Rgtk4:::.dnd_is_gesture_drag(ctrl))
        n_hdr_drag <- n_hdr_drag + 1L
    }
  }
  message(sprintf(
    "headers=%d drag_sources=%d gesture_clicks_on_titles=%d header_gesture_drags=%d reorderable=%s",
    length(titles), n_src, n_click, n_hdr_drag,
    gtkColumnViewGetReorderable(gd$widget)
  ))
}

gTimeoutAdd(100L, function() {
  .report_wiring()
  FALSE
})

visible(w) <- TRUE
message("Drag a column header onto V1. Close the window when done.")
