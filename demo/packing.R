## Visual tour of expand / fill / anchor packing.
##
##   cd gWidgets2Rgtk4
##   Rscript -e 'pkgload::load_all("."); source("demo/packing.R")'
## Or after install: demo("packing", package = "gWidgets2Rgtk4")

require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 packing", visible = FALSE, width = 720, height = 560)
sb <- gstatusbar("Resize the window — expand/fill children grow; anchored ones stay at edges",
                 container = w)
nb <- gnotebook(container = w)

## --- Nine-spot anchors ----------------------------------------------------
## Child sits in one of 9 edge/corner/center spots when it does not fill.
anchor_page <- gvbox(container = nb, label = "anchors", spacing = 6, expand = TRUE)
glabel("ggroup with expand=TRUE + fill=\"\" + anchor = c(x, y) in [-1, 1]^2",
       container = anchor_page)
glabel("x: -1 left … +1 right    y: +1 top … -1 bottom",
       container = anchor_page)

anchors <- list(
  NW = c(-1, 1), N = c(0, 1), NE = c(1, 1),
  W  = c(-1, 0), C = c(0, 0), E  = c(1, 0),
  SW = c(-1, -1), S = c(0, -1), SE = c(1, -1)
)
grid <- glayout(homogeneous = TRUE, spacing = 4, container = anchor_page,
                expand = TRUE, fill = TRUE)
ii <- 0L
for (nm in names(anchors)) {
  ii <- ii + 1L
  row <- ((ii - 1L) %/% 3L) + 1L
  col <- ((ii - 1L) %% 3L) + 1L
  ## cell created without container; attached once via [<- 
  cell <- ggroup(horizontal = FALSE)
  size(cell) <- c(180, 100)
  gbutton(sprintf("%s  (%s)", nm, paste(anchors[[nm]], collapse = ",")),
          container = cell, expand = TRUE, fill = "",
          anchor = anchors[[nm]])
  grid[row, col, expand = TRUE, fill = TRUE] <- cell
}

## --- Horizontal expand / fill ---------------------------------------------
h_page <- gvbox(container = nb, label = "h expand/fill", spacing = 8, expand = TRUE)
glabel("Horizontal ggroup — resize the window width", container = h_page)

mk_h_row <- function(title, expand, fill) {
  fr <- gframe(title, container = h_page, expand = TRUE, fill = TRUE)
  g <- ggroup(horizontal = TRUE, container = fr, expand = TRUE, fill = TRUE)
  size(g) <- c(-1, 48)
  gbutton("fixed", container = g)
  gbutton(sprintf("expand=%s fill=%s", expand, deparse(fill)),
          container = g, expand = expand, fill = fill)
  gbutton("fixed", container = g)
}

mk_h_row("no expand", FALSE, FALSE)
mk_h_row("expand + fill=\"x\"", TRUE, "x")
mk_h_row("expand + fill=\"y\"", TRUE, "y")
mk_h_row("expand + fill=\"both\"", TRUE, "both")
fr <- gframe("expand + fill=\"\" + anchors (L / C / R)", container = h_page,
             expand = TRUE, fill = TRUE)
g <- ggroup(horizontal = TRUE, container = fr, expand = TRUE, fill = TRUE)
size(g) <- c(-1, 48)
gbutton("L", container = g, expand = TRUE, fill = "", anchor = c(-1, 0))
gbutton("C", container = g, expand = TRUE, fill = "", anchor = c(0, 0))
gbutton("R", container = g, expand = TRUE, fill = "", anchor = c(1, 0))

## --- Vertical expand / fill -----------------------------------------------
v_page <- ggroup(horizontal = TRUE, container = nb, label = "v expand/fill",
                 spacing = 8, expand = TRUE, fill = TRUE)
glabel("Vertical gvbox —\nresize height", container = v_page)

mk_v_col <- function(title, expand, fill) {
  fr <- gframe(title, container = v_page, expand = TRUE, fill = TRUE)
  g <- gvbox(container = fr, expand = TRUE, fill = TRUE)
  size(g) <- c(120, -1)
  gbutton("fixed", container = g)
  gbutton(sprintf("e=%s\nf=%s", expand, deparse(fill)),
          container = g, expand = expand, fill = fill)
  gbutton("fixed", container = g)
}

mk_v_col("none", FALSE, FALSE)
mk_v_col("fill y", TRUE, "y")
mk_v_col("fill x", TRUE, "x")
mk_v_col("both", TRUE, "both")

fr <- gframe("anchors T/C/B", container = v_page, expand = TRUE, fill = TRUE)
g <- gvbox(container = fr, expand = TRUE, fill = TRUE)
gbutton("T", container = g, expand = TRUE, fill = "", anchor = c(0, 1))
gbutton("C", container = g, expand = TRUE, fill = "", anchor = c(0, 0))
gbutton("B", container = g, expand = TRUE, fill = "", anchor = c(0, -1))

## --- glayout (iNZight-shaped) ---------------------------------------------
lay_page <- gvbox(container = nb, label = "glayout", spacing = 6, expand = TRUE)
glabel("Typical form: right-aligned labels, left-aligned fields, expanding content",
       container = lay_page)
lay <- glayout(spacing = 6, container = lay_page, expand = TRUE, fill = TRUE)
lay[1, 1, anchor = c(1, 0), expand = TRUE, fill = FALSE] <- "Name"
lay[1, 2:3, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <-
  gedit("Alice", container = lay)
lay[2, 1, anchor = c(1, 0), expand = TRUE, fill = FALSE] <- "Choice"
lay[2, 2:3, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <-
  gcombobox(c("A", "B", "C"), container = lay)
lay[3, 1, anchor = c(1, 1), expand = FALSE, fill = FALSE] <- "Notes"
lay[3, 2:3, expand = TRUE, fill = TRUE] <-
  gtext("expands to fill the cell", height = 80, container = lay)
lay[4, 2:3, expand = FALSE, fill = FALSE, anchor = c(1, 0)] <-
  gbutton("OK (anchored right)", container = lay,
          handler = function(h, ...) svalue(sb) <- "OK")

## --- spring / space -------------------------------------------------------
sp_page <- gvbox(container = nb, label = "spring/space", spacing = 6)
glabel("addSpring pushes following children to the far edge",
       container = sp_page)
g1 <- ggroup(container = sp_page)
gbutton("Left", container = g1)
addSpring(g1)
gbutton("Right (after spring)", container = g1)

glabel("addSpace(40) inserts a fixed gap", container = sp_page)
g2 <- ggroup(container = sp_page)
gbutton("A", container = g2)
addSpace(g2, 40)
gbutton("B", container = g2)

## --- padding / margin / border --------------------------------------------
box_page <- gvbox(padding = 8, container = nb, label = "box model", spacing = 8)
glabel("Constructor padding/margin/border (CSS box model). Prefer set_padding over deprecated set_borderwidth.",
       container = box_page)
inner <- gframe("padding=12, margin=6, border=2",
                padding = 12, margin = 6, border = 2,
                container = box_page, expand = TRUE, fill = TRUE)
glabel("Inner content — padding is inside the frame's box; margin is outside.",
       container = inner)
gbutton("set_padding(20)", container = box_page, handler = function(h, ...) {
  inner$set_padding(20)
  svalue(sb) <- "inner$set_padding(20)"
})
gbutton("set_margin(c(4,20,4,20))", container = box_page, handler = function(h, ...) {
  inner$set_margin(c(4L, 20L, 4L, 20L))
  svalue(sb) <- "inner$set_margin(length-4)"
})

gbutton("Close", container = box_page, handler = function(h, ...) dispose(w))

visible(w) <- TRUE
svalue(nb) <- 1
message("Packing demo open — try anchors / expand-fill / glayout / box model tabs.")
message("Resize the window to see expand/fill; anchored buttons stay at edges of their allocation.")
