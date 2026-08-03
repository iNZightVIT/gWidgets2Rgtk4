## Selection and numeric controls smoke window.
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 controls", visible = FALSE, width = 420, height = 420)
g <- gvbox(container = w, spacing = 6)

glabel("Radio / checkbox / combo", container = g)
r <- gradio(c("alpha", "beta", "gamma"), selected = 2, horizontal = TRUE, container = g)
cg <- gcheckboxgroup(c("x", "y", "z"), checked = c(TRUE, FALSE, TRUE), container = g)
cb <- gcombobox(c("one", "two", "three"), selected = 1, container = g)

glabel("Slider / spin / progress", container = g)
sl <- gslider(from = 0, to = 100, by = 1, value = 40, container = g)
sp <- gspinbutton(from = 0, to = 10, by = 1, value = 3, container = g)
pb <- gprogressbar(40, container = g)

out <- glabel("", container = g)
gbutton("Snapshot values", container = g, handler = function(h, ...) {
  svalue(pb) <- svalue(sl)
  svalue(out) <- sprintf(
    "radio=%s; checks=%s; combo=%s; slider=%s; spin=%s",
    svalue(r), paste(svalue(cg), collapse = ","), svalue(cb), svalue(sl), svalue(sp)
  )
})
gbutton("Close", container = g, handler = function(h, ...) dispose(w))

visible(w) <- TRUE
message("Controls demo open.")
