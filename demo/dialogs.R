## Modal and non-modal dialog helpers.
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 dialogs", visible = FALSE, width = 360, height = 240)
g <- gvbox(container = w, spacing = 8)
out <- glabel("(results appear here)", container = g)

gbutton("gmessage", container = g, handler = function(h, ...) {
  gmessage("Hello from gmessage", title = "Message", parent = w)
  svalue(out) <- "gmessage closed"
})

gbutton("gconfirm", container = g, handler = function(h, ...) {
  ok <- gconfirm("Proceed?", title = "Confirm", parent = w)
  svalue(out) <- sprintf("gconfirm -> %s", ok)
})

gbutton("ginput", container = g, handler = function(h, ...) {
  val <- ginput("Your name?", text = "Ada", title = "Input", parent = w)
  svalue(out) <- sprintf("ginput -> %s", paste(val, collapse = ""))
})

gbutton("galert", container = g, handler = function(h, ...) {
  galert("Alert via status/infobar path", parent = w)
  svalue(out) <- "galert shown"
})

gbutton("gfile (open)", container = g, handler = function(h, ...) {
  f <- gfile(type = "open", parent = w)
  svalue(out) <- sprintf("gfile -> %s", paste(f, collapse = ""))
})

gbutton("Close", container = g, handler = function(h, ...) dispose(w))

visible(w) <- TRUE
message("Dialogs demo open.")
