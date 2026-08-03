## Menubar, toolbar, statusbar, and shared gaction proxies (Phase 2 chrome).
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 chrome", visible = FALSE, width = 480, height = 320)

acts <- list(
  open = gaction("open", icon = "open", tooltip = "Open",
                 handler = function(...) svalue(sb) <- "open", parent = w),
  save = gaction("save", icon = "save", tooltip = "Save",
                 handler = function(...) svalue(sb) <- "save", parent = w),
  undo = gaction("undo", icon = "undo",
                 handler = function(...) svalue(sb) <- "undo", parent = w),
  redo = gaction("redo", icon = "redo",
                 handler = function(...) svalue(sb) <- "redo", parent = w),
  quit = gaction("quit", icon = "quit", tooltip = "Quit",
                 handler = function(...) dispose(w), parent = w)
)

mb_list <- list(
  File = list(acts$open, acts$save, gseparator(horizontal = TRUE), acts$quit),
  Edit = list(acts$undo, acts$redo)
)
tb_list <- list(acts$open, acts$save, gseparator(horizontal = FALSE), acts$quit)

gmenu(mb_list, container = w)
gtoolbar(tb_list, style = "both", container = w)
sb <- gstatusbar("Ready — try menu, toolbar, or the action button", container = w)

g <- gvbox(container = w, spacing = 8)
glabel("Shared gaction proxies: disable Save and watch menu + toolbar update.", container = g)

row <- ggroup(container = g)
gbutton(action = acts$open, container = row)
gbutton("Toggle Save enabled", container = row, handler = function(h, ...) {
  enabled(acts$save) <- !enabled(acts$save)
  svalue(sb) <- sprintf("save enabled=%s", enabled(acts$save))
})
gbutton("galert", container = row, handler = function(h, ...) {
  galert("Transient message via galert / set_infobar path.", parent = w)
})

visible(w) <- TRUE
message("Chrome demo open: File/Edit menus, toolbar, statusbar, action button.")
