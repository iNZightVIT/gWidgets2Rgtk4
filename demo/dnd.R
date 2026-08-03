## Drag-and-drop: text, object payloads, and gvarbrowser → V1/V2.
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 DnD", visible = FALSE, width = 720, height = 520)
sb <- gstatusbar("Drag from sources onto drop targets", container = w)
nb <- gnotebook(container = w)

## --- Text transfers -------------------------------------------------------
txt_page <- gvbox(container = nb, label = "text", spacing = 8)
glabel("Drag the label or button onto the entry / text area.",
       container = txt_page)

tgt_entry <- gedit("", container = txt_page)
addDropTarget(tgt_entry, handler = function(h, ...) {
  svalue(h$obj) <- as.character(h$dropdata)[1]
  svalue(sb) <- sprintf("text → gedit: %s", svalue(h$obj))
})

tgt_text <- gtext("", height = 100, container = txt_page)
addDropTarget(tgt_text, handler = function(h, ...) {
  insert(h$obj, as.character(h$dropdata)[1], where = "end")
  svalue(sb) <- "text → gtext"
})

src_row <- ggroup(container = txt_page)
src_lbl <- glabel("Drag me (text)", container = src_row)
addDropSource(src_lbl, handler = function(h, ...) "hello from label")
src_btn <- gbutton("Drag me too", container = src_row)
addDropSource(src_btn, handler = function(h, ...) sprintf("button @ %s", Sys.time()))

## --- Object transfers -----------------------------------------------------
obj_page <- gvbox(container = nb, label = "object", spacing = 8)
glabel("Drag the package; drop target receives an R list (not just text).",
       container = obj_page)

obj_src <- glabel("Drag me (object: list(a=1, b='x'))", container = obj_page)
addDropSource(obj_src, data.type = "object", handler = function(h, ...) {
  list(a = 1, b = "x", when = Sys.time())
})

obj_out <- gtext("", height = 160, container = obj_page)
addDropTarget(obj_out, handler = function(h, ...) {
  svalue(h$obj) <- paste(capture.output(str(h$dropdata)), collapse = "\n")
  svalue(sb) <- sprintf("object drop class=%s", paste(class(h$dropdata), collapse = ","))
})

## --- gvarbrowser → V1 / V2 ------------------------------------------------
vb_page <- gpanedgroup(horizontal = TRUE, container = nb, label = "varbrowser")
left <- gvbox(container = vb_page, spacing = 4)
right <- gvbox(container = vb_page, spacing = 6)

glabel("Workspace browser (drag a variable)", container = left)
.assign_demo_objs <- function() {
  assign(".dnd_demo_x", 1:10, envir = .GlobalEnv)
  assign(".dnd_demo_y", letters[1:5], envir = .GlobalEnv)
  assign(".dnd_demo_df", head(mtcars, 3), envir = .GlobalEnv)
}
.rm_demo_objs <- function() {
  nms <- c(".dnd_demo_x", ".dnd_demo_y", ".dnd_demo_df")
  rm(list = intersect(nms, ls(envir = .GlobalEnv, all.names = TRUE)),
     envir = .GlobalEnv)
}
.assign_demo_objs()
vb <- gvarbrowser(container = left, expand = TRUE)

glabel("Drop onto V1 / V2 (iNZight-shaped)", container = right)
fl <- gformlayout(container = right)
v1 <- gedit("", label = "V1", container = fl)
v2 <- gedit("", label = "V2", container = fl)

.drop_var_name <- function(h, ...) {
  dd <- h$dropdata
  nm <- if (is.list(dd) && !is.null(dd$name)) {
    as.character(dd$name)[1]
  } else {
    as.character(dd)[1]
  }
  svalue(h$obj) <- nm
  svalue(sb) <- sprintf("varbrowser → %s", nm)
}
addDropTarget(v1, handler = .drop_var_name)
addDropTarget(v2, handler = .drop_var_name)

gbutton("Refresh demo objects", container = right, handler = function(h, ...) {
  .assign_demo_objs()
  vb$ws_model$update_state()
  svalue(sb) <- "demo objects refreshed"
})

addHandlerDestroy(w, handler = function(h, ...) {
  try(vb$stop_timer(), silent = TRUE)
  .rm_demo_objs()
})

svalue(nb) <- 1L
visible(w) <- TRUE
message("DnD demo open: text, object, varbrowser tabs.")
