## Widgets without a dedicated demo — start with gtree / gvarbrowser.
## Also see: demo(gtable), demo(gdf), demo(chrome), …
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 misc widgets", visible = FALSE,
             width = 640, height = 480)
sb <- gstatusbar("Ready", container = w)
nb <- gnotebook(container = w)

## --- gtree ---------------------------------------------------------------
tree_page <- gvbox(container = nb, label = "gtree", spacing = 6)
glabel("Hierarchical list via offspring(). Click a node; expand branches.",
       container = tree_page)

offspring_list <- function(path = character(0), lst, ...) {
  if (length(path))
    obj <- lst[[path]]
  else
    obj <- lst
  nms <- names(obj)
  hasOffspring <- vapply(nms, function(i) {
    newobj <- obj[[i]]
    is.recursive(newobj) && !is.null(names(newobj))
  }, logical(1))
  data.frame(
    Name = nms,
    hasOffspring = hasOffspring,
    Description = vapply(nms, function(i) {
      x <- obj[[i]]
      if (is.recursive(x) && !is.null(names(x)))
        sprintf("list [%d]", length(x))
      else
        paste(as.character(x), collapse = ", ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

demo_tree <- list(
  alpha = "leaf A",
  beta = list(
    b1 = "leaf B1",
    b2 = list(
      deep = "leaf deep",
      other = 42
    ),
    b3 = "leaf B3"
  ),
  gamma = list(
    g1 = TRUE,
    g2 = "leaf G2"
  )
)

tr <- gtree(
  offspring = offspring_list,
  offspring.data = demo_tree,
  chosen.col = 1,
  offspring.col = 2,
  container = tree_page,
  expand = TRUE
)

tree_out <- glabel("Selection: (none)", container = tree_page)
addHandlerClicked(tr, handler = function(h, ...) {
  path <- svalue(h$obj, drop = FALSE)
  tip <- svalue(h$obj, drop = TRUE)
  if (!length(path)) {
    svalue(tree_out) <- "Selection: (none)"
    svalue(sb) <- "gtree: cleared"
    return()
  }
  svalue(tree_out) <- sprintf(
    "Selection: tip=%s  path=%s  index=%s",
    tip,
    paste(path, collapse = " / "),
    paste(svalue(h$obj, index = TRUE), collapse = ",")
  )
  svalue(sb) <- sprintf("gtree → %s", paste(path, collapse = "$"))
})

tree_btns <- ggroup(container = tree_page)
gbutton("Select beta/b2/deep", container = tree_btns, handler = function(h, ...) {
  svalue(tr, index = TRUE) <- c(2L, 2L, 1L)
})
gbutton("Clear", container = tree_btns, handler = function(h, ...) {
  svalue(tr) <- integer(0)
})
gbutton("Update (swap gamma)", container = tree_btns, handler = function(h, ...) {
  demo_tree$gamma <<- list(g_new = "replaced", g_extra = 99)
  update(tr)
  svalue(sb) <- "gtree updated"
})

## --- gvarbrowser ---------------------------------------------------------
vb_page <- gvbox(container = nb, label = "gvarbrowser", spacing = 6)
glabel("Workspace browser (categories + WSWatcherModel). Assign objects, wait a beat.",
       container = vb_page)

## Seed a few GlobalEnv objects so the browser is not empty
.assign_demo_objs <- function() {
  assign(".demo_num", 1:5, envir = .GlobalEnv)
  assign(".demo_chr", letters[1:3], envir = .GlobalEnv)
  assign(".demo_df", head(mtcars, 3), envir = .GlobalEnv)
  assign(".demo_lst", list(a = 1, b = list(c = "nested")), envir = .GlobalEnv)
  assign(".demo_fit", lm(mpg ~ wt, data = mtcars), envir = .GlobalEnv)
}
.rm_demo_objs <- function() {
  nms <- c(".demo_num", ".demo_chr", ".demo_df", ".demo_lst", ".demo_fit")
  rm(list = intersect(nms, ls(envir = .GlobalEnv, all.names = TRUE)),
     envir = .GlobalEnv)
}
.assign_demo_objs()

vb <- gvarbrowser(container = vb_page, expand = TRUE)
vb_out <- glabel("Selection: (none)", container = vb_page)

addHandlerSelectionChanged(vb, handler = function(h, ...) {
  nm <- svalue(h$obj, drop = TRUE)
  if (!length(nm)) {
    svalue(vb_out) <- "Selection: (none)"
    return()
  }
  svalue(vb_out) <- sprintf("Selection: %s", paste(nm, collapse = ", "))
  svalue(sb) <- sprintf("gvarbrowser → %s", paste(nm, collapse = ", "))
})
addHandlerChanged(vb, handler = function(h, ...) {
  nm <- svalue(h$obj, drop = TRUE)
  svalue(sb) <- sprintf("gvarbrowser activate → %s", paste(nm, collapse = ", "))
})

vb_btns <- ggroup(container = vb_page)
gbutton("Refresh objects", container = vb_btns, handler = function(h, ...) {
  .assign_demo_objs()
  vb$ws_model$update_state()
  svalue(sb) <- "demo objects (re)assigned — browser should refresh"
})
gbutton("Remove demo objects", container = vb_btns, handler = function(h, ...) {
  .rm_demo_objs()
  vb$ws_model$update_state()
  svalue(sb) <- "demo objects removed"
})

## Stop timer + clean GlobalEnv when the window closes
addHandlerDestroy(w, handler = function(h, ...) {
  try(vb$stop_timer(), silent = TRUE)
  .rm_demo_objs()
})

svalue(nb) <- 1L
visible(w) <- TRUE
message("Misc demo open: gtree, gvarbrowser. Close the window to stop the WS timer.")
