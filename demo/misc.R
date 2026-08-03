## Catch-all widget gallery + gtree / gvarbrowser.
## Also see: demo(controls), demo(containers), demo(gtable), demo(gdf), demo(chrome), …
require(gWidgets2)
options(guiToolkit = "Rgtk4")

w <- gwindow("gWidgets2Rgtk4 misc widgets", visible = FALSE,
             width = 680, height = 560)
sb <- gstatusbar("Ready", container = w)
nb <- gnotebook(container = w)

## --- widget gallery ------------------------------------------------------
gal_page <- gvbox(container = nb, label = "widgets", spacing = 8)
glabel("Kitchen-sink page of common controls. Values echo to the status bar.",
       container = gal_page)

## Buttons / label
fr_btn <- gframe("Buttons & label", container = gal_page)
btn_row <- ggroup(container = fr_btn)
gbutton("Click me", container = btn_row, handler = function(h, ...) {
  svalue(sb) <- "gbutton clicked"
})
gbutton(action = gaction("Stock-ish", icon = "ok", handler = function(h, ...) {
  svalue(sb) <- "gaction button"
}), container = btn_row)
gal_lbl <- glabel("A glabel — click Update to refresh", container = fr_btn)

## Text inputs
fr_txt <- gframe("Text input", container = gal_page)
fl_txt <- gformlayout(container = fr_txt)
gedit("edit me", label = "gedit", container = fl_txt,
      handler = function(h, ...) svalue(sb) <- sprintf("gedit → %s", svalue(h$obj)))
gcalendar(format(Sys.Date()), label = "gcalendar", container = fl_txt,
          handler = function(h, ...) svalue(sb) <- sprintf("gcalendar → %s", svalue(h$obj)))
gfilebrowse(text = "Pick a file…", label = "gfilebrowse", container = fl_txt,
            handler = function(h, ...) svalue(sb) <- sprintf("gfilebrowse → %s", svalue(h$obj)))
gtext("gtext — multi-line\nedit area", height = 80, container = fr_txt)

## Selection
fr_sel <- gframe("Selection", container = gal_page)
gradio(c("alpha", "beta", "gamma"), selected = 1, horizontal = TRUE, container = fr_sel,
       handler = function(h, ...) svalue(sb) <- sprintf("gradio → %s", svalue(h$obj)))
gcheckbox("gcheckbox", checked = TRUE, container = fr_sel,
          handler = function(h, ...) svalue(sb) <- sprintf("gcheckbox → %s", svalue(h$obj)))
gcheckboxgroup(c("x", "y", "z"), checked = c(TRUE, FALSE, TRUE), horizontal = TRUE,
               container = fr_sel,
               handler = function(h, ...) {
                 svalue(sb) <- sprintf("gcheckboxgroup → %s", paste(svalue(h$obj), collapse = ","))
               })
gcombobox(c("one", "two", "three"), selected = 1, container = fr_sel,
          handler = function(h, ...) svalue(sb) <- sprintf("gcombobox → %s", svalue(h$obj)))

## Numeric
fr_num <- gframe("Numeric", container = gal_page)
gal_sl <- gslider(from = 0, to = 100, by = 1, value = 40, container = fr_num,
                  handler = function(h, ...) {
                    svalue(gal_pb) <- svalue(h$obj)
                    svalue(sb) <- sprintf("gslider → %s", svalue(h$obj))
                  })
gspinbutton(from = 0, to = 10, by = 1, value = 3, container = fr_num,
            handler = function(h, ...) svalue(sb) <- sprintf("gspinbutton → %s", svalue(h$obj)))
gal_pb <- gprogressbar(40, container = fr_num)
gseparator(container = fr_num)
gbutton("Update label from slider", container = fr_num, handler = function(h, ...) {
  svalue(gal_lbl) <- sprintf("Slider is at %s", svalue(gal_sl))
  svalue(sb) <- "label updated"
})

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
message("Misc demo open: widgets (incl. gcalendar), gtree, gvarbrowser.")
