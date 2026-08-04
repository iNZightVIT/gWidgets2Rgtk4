## Spike: dump GtkColumnView widget tree (CSS names) for gdf header discovery.
## Run from package root or after installing gWidgets2Rgtk4:
##   Rscript demo/gdf-column-headers-spike.R
## Or interactively: source("demo/gdf-column-headers-spike.R")
##
## What to look for: CSS names under the ColumnView that look like header
## cells / buttons / labels for each column title (mpg, cyl, hp).
require(gWidgets2)
options(guiToolkit = "Rgtk4")

.dump_tree <- function(w, depth = 0L, max_depth = 10L) {
  if (is.null(w) || depth > max_depth)
    return(invisible(NULL))
  css <- tryCatch(gtkWidgetGetCssName(w), error = function(e) "?")
  typ <- paste(class(w), collapse = "/")
  extra <- ""
  if (inherits(w, "GtkLabel")) {
    lab <- tryCatch(as.character(gtkLabelGetText(w))[1], error = function(e) "")
    if (nzchar(lab))
      extra <- sprintf(" label=%s", lab)
  }
  nctrl <- tryCatch({
    m <- gtkWidgetObserveControllers(w)
    as.integer(gListModelGetNItems(m))[1]
  }, error = function(e) NA_integer_)
  cat(sprintf("%s[%s] %s nctrl=%s%s\n",
              strrep("  ", depth), css, typ, nctrl, extra))
  ch <- tryCatch(gtkWidgetGetFirstChild(w), error = function(e) NULL)
  while (!is.null(ch)) {
    .dump_tree(ch, depth + 1L, max_depth)
    ch <- tryCatch(gtkWidgetGetNextSibling(ch), error = function(e) NULL)
  }
  invisible(NULL)
}

w <- gwindow("gdf header spike", visible = FALSE, width = 480, height = 280)
g <- gvbox(container = w)
glabel("Tree dumps to console after realize. Close window when done.",
       container = g)
gd <- gdf(mtcars[1:3, c("mpg", "cyl", "hp")], container = g, expand = TRUE)
visible(w) <- TRUE

## Process a few iterations so ColumnView builds header widgets.
for (i in 1:30) {
  try(gtkMainIterationDo(FALSE), silent = TRUE)
  Sys.sleep(0.02)
}

cat("\n=== ColumnView widget tree ===\n")
.dump_tree(gd$widget)
cat("\n=== reorderable =",
    tryCatch(gtkColumnViewGetReorderable(gd$widget), error = function(e) NA),
    "===\n")
cat("Column titles:", paste(names(gd), collapse = ", "), "\n")
message("Spike ready — inspect console dump; close the window to exit.")
