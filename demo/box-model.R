## Visual tour of container padding / margin / border (CSS box model).
##
##   cd gWidgets2Rgtk4
##   Rscript -e 'pkgload::load_all("."); source("demo/box-model.R")'
## Or after install: demo("box-model", package = "gWidgets2Rgtk4")
##
## Semantics:
##   padding — space inside the box around children (CSS padding)
##   margin  — space outside the box (GTK margins on the outer block)
##   border  — visible border width in px (CSS border: Npx solid)
## Prefer set_padding(); set_borderwidth() is deprecated (warns + aliases).

require(gWidgets2)
options(guiToolkit = "Rgtk4")

## Light tint so empty padding/margin is obvious against the window.
.tint <- function(obj, color) {
  css(obj) <- sprintf("background-color: %s;", color)
}

w <- gwindow("gWidgets2Rgtk4 box model", visible = FALSE, width = 780, height = 520)
sb <- gstatusbar("padding = inside · margin = outside · border = outline",
                 container = w)

outer <- gvbox(padding = 10, spacing = 8, container = w, expand = TRUE)
glabel("Three cards start identical except one property. Tweak with the buttons below.",
       container = outer)

row <- ggroup(horizontal = TRUE, spacing = 8, container = outer,
              expand = TRUE, fill = TRUE)

## --- padding card ---------------------------------------------------------
card_pad <- gvbox(padding = 20, margin = 4, border = 1,
                  container = row, expand = TRUE, fill = TRUE)
.tint(card_pad, "#dbeafe") ## blue = padding zone
glabel("padding = 20", container = card_pad)
glabel("(space inside, around children)", container = card_pad)
gbutton("Child", container = card_pad)

## --- margin card ----------------------------------------------------------
card_mar <- gvbox(padding = 4, margin = 20, border = 1,
                  container = row, expand = TRUE, fill = TRUE)
.tint(card_mar, "#dcfce7") ## green = content; margin is the gap outside
glabel("margin = 20", container = card_mar)
glabel("(space outside the box)", container = card_mar)
gbutton("Child", container = card_mar)

## --- border card ----------------------------------------------------------
card_bor <- gvbox(padding = 8, margin = 4, border = 4,
                  container = row, expand = TRUE, fill = TRUE)
.tint(card_bor, "#fef3c7") ## amber
glabel("border = 4", container = card_bor)
glabel("(visible CSS border)", container = card_bor)
gbutton("Child", container = card_bor)

## --- playground -----------------------------------------------------------
play <- gframe("Playground — live setters",
               padding = 10, margin = 2, border = 1,
               container = outer, expand = TRUE, fill = TRUE)
.tint(play, "#f3e8ff")

info <- glabel("padding=10  margin=2  border=1", container = play)

.refresh_info <- function() {
  svalue(info) <- sprintf(
    "padding=%s  margin=%s  border=%s",
    paste(play$get_padding(), collapse = ","),
    paste(play$get_margin(), collapse = ","),
    play$get_border()
  )
  svalue(sb) <- svalue(info)
}

btns <- ggroup(container = play)
gbutton("padding 4", container = btns, handler = function(h, ...) {
  play$set_padding(4L); .refresh_info()
})
gbutton("padding 24", container = btns, handler = function(h, ...) {
  play$set_padding(24L); .refresh_info()
})
gbutton("padding TRBL", container = btns, handler = function(h, ...) {
  play$set_padding(c(4L, 28L, 4L, 28L)); .refresh_info()
})

btns2 <- ggroup(container = play)
gbutton("margin 0", container = btns2, handler = function(h, ...) {
  play$set_margin(0L); .refresh_info()
})
gbutton("margin 16", container = btns2, handler = function(h, ...) {
  play$set_margin(16L); .refresh_info()
})
gbutton("margin asymmetric", container = btns2, handler = function(h, ...) {
  play$set_margin(c(2L, 40L, 2L, 8L)); .refresh_info()
})

btns3 <- ggroup(container = play)
gbutton("border 0", container = btns3, handler = function(h, ...) {
  play$set_border(0L); .refresh_info()
})
gbutton("border 1", container = btns3, handler = function(h, ...) {
  play$set_border(1L); .refresh_info()
})
gbutton("border 5", container = btns3, handler = function(h, ...) {
  play$set_border(5L); .refresh_info()
})

gseparator(container = play)
warn_row <- ggroup(container = play)
gbutton("deprecated set_borderwidth(12)…", container = warn_row, handler = function(h, ...) {
  ## Capture deprecation warning for the status bar (GTK callbacks lack muffleWarning via tryCatch)
  wmsg <- NULL
  withCallingHandlers(
    play$set_borderwidth(12L),
    warning = function(w) {
      wmsg <<- conditionMessage(w)
      tryInvokeRestart("muffleWarning")
    }
  )
  if (!is.null(wmsg))
    svalue(sb) <- wmsg
  .refresh_info()
})

foot <- ggroup(container = outer)
addSpring(foot)
gbutton("Close", container = foot, handler = function(h, ...) dispose(w))

visible(w) <- TRUE
message("Box-model demo open.")
message("  Blue card = large padding; green = large margin; amber = thick border.")
message("  Use Playground buttons to tweak the purple frame live.")
message("  Run:  Rscript -e 'pkgload::load_all(\".\"); source(\"demo/box-model.R\")'")
