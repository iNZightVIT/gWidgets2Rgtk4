##' @include misc.R
NULL

##' Add stock icons
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .addStockIcons guiWidgetsToolkitRgtk4
.addStockIcons.guiWidgetsToolkitRgtk4 <- function(toolkit, iconNames, iconFiles, ...) {
  .GWidgetsRgtk4Icons$add_icons(iconNames, iconFiles)
}

##' Returns list of stock / icon ids
##'
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .getStockIcons guiWidgetsToolkitRgtk4
.getStockIcons.guiWidgetsToolkitRgtk4 <- function(toolkit, ...) {
  c(as.list(.stock_to_icon_name), .GWidgetsRgtk4Icons$icons)
}

##' Return icon name by stock / gWidgets name
##'
##' @param name name of icon
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .getStockIconByName guiWidgetsToolkitRgtk4
.getStockIconByName.guiWidgetsToolkitRgtk4 <- function(toolkit, name, ...) {
  sapply(name, function(icon) {
    ## Prefer GTK4 theme names for known stock ids
    mapped <- stock_to_icon_name(icon)
    if (!is.null(mapped) && !identical(mapped, icon))
      return(mapped)
    ## Custom / gWidgets image files
    tmp <- .GWidgetsRgtk4Icons$icons[[icon, exact = TRUE]]
    if (is.null(tmp))
      tmp <- .GWidgetsRgtk4Icons$icons[[sprintf("gw-%s", icon), exact = TRUE]]
    if (!is.null(tmp))
      return(tmp)
    if (!is.null(mapped))
      return(mapped)
    ""
  }, USE.NAMES = TRUE)
}

##' Icon registry for gWidgets2Rgtk4 (file paths + theme names)
GWidgetsRgtk4Icons <- setRefClass(
  "GWidgetsRgtk4Icons",
  contains = "GWidgets2Icons",
  methods = list(
    update_icons = function() {
      callSuper()
    },
    add_icons = function(iconNames, iconFiles) {
      for (i in seq_along(iconNames)) {
        key <- paste0("gw-", iconNames[i])
        icons[[key]] <<- iconFiles[i]
        icons[[iconNames[i]]] <<- iconFiles[i]
      }
      invisible(TRUE)
    }
  )
)

.GWidgetsRgtk4Icons <- GWidgetsRgtk4Icons$new()

load_gwidget_icons <- function() {
  iconFileNames <- list.files(system.file("images", package = "gWidgets2"),
                              full.names = TRUE)
  iconFileNames <- Filter(function(x) grepl("\\.gif$", x), iconFileNames)
  if (!length(iconFileNames))
    return(invisible(FALSE))
  iconNames <- gsub("\\.gif$", "", basename(iconFileNames))
  .GWidgetsRgtk4Icons$add_icons(iconNames, iconFileNames)
  invisible(TRUE)
}

##' Return stock id from object
##'
##' @param obj R object to get icon from
##' @export
##' @rdname gWidgets2Rgtk4-undocumented
##' @method .stockIconFromObject guiWidgetsToolkitRgtk4
.stockIconFromObject.guiWidgetsToolkitRgtk4 <- function(toolkit, obj, ...) {
  icon_for_object <- function(x) UseMethod("icon_for_object")
  icon_for_object.default <- function(x) "gw-symbol_dot"
  icon_for_object.numeric <- function(x) "gw-numeric"
  icon_for_object.factor <- function(x) "gw-factor"
  icon_for_object.character <- function(x) "gw-character"
  icon_for_object.function <- function(x) "gw-function"
  icon_for_object.data.frame <- function(x) "gw-dataframe"
  icon_for_object(obj)
}
