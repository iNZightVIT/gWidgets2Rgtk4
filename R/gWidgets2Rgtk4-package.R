##' @title gWidgets2Rgtk4: gWidgets2 toolkit for Rgtk4
##'
##' @description
##' Implements the \pkg{gWidgets2} API using \pkg{Rgtk4} (GTK4).
##' Design docs live in the source tree under \code{docs/}.
##'
##' Font and style: \code{font<-} maps the portable font specification to
##' CSS. Toolkit helpers \code{\link{css}}, \code{\link{loadCss}},
##' \code{\link{addCssClass}} allow direct CSS. Defaults can be remapped via
##' \code{options("gWidgets2Rgtk4.font.family")},
##' \code{options("gWidgets2Rgtk4.font.weight")},
##' \code{options("gWidgets2Rgtk4.font.scale.base")}, and an app stylesheet
##' via \code{options("gWidgets2Rgtk4.css")}.
##'
##' @keywords internal
##' @import methods
##' @import gWidgets2
##' @import Rgtk4
##' @import memoise
"_PACKAGE"
