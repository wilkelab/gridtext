#' Calculate descent and other text properties, either from grob or from graphical
#' parameters
#' @param grob The grob for which we want information
#' @export
grob_descent <- function(grob) {
  pushViewport(viewport(gp = grob$gp)) # change viewport to get correct font settings
  if (inherits(grob, "text")) {
    grob <- editGrob(grob, label = "gjpqyQ") # change text label to generic descent label
  }
  descent <- descentDetails(grob)
  popViewport()
  descent
}

#' @rdname grob_descent
#' @export
grob_descent_pt <- function(grob) {
  convertHeight(grob_descent(grob), "pt", valueOnly = TRUE)
}

#' Calculates generic font height and descent from given graphical parameters
#'
#' @param gp Graphical parameters
#' @export
font_details_pt <- function(gp = gpar()) {
  pushViewport(viewport(gp = gp)) # change viewport to get correct font settings
  grob <- textGrob(label = "gjpqyQ")
  height <- convertHeight(heightDetails(grob), "pt", valueOnly = TRUE)
  descent <- convertHeight(descentDetails(grob), "pt", valueOnly = TRUE)
  popViewport()
  list(height_pt = height, descent_pt = descent)
}

