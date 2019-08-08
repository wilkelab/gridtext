#' Calculate descent and other text properties, either from grob or from graphical
#' parameters
#' @param grob The grob for which we want information
#' @export
grob_descent <- function(grob) {
  unit(grob_descent_pt(grob), "pt")
}

#' @rdname grob_descent
#' @export
grob_descent_pt <- function(grob) {
  if (inherits(grob, "text")) {
    text_details(grob$label, grob$gp)$descent_pt
  } else 0
}
