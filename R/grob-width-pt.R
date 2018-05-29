#' Get width and height info of a grob, in pt
#'
#' @param grob The grob for which we want the width or height
#' @export
grob_width_pt <- function(grob) {
  convertWidth(grobWidth(grob), "pt", valueOnly = TRUE)
}


#' @rdname grob_width_pt
#' @export
grob_height_pt <- function(grob) {
  convertHeight(grobHeight(grob), "pt", valueOnly = TRUE)
}
