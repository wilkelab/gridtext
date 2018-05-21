update_gpar <- function(gp, gp_new) {
  names_new <- names(gp_new)
  names_old <- names(gp)
  gp[c(intersect(names_old, names_new), "font")] <- NULL
  gp_new["font"] <- NULL
  do.call(gpar, c(gp, gp_new))
}


setup_drawing_context <- function(gp = NULL) {
  gp <- update_gpar(get.gpar(), gp)

  update_drawing_context(list(), gp)
}


update_drawing_context <- function(drawing_context, gp = NULL) {
  gp <- update_gpar(drawing_context$gp, gp)

  x <- textGrob(label = "gjpqyQ", gp = gp)

  height_pt <- convertHeight(heightDetails(x), "pt", valueOnly = TRUE)
  descent_pt <- convertHeight(descentDetails(x), "pt", valueOnly = TRUE)
  linespacing_pt <- gp$lineheight * gp$fontsize

  list(gp = gp, height_pt = height_pt, descent_pt = descent_pt, linespacing_pt = linespacing_pt)
}
