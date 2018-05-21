update_gpar <- function(gp, gp_new) {
  names_new <- names(gp_new)
  names_old <- names(gp)
  gp[c(intersect(names_old, names_new), "font")] <- NULL
  gp_new["font"] <- NULL
  do.call(gpar, c(gp, gp_new))
}


setup_drawing_context <- function(fontsize = 12, fontfamily = "", fontface = "plain", color = "black",
                                  cex = 1, lineheight = 1.2) {
  gp <- gpar(
    fontsize = fontsize, fontfamily = fontfamily, fontface = fontface,
    col = color, cex = cex, lineheight = lineheight
  )
  gp <- update_gpar(get.gpar(), gp)

  set_context_gp(list(), gp)
}

set_context_gp <- function(drawing_context, gp = NULL) {
  gp <- update_gpar(drawing_context$gp, gp)

  x <- textGrob(label = "gjpqyQ", gp = gp)

  height_pt <- convertHeight(heightDetails(x), "pt", valueOnly = TRUE)
  descent_pt <- convertHeight(descentDetails(x), "pt", valueOnly = TRUE)
  linespacing_pt <- gp$lineheight * gp$fontsize

  list(gp = gp, height_pt = height_pt, descent_pt = descent_pt, linespacing_pt = linespacing_pt)
}

set_context_fontface <- function(drawing_context, fontface = "plain", overwrite = FALSE) {
  fontface_old <- drawing_context$gp$fontface

  # combine bold and italic if needed
  if (!isTRUE(overwrite)) {
    if (fontface == "italic" && fontface_old == "bold") {
      fontface <- "bold.italic"
    } else if (fontface == "bold" && fontface_old == "italic") {
      fontface <- "bold.italic"
    }
  }

  set_context_gp(drawing_context, gpar(fontface = fontface))
}
