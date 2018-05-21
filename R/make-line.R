make_line <- function(labels, drawing_context) {
  grobs <- lapply(labels, textGrob, x = 0, y = 0, hjust = 0, vjust = 0, gp = drawing_context$gp)
  width_pt <- vapply(grobs, function(x) convertWidth(grobWidth(x), "pt"), numeric(1))
  height_pt <- vapply(grobs, function(x) convertHeight(grobHeight(x), "pt"), numeric(1))
  descent_pt <- vapply(grobs, function(x) convertHeight(descentDetails(x), "pt"), numeric(1))
  tibble(grob = grobs, width_pt, height_pt, descent_pt, type = "text")
}
