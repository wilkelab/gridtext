make_text_grobs <- function(labels, drawing_context) {
  grobs <- lapply(labels, textGrob, x = 0, y = 0, hjust = 0, vjust = 0, gp = drawing_context$gp)
  width_pt <- vapply(grobs, function(x) convertWidth(grobWidth(x), "pt", valueOnly = TRUE), numeric(1))
  tibble(
    grob = grobs,
    width_pt,
    height_pt = drawing_context$height_pt,
    descent_pt = drawing_context$descent_pt,
    linespacing_pt = drawing_context$linespacing_pt,
    type = "text"
  )
}


make_line_break <- function(drawing_context) {
  tibble(
    grob = list(zeroGrob()),
    width_pt = 0,
    height_pt = 0,
    descent_pt = 0,
    linespacing_pt = 0,
    type = "br"
  )
}
