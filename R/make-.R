make_text_grobs <- function(labels, drawing_context) {
  # standardize whitespace; doesn't quite work yet in all contexts
  labels <- gsub("\\s+", " ", labels, perl = TRUE)

  grobs <- lapply(labels, textGrob, x = 0, y = 0, hjust = 0, vjust = 0, gp = drawing_context$gp)
  width_pt <- vapply(grobs, grob_width_pt, numeric(1))
  tibble(
    grob = grobs,
    width_pt,
    height_pt = drawing_context$height_pt,
    descent_pt = drawing_context$descent_pt,
    linespacing_pt = drawing_context$linespacing_pt,
    yoff_pt = drawing_context$yoff_pt,
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
    yoff_pt = 0,
    type = "br"
  )
}
