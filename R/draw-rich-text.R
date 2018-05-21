#' Draw rich text
#'
#' @param contents character vector containing html string to draw
#' @param hjust horizontal justification
#' @param x_pt x location, in points
#' @param y_pt y location, in points
#' @examples
#' library(grid)
#' grid.newpage()
#' draw_rich_text("This is the first line.<br>This is the second line.<br>And some <b>text in bold.</b>")
#' @export
draw_rich_text <- function(contents, hjust = 0.5, x_pt = 50, y_pt = 100) {
  doctree <- read_html(contents)
  drawing_context <- list(gp = gpar())

  grobs_table <- process_tags(as_list(doctree)$html$body, drawing_context)

  grobs_table$groups <- cumsum(grobs_table$type == "br")
  lines <- split(grobs_table, grobs_table$groups)
  grid.draw(do.call(gList, render_lines(lines, hjust, x_pt, y_pt)))
}
