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
#' draw_rich_text("This is some <span style='font-family:\"Comic Sans MS\"; font-size:20;'>big font
#'   <span style='font-size:25; color:red'>in red.</span></span> <br>And the next line like nothing happened.")
#' @export
draw_rich_text <- function(contents, hjust = 0.5, x_pt = 50, y_pt = 100, newpage = TRUE) {
  doctree <- read_html(contents)

  drawing_context <- setup_context()

  grobs_table <- process_tags(as_list(doctree)$html$body, drawing_context)

  grobs_table$groups <- cumsum(grobs_table$type == "br")
  lines <- split(grobs_table, grobs_table$groups)

  if (isTRUE(newpage)) grid.newpage()
  grid.draw(render_lines(lines, hjust, x_pt, y_pt))
}


#' Rich-text grob
#'
#' @param contents character vector containing html string
#' @param hjust horizontal justification
#' @param x x location
#' @param y y location
#' @examples
#' library(grid)
#' grid.newpage()
#' grid.draw(rich_text_grob("Some text <b>in bold.</b>"))
#' @export
rich_text_grob <- function(contents, x = unit(0.5, "npc"), y = unit(0.5, "npc"), hjust = 0) {
  if (!is.unit(x)) x <- unit(x, "npc")
  if (!is.unit(y)) y <- unit(y, "npc")

  doctree <- read_html(contents)

  drawing_context <- setup_context()

  grobs_table <- process_tags(as_list(doctree)$html$body, drawing_context)

  grobs_table$groups <- cumsum(grobs_table$type == "br")
  lines <- split(grobs_table, grobs_table$groups)

  render_lines(lines, hjust, 0, 0, vp = viewport(x, y))
}

