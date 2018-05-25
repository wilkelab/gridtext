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
#' draw_rich_text('This is some <font size="20" face="Comic Sans MS">big font
#'   <font size="8" color="red">in red.</font></font> <br>And the next line like nothing happened.')
#' @export
draw_rich_text <- function(contents, hjust = 0.5, x_pt = 50, y_pt = 100, newpage = TRUE) {
  doctree <- read_html(contents)

  drawing_context <- setup_context()

  grobs_table <- process_tags(as_list(doctree)$html$body, drawing_context)

  grobs_table$groups <- cumsum(grobs_table$type == "br")
  lines <- split(grobs_table, grobs_table$groups)

  if (isTRUE(newpage)) grid.newpage()
  grid.draw(do.call(gList, render_lines(lines, hjust, x_pt, y_pt)))
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
rich_text_grob <- function(contents, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           hjust = 0, angle = 0) {
  if (!is.unit(x)) x <- unit(x, "npc")
  if (!is.unit(y)) y <- unit(y, "npc")

  doctree <- read_html(contents)

  drawing_context <- setup_context()

  grobs_table <- process_tags(as_list(doctree)$html$body, drawing_context)

  grobs_table$groups <- cumsum(grobs_table$type == "br")
  lines <- split(grobs_table, grobs_table$groups)

  children <- do.call(gList, render_lines(lines, hjust, 0, 0))
  #children <- gList(children, pointsGrob(0, 0)) # highlight reference point for debugging
  grobTree(children, vp = viewport(x, y, just = c(0, 0), angle = angle))
}

