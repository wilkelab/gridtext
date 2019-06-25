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
#' @param hjust_int Internal horizontal justification (0 = left, 0.5 = center, 1 = right)
#' @param gp Other graphical parameters for drawing
#' @param ... Other arguments handed off to [`box_grob()`]
#' @examples
#' library(grid)
#' grid.newpage()
#' grid.draw(rich_text_grob("Some text <b>in bold.</b>"))
#' @export
rich_text_grob <- function(contents, ..., hjust_int = 0, gp = NULL) {
  doctree <- read_html(contents)

  drawing_context <- setup_context(gp = gp)

  grobs_table <- process_tags(as_list(doctree)$html$body, drawing_context)

  grobs_table$groups <- cumsum(grobs_table$type == "br")
  lines <- split(grobs_table, grobs_table$groups)

  box_grob(
    render_lines(lines, hjust_int, 0, 0),
    ...
  )
}

#' @rdname rich_text_grob
#' @export
markdown_grob <- function(contents, ...) {
  html <- markdown::markdownToHTML(text = contents, fragment.only = TRUE)
  rich_text_grob(html, ...)
}

