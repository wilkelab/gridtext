#' Draw rich text
#'
#' @param contents character vector containing html string to draw
#' @param x_pt x location, in points
#' @param y_pt y location, in points
#' @param width_pt width, in points
#' @param gp Other graphical parameters for drawing
#' @param newpage Bool indicating whether `grid.newpage()` should be called first
#' @examples
#' library(grid)
#' grid.newpage()
#' draw_rich_text(
#'   "This is the first line.<br>This is the second line.<br>And some <b>text in bold.</b>"
#' )
#' draw_rich_text(
#'   "This is some <span style='font-family:\"Bookman\"; font-size:20;'>big
#'    font <span style='font-size:25; color:red'>in red.</span></span> <br>And the next
#'    line like nothing happened."
#' )
#' @export
draw_rich_text <- function(contents, x_pt = 50, y_pt = 100, width_pt = 300, gp = NULL, newpage = TRUE) {
  doctree <- read_html(contents)

  drawing_context <- setup_context(gp = gp)

  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox <- bl_make_vbox(boxlist, width_pt = width_pt, hjust = 0, vjust = 0, width_policy = "fixed")

  bl_calc_layout(vbox, width_pt, 0)
  grob <- bl_render(vbox, x_pt, y_pt)

  if (isTRUE(newpage)) grid.newpage()
  grid.draw(grob)
}

