#' Rich-text grob
#'
#' @param text Character vector containing markdown/html string to draw.
#' @param width,height Unit objects specifying width and height of the
#'   grob; height is currently ignored.
#' @param x,y Unit objects specifying the location of the reference point.
#' @param hjust,vjust Numerical values specifying the location of the grob
#'   relative to the reference point.
#' @param gp Other graphical parameters for drawing
#' @param use_markdown Should the `text` input be treated as markdown?
#' @examples
#' library(grid)
#' grid.newpage()
#' grid.draw(rich_text_grob("Some text **in bold.**"))
#' @export
rich_text_grob <- function(text, width = unit(0.5, "npc"), height = NULL,
                           x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           hjust = 0.5, vjust = 0.5, gp = gpar(), use_markdown = TRUE) {
  if (use_markdown) {
    text <- markdown::markdownToHTML(text = text, options = c("use_xhtml", "fragment_only"))
  }
  doctree <- read_html(text)

  drawing_context <- setup_context(gp = gp)
  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox <- bl_make_vbox(boxlist, width = 100, hjust = hjust, vjust = vjust, width_policy = "relative")

  gTree(
    width = width,
    height = height,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    vbox = vbox,
    gp = gp,
    cl = "rich_text_grob"
  )
}

#' @export
makeContent.rich_text_grob <- function(x) {
  # get absolute coordinates of the grob
  x_pt <- convertX(x$x, "pt", valueOnly = TRUE)
  y_pt <- convertY(x$y, "pt", valueOnly = TRUE)
  width_pt <- convertWidth(x$width, "pt", valueOnly = TRUE)

  bl_calc_layout(x$vbox, width_pt, 0)

  w <- bl_box_width(x$vbox)
  h <- bl_box_height(x$vbox)
  grob <- bl_render(x$vbox, x_pt, y_pt)

  x$grobwidth <- unit(w, "pt")
  x$grobheight <- unit(h, "pt")

  setChildren(x, gList(grob))
}

#' @export
heightDetails.rich_text_grob <- function(x) {
  if (is.null(x$grobheight)) {
    x <- makeContent(x)
  }
  x$grobheight
}

#' @export
widthDetails.rich_text_grob <- function(x) {
  if (is.null(x$grobwidth)) {
    x <- makeContent(x)
  }
  x$grobwidth
}

#' @export
ascentDetails.rich_text_grob <- function(x) {
  heightDetails(x)
}

#' @export
descentDetails.rich_text_grob <- function(x) {
  unit(0, "pt")
}
