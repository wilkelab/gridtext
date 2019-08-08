#' Rich-text grob
#'
#' This grob acts mostly as a drop-in replacement for [`grid::textGrob()`]
#' but provides more sophisticated formatting. The grob can handle basic
#' markdown and HTML formatting directives, and it can also draw
#' boxes around each piece of text. Note that this grob **does not** draw
#' [plotmath] expressions.
#'
#' @param text Character vector containing markdown/html strings to draw.
#' @param x,y Unit objects specifying the location of the reference point.
#' @param hjust,vjust Numerical values specifying the location of the grob
#'   relative to the reference point.
#' @param rot Angle of rotation for text, in degrees.
#' @param default.units Units of `x` and `y` if these are provided only as
#'   numerical values.
#' @param margin,padding Unit vectors of four elements each indicating the
#'   margin and padding around each text label in the order top, right,
#'   bottom, left. Margins are drawn outside the enclosing box (if any),
#'   and padding is drawn inside. To avoid rendering artifacts, it is best
#'   to specify these values in absolute units (such as points, mm, or inch)
#'   rather than in relative units (such as npc).
#' @param r The radius of the rounded corners. To avoid rendering artifacts,
#'   it is best to specify this in absolute units (such as points, mm, or inch)
#'   rather than in relative units (such as npc).
#' @param name Name of the grob.
#' @param gp Other graphical parameters for drawing.
#' @param box_gp Graphical parameters for the enclosing box around each text label.
#' @param vp Viewport.
#' @param use_markdown Should the `text` input be treated as markdown?
#' @examples
#' library(grid)
#'
#' text <- c(
#'   "Some text **in bold.**", "Linebreaks<br>Linebreaks<br>Linebreaks",
#'   "*x*<sup>2</sup> + 5*x* + *C*<sub>i</sub>",
#'   "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
#'    And some <span style='font-size:18; color:black'>large</span> text."
#' )
#'
#' x <- c(.2, .1, .7, .9)
#' y <- c(.8, .4, .1, .5)
#' rot <- c(0, 0, 45, -45)
#' gp = gpar(col = c("black", "red"), fontfamily = c("Palatino", "Courier", "Times", "Helvetica"))
#' box_gp = gpar(col = "black", fill = c("cornsilk", NA, "lightblue1", NA), lty = c(0, 1, 1, 1))
#' hjust <- c(0.5, 0, 0, 1)
#' vjust <- c(0.5, 1, 0, 0.5)
#'
#' grid.newpage()
#' g <- rich_text_grob(
#'   text, x, y, hjust = hjust, vjust = vjust, rot = rot,
#'   padding = unit(c(6, 6, 4, 6), "pt"),
#'   r = unit(c(0, 2, 4, 8), "pt"),
#'   gp = gp, box_gp = box_gp
#' )
#' grid.draw(g)
#' grid.points(x, y, default.units = "npc", pch = 19, size = unit(5, "pt"))
#' @export
rich_text_grob <- function(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           hjust = 0.5, vjust = 0.5, rot = 0, default.units = "npc",
                           margin = unit(c(0, 0, 0, 0), "pt"), padding = unit(c(0, 0, 0, 0), "pt"),
                           r = unit(0, "pt"),
                           name = NULL, gp = gpar(), box_gp = gpar(), vp = NULL,
                           use_markdown = TRUE) {
  # make sure x and y are units
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  # margin, padding, and r need to be in points
  margin_pt <- rep(0, 4)
  margin_pt[c(1, 3)] <- convertHeight(margin[c(1, 3)], "pt", valueOnly = TRUE)
  margin_pt[c(2, 4)] <- convertWidth(margin[c(2, 4)], "pt", valueOnly = TRUE)
  padding_pt <- rep(0, 4)
  padding_pt[c(1, 3)] <- convertHeight(padding[c(1, 3)], "pt", valueOnly = TRUE)
  padding_pt[c(2, 4)] <- convertWidth(padding[c(2, 4)], "pt", valueOnly = TRUE)
  r_pt <- convertUnit(r, "pt", valueOnly = TRUE)

  # make sure text, x, and y have the same length
  n <- unique(length(text), length(x), length(y))
  if (length(n) > 1) {
    stop("Arguments `text`, `x`, and `y` must have the same length.", call. = FALSE)
  }
  gp_list <- recycle_gpar(gp, n)
  box_gp_list <- recycle_gpar(box_gp, n)

  grobs <- mapply(
    make_rich_text_grob,
    text,
    x,
    y,
    hjust,
    vjust,
    rot,
    list(margin_pt),
    list(padding_pt),
    r_pt,
    use_markdown,
    gp_list,
    box_gp_list,
    SIMPLIFY = FALSE
  )

  children <- do.call(gList, grobs)

  gTree(
    gp = gp,
    vp = vp,
    name = name,
    children = children,
    cl = "rich_text_grob"
  )
}

make_rich_text_grob <- function(text, x, y, hjust, vjust, rot, margin_pt, padding_pt, r_pt,
                                use_markdown, gp, box_gp) {
  if (use_markdown) {
    text <- markdown::markdownToHTML(text = text, options = c("use_xhtml", "fragment_only"))
  }
  doctree <- read_html(text)

  drawing_context <- setup_context(gp = gp)
  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox_inner <- bl_make_vbox(boxlist, vjust = 0, width_policy = "native")

  rect_box <- bl_make_rect_box(
    vbox_inner, 0, 0, margin_pt, padding_pt, box_gp,
    content_hjust = 0, content_vjust = 0,
    width_policy = "native", height_policy = "native", r = r_pt
  )
  vbox_outer <- bl_make_vbox(list(rect_box), hjust = hjust, vjust = vjust, width_policy = "native")

  bl_calc_layout(vbox_outer)
  grobs <- bl_render(vbox_outer)

  gTree(
    children = grobs,
    vp = viewport(x = x, y = y, just = c(0, 0), angle = rot)
  )
}
