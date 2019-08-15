#' Textbox grob
#'
#' @param text Character vector containing markdown/html string to draw.
#' @param x,y Unit objects specifying the location of the reference point.
#' @param width,height Unit objects specifying width and height of the
#'   grob; a value of `NULL` means take up all available space.
#'   The `height` setting is currently ignored.
#' @param hjust,vjust Numerical values specifying the location of the grob
#'   relative to the reference point.
#' @param default.units Units of `x`, `y`, `width`, `height` if these are
#'   provided only as numerical values.
#' @param margin,padding Unit vectors of four elements each indicating the
#'   margin and padding around each text label in the order top, right,
#'   bottom, left. Margins are drawn outside the enclosing box (if any),
#'   and padding is drawn inside. To avoid rendering artifacts, it is best
#'   to specify these values in absolute units (such as points, mm, or inch)
#'   rather than in relative units (such as npc).
#' @param r The radius of the rounded corners. To avoid rendering artifacts,
#'   it is best to specify this in absolute units (such as points, mm, or inch)
#'   rather than in relative units (such as npc).
#' @param orientation Orientation of the box. Allowed values are `"upright"`,
#'   `"left-rotated"`, `"right-rotated"`, and `"inverted"`, corresponding to
#'   a rotation by 0, 90, 270, and 180 degrees counter-clockwise, respectively.
#' @param name Name of the grob.
#' @param gp Other graphical parameters for drawing.
#' @param box_gp Graphical parameters for the enclosing box around each text label.
#' @param vp Viewport.
#' @param use_markdown Should the `text` input be treated as markdown?
#' @examples
#' library(grid)
#' g <- textbox_grob(
#'   "**The quick brown fox jumps over the lazy dog.**<br><br>
#'   The quick brown fox jumps over the lazy dog.
#'   The **quick <span style='color:brown;'>brown fox</span>** jumps over the lazy dog.
#'   The quick brown fox jumps over the lazy dog.",
#'   x = unit(0, "npc"), y = unit(0.9, "npc"), hjust = 0, vjust = 1,
#'   gp = gpar(fontsize = 15),
#'   box_gp = gpar(col = "black", fill = "lightcyan1"),
#'   r = unit(5, "pt"),
#'   padding = unit(c(10, 10, 10, 10), "pt"),
#'   margin = unit(c(0, 10, 0, 10), "pt")
#' )
#' grid.newpage()
#' grid.draw(g)
#' @export
textbox_grob <- function(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                         width = NULL, height = NULL,
                         hjust = 0.5, vjust = 0.5, default.units = "npc",
                         margin = unit(c(0, 0, 0, 0), "pt"), padding = unit(c(0, 0, 0, 0), "pt"),
                         r = unit(0, "pt"),
                         orientation = c("upright", "left-rotated", "right-rotated", "inverted"),
                         name = NULL, gp = gpar(), box_gp = gpar(col = NA), vp = NULL,
                         use_markdown = TRUE) {
  # make sure x, y, width, height are units
  x <- with_unit(x, default.units)
  y <- with_unit(y, default.units)
  width <- with_unit(width, default.units)
  height <- with_unit(height, default.units)

  # make sure we can handle input text even if provided as factor
  text <- as.character(text)

  # determine orientation and act accordingly
  orientation <- match.arg(orientation)
  ## TODO: swap width/height etc., set up rotation angle

  # margin, padding, and r need to be in points
  margin_pt <- rep(0, 4)
  margin_pt[c(1, 3)] <- convertHeight(margin[c(1, 3)], "pt", valueOnly = TRUE)
  margin_pt[c(2, 4)] <- convertWidth(margin[c(2, 4)], "pt", valueOnly = TRUE)
  padding_pt <- rep(0, 4)
  padding_pt[c(1, 3)] <- convertHeight(padding[c(1, 3)], "pt", valueOnly = TRUE)
  padding_pt[c(2, 4)] <- convertWidth(padding[c(2, 4)], "pt", valueOnly = TRUE)
  r_pt <- convertUnit(r, "pt", valueOnly = TRUE)

  # make sure text, x, y, and width have at most length 1
  n <- max(length(text), length(x), length(y), length(width), length(height))
  if (n > 1) {
    stop("The function textbox_grob() is not vectorized.", call. = FALSE)
  }

  # now parse html
  if (use_markdown) {
    text <- markdown::markdownToHTML(text = text, options = c("use_xhtml", "fragment_only"))
  }
  doctree <- read_html(text)

  drawing_context <- setup_context(gp = gp)
  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox_inner <- bl_make_vbox(boxlist, vjust = 0, width_pt = 100, width_policy = "relative")

  rect_box <- bl_make_rect_box(
    vbox_inner, 100, 0, margin_pt, padding_pt, box_gp,
    content_hjust = 0, content_vjust = 0,
    width_policy = "relative", height_policy = "native", r = r_pt
  )

  vbox_outer <- bl_make_vbox(list(rect_box), width_pt = 100, hjust = 0, vjust = 0, width_policy = "relative")

  gTree(
    width = width,
    height = height,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    vbox_outer = vbox_outer,
    gp = gp,
    box_gp = box_gp,
    cl = "textbox_grob"
  )
}

#' @export
makeContext.textbox_grob <- function(x) {
  # get absolute coordinates of the grob
  x_pt <- convertX(x$x, "pt", valueOnly = TRUE)
  y_pt <- convertY(x$y, "pt", valueOnly = TRUE)

  width_pt <- current_width_pt(x, x$width)

  bl_calc_layout(x$vbox_outer, width_pt)
  width_pt <- bl_box_width(x$vbox_outer)
  height_pt <- bl_box_height(x$vbox_outer)

  x$width_pt <- width_pt
  x$height_pt <- height_pt
  x$x_pt <- x_pt - x$hjust*width_pt
  x$y_pt <- y_pt - x$vjust*height_pt

  x
}

#' @export
makeContent.textbox_grob <- function(x) {
  grobs <- bl_render(x$vbox_outer, x$x_pt, x$y_pt)

  setChildren(x, grobs)
}


#' @export
heightDetails.textbox_grob <- function(x) {
  unit(x$height_pt, "pt")
}

#' @export
widthDetails.textbox_grob <- function(x) {
  unit(x$width_pt, "pt")
}

#' @export
ascentDetails.textbox_grob <- function(x) {
  unit(x$height_pt, "pt")
}

#' @export
descentDetails.textbox_grob <- function(x) {
  unit(0, "pt")
}
