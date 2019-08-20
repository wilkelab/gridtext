#' Textbox grob
#'
#' @param text Character vector containing markdown/html string to draw.
#' @param x,y Unit objects specifying the location of the reference point.
#'   If set to `NULL` (the default), these values are chosen based on the
#'   values of `box_hjust` and `box_vjust` such that the box is appropriately
#'   justified in the enclosing viewport.
#' @param width,height Unit objects specifying width and height of the
#'   grob; a value of `NULL` means take up all available space.
#' @param minwidth,minheight,maxwidth,maxheight Min and max values for
#'   width and height. Set to `NULL` to impose neither a minimum nor
#'   a maximum.
#' @param hjust,vjust Numerical values specifying the location of the text
#'   inside the text box.
#' @param box_hjust,box_vjust Numerical values specifying the justification
#'   of the text box relative to the reference point defined by `x` and `y`.
#' @param default.units Units of `x`, `y`, `width`, `height`, `minwidth`,
#'   `minheight`, `maxwidth`, `maxheight` if these are provided only as
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
textbox_grob <- function(text, x = NULL, y = NULL,
                         width = NULL, height = NULL,
                         minwidth = NULL, maxwidth = NULL,
                         minheight = NULL, maxheight = NULL,
                         hjust = 0, vjust = 1,
                         box_hjust = 0.5, box_vjust = 0.5, default.units = "npc",
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
  minwidth <- with_unit(minwidth, default.units)
  minheight <- with_unit(minheight, default.units)
  maxwidth <- with_unit(maxwidth, default.units)
  maxheight <- with_unit(maxheight, default.units)

  # make sure we can handle input text even if provided as factor
  text <- as.character(text)

  # determine orientation and adjust accordingly
  orientation <- match.arg(orientation)
  if (orientation == "upright") {
    angle <- 0
    if (is.null(x)) {
      x <- unit(box_hjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(box_vjust, "npc")
    }
    flip <- FALSE
  } else if (orientation == "left-rotated") {
    angle <- 90
    if (is.null(x)) {
      x <- unit(1-box_vjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(box_hjust, "npc")
    }
    flip <- TRUE
  } else if (orientation == "right-rotated") {
    angle <- -90
    if (is.null(x)) {
      x <- unit(box_vjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(1-box_hjust, "npc")
    }
    flip <- TRUE
  } else if (orientation == "inverted") {
    angle <- 180
    if (is.null(x)) {
      x <- unit(1-box_hjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(1-box_vjust, "npc")
    }
    flip <- FALSE
  }

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

  drawing_context <- setup_context(gp = gp, hjust = hjust, word_wrap = TRUE)
  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox_inner <- bl_make_vbox(boxlist, vjust = 0, width_pt = 100, width_policy = "relative")

  gTree(
    width = width,
    height = height,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    box_hjust = box_hjust,
    box_vjust = box_vjust,
    minwidth = minwidth,
    minheight = minheight,
    maxwidth = maxwidth,
    maxheight = maxheight,
    angle = angle,
    flip = flip,
    vbox_inner = vbox_inner,
    margin_pt = margin_pt,
    padding_pt = padding_pt,
    r_pt = r_pt,
    gp = gp,
    box_gp = box_gp,
    vp = vp,
    cl = "textbox_grob"
  )
}

#' @export
makeContext.textbox_grob <- function(x) {
  width_pt <- current_width_pt(x, x$width, x$flip)
  minwidth_pt <- current_width_pt(x, x$minwidth, x$flip, convert_null = FALSE)
  maxwidth_pt <- current_width_pt(x, x$maxwidth, x$flip, convert_null = FALSE)

  if (!is.null(minwidth_pt) && width_pt < minwidth_pt) {
    width_pt <- minwidth_pt
  }
  if (!is.null(maxwidth_pt) && width_pt > maxwidth_pt) {
    width_pt <- maxwidth_pt
  }

  height_pt <- current_height_pt(x, x$height, x$flip, convert_null = FALSE)
  minheight_pt <- current_height_pt(x, x$minheight, x$flip, convert_null = FALSE)
  maxheight_pt <- current_height_pt(x, x$maxheight, x$flip, convert_null = FALSE)

  if (is.null(height_pt)) {
    # if height is not set it is taken from the layout
    rect_box <- bl_make_rect_box(
      x$vbox_inner, 100, 0, x$margin_pt, x$padding_pt, x$box_gp,
      content_hjust = x$hjust, content_vjust = x$vjust,
      width_policy = "relative", height_policy = "native", r = x$r_pt
    )
  } else {
    # otherwise, set explicit height
    rect_box <- bl_make_rect_box(
      x$vbox_inner, 100, height_pt, x$margin_pt, x$padding_pt, x$box_gp,
      content_hjust = x$hjust, content_vjust = x$vjust,
      width_policy = "relative", height_policy = "fixed", r = x$r_pt
    )
  }
  vbox_outer <- bl_make_vbox(list(rect_box), width_pt = 100, hjust = x$box_hjust, vjust = x$box_vjust, width_policy = "relative")
  bl_calc_layout(vbox_outer, width_pt)
  width_pt <- bl_box_width(vbox_outer)
  height_pt <- bl_box_height(vbox_outer)

  # check if height needs to be adjusted, and relayout if necessary
  relayout <- FALSE
  if (!is.null(minheight_pt) && height_pt < minheight_pt) {
    height_pt <- minheight_pt
    relayout <- TRUE
  }
  if (!is.null(maxheight_pt) && height_pt > maxheight_pt) {
    height_pt <- maxheight_pt
    relayout <- TRUE
  }
  if (relayout) {
    rect_box <- bl_make_rect_box(
      x$vbox_inner, 100, height_pt, x$margin_pt, x$padding_pt, x$box_gp,
      content_hjust = x$hjust, content_vjust = x$vjust,
      width_policy = "relative", height_policy = "fixed", r = x$r_pt
    )
    vbox_outer <- bl_make_vbox(list(rect_box), width_pt = 100, hjust = x$box_hjust, vjust = x$box_vjust, width_policy = "relative")
    bl_calc_layout(vbox_outer, width_pt)
    width_pt <- bl_box_width(vbox_outer)
    height_pt <- bl_box_height(vbox_outer)
  }

  x$vbox_outer <- vbox_outer

  if (isTRUE(x$flip)) {
    x$width_pt <- height_pt
    x$height_pt <- width_pt
  } else {
    x$width_pt <- width_pt
    x$height_pt <- height_pt
  }

  vp <- viewport(x$x, x$y, just = c(x$box_hjust, x$box_vjust), angle = x$angle)
  if (is.null(x$vp)) {
    x$vp <- vp
  } else {
    x$vp <- vpStack(x$vp, vp)
  }
  x
}

#' @export
makeContent.textbox_grob <- function(x) {
  # get absolute coordinates of the grob
  x_pt <- convertX(unit(x$box_hjust, "npc"), "pt", valueOnly = TRUE)
  y_pt <- convertY(unit(x$box_vjust, "npc"), "pt", valueOnly = TRUE)

  grobs <- bl_render(x$vbox_outer, x_pt, y_pt)

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
