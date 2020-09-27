#' Draw formatted multi-line text with word wrap
#'
#' The function `textbox_grob()` is intended to render multi-line text
#' labels that require automatic word wrapping. It is similar to
#' [`richtext_grob()`], but there are a few important differences. First,
#' while [`richtext_grob()`] is vectorized, `textbox_grob()` is not. It
#' can draw only a single text box at a time. Second, `textbox_grob()`
#' doesn't support rendering the text box at arbitrary angles. Only
#' four different orientations are supported, corresponding to a
#' rotation by 0, 90, 180, and 270 degrees.
#'
#' @param text Character vector containing Markdown/HTML string to draw.
#' @param x,y Unit objects specifying the location of the reference point.
#'   If set to `NULL` (the default), these values are chosen based on the
#'   values of `hjust` and `vjust` such that the box is appropriately
#'   justified in the enclosing viewport.
#' @param width,height Unit objects specifying width and height of the
#'   grob. A value of `NULL` means take up exactly the space necessary
#'   to render all content. Use a value of `unit(1, "npc")` to have the
#'   box take up all available space.
#' @param minwidth,minheight,maxwidth,maxheight Min and max values for
#'   width and height. Set to `NULL` to impose neither a minimum nor
#'   a maximum. Note: `minheight` and `maxheight` do not work if `width = NULL`.
#' @param hjust,vjust Numerical values specifying the justification
#'   of the text box relative to the reference point defined by `x` and `y`. These
#'   justification parameters are specified in the internal reference frame of
#'   the text box, so that, for example, `hjust` adjusts the vertical
#'   justification when the text box is left- or right-rotated.
#' @param halign,valign Numerical values specifying the justification of the text
#'   inside the text box.
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
#' @return A grid [`grob`] that represents the formatted text.
#' @seealso [`richtext_grob()`]
#' @examples
#' library(grid)
#' g <- textbox_grob(
#'   "**The quick brown fox jumps over the lazy dog.**<br><br>
#'   The quick brown fox jumps over the lazy dog.
#'   The **quick <span style='color:brown;'>brown fox</span>** jumps over the lazy dog.
#'   The quick brown fox jumps over the lazy dog.",
#'   x = unit(0.5, "npc"), y = unit(0.7, "npc"), halign = 0, valign = 1,
#'   gp = gpar(fontsize = 15),
#'   box_gp = gpar(col = "black", fill = "lightcyan1"),
#'   r = unit(5, "pt"),
#'   padding = unit(c(10, 10, 10, 10), "pt"),
#'   margin = unit(c(0, 10, 0, 10), "pt")
#' )
#' grid.newpage()
#' grid.draw(g)
#'
#' # internal vs. external alignment
#' g1 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 0, vjust = 1, halign = 0, valign = 1,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' g2 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 1, vjust = 1, halign = 0.5, valign = 0.5,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' g3 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 0, vjust = 0, halign = 1, valign = 1,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' g4 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 1, vjust = 0, halign = 0, valign = 0,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' grid.newpage()
#' grid.draw(g1)
#' grid.draw(g2)
#' grid.draw(g3)
#' grid.draw(g4)
#'
#' # internal vs. external alignment, with rotated boxes
#' g1 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 1, vjust = 1, halign = 0, valign = 1,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   orientation = "left-rotated",
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' g2 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 0, vjust = 1, halign = 0.5, valign = 0.5,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   orientation = "right-rotated",
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' g3 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 1, vjust = 1, halign = 1, valign = 1,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   orientation = "inverted",
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' g4 <- textbox_grob(
#'   "The quick brown fox jumps over the lazy dog.",
#'   hjust = 1, vjust = 0, halign = 0, valign = 0,
#'   width = unit(1.5, "inch"), height = unit(1.5, "inch"),
#'   orientation = "upright",
#'   box_gp = gpar(col = "black", fill = "cornsilk"),
#'   padding = unit(c(2, 2, 2, 2), "pt"),
#'   margin = unit(c(5, 5, 5, 5), "pt")
#' )
#' grid.newpage()
#' grid.draw(g1)
#' grid.draw(g2)
#' grid.draw(g3)
#' grid.draw(g4)
#' @export
textbox_grob <- function(text, x = NULL, y = NULL,
                         width = unit(1, "npc"), height = NULL,
                         minwidth = NULL, maxwidth = NULL,
                         minheight = NULL, maxheight = NULL,
                         hjust = 0.5, vjust = 0.5, halign = 0, valign = 1,
                         default.units = "npc",
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
  # convert NAs to empty strings
  text <- ifelse(is.na(text), "", text)

  # determine orientation and adjust accordingly
  orientation <- match.arg(orientation)
  if (orientation == "upright") {
    angle <- 0
    if (is.null(x)) {
      x <- unit(hjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(vjust, "npc")
    }
    flip <- FALSE
  } else if (orientation == "left-rotated") {
    angle <- 90
    if (is.null(x)) {
      x <- unit(1-vjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(hjust, "npc")
    }
    flip <- TRUE
  } else if (orientation == "right-rotated") {
    angle <- -90
    if (is.null(x)) {
      x <- unit(vjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(1-hjust, "npc")
    }
    flip <- TRUE
  } else if (orientation == "inverted") {
    angle <- 180
    if (is.null(x)) {
      x <- unit(1-hjust, "npc")
    }
    if (is.null(y)) {
      y <- unit(1-vjust, "npc")
    }
    flip <- FALSE
  }

  # make sure margin and padding are of length 4
  margin <- rep(margin, length.out = 4)
  padding <- rep(padding, length.out = 4)

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
  doctree <- read_html(paste0("<!DOCTYPE html>", text))

  # if width is set to NULL, we use the native size policy and turn off word wrap
  if (is.null(width)) {
    width_policy <- "native"
    word_wrap <- FALSE
  } else {
    width_policy <- "relativce"
    word_wrap <- TRUE
  }

  drawing_context <- setup_context(gp = gp, halign = halign, word_wrap = word_wrap)
  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox_inner <- bl_make_vbox(boxlist, vjust = 0, width_pt = 100, width_policy = width_policy)

  gTree(
    width = width,
    height = height,
    x = x,
    y = y,
    halign = halign,
    valign = valign,
    hjust = hjust,
    vjust = vjust,
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
    name = name,
    cl = "textbox_grob"
  )
}

#' @export
makeContext.textbox_grob <- function(x) {
  if (is.null(x$width)) {
    width_policy <- "native"
  } else {
    width_policy <- "fixed"
  }

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
    height_pt <- 0
    height_policy <- "native"
  } else {
    height_policy <- "fixed"
  }

  rect_box <- bl_make_rect_box(
    x$vbox_inner, width_pt, height_pt, x$margin_pt, x$padding_pt, x$box_gp,
    content_hjust = x$halign, content_vjust = x$valign,
    width_policy = width_policy, height_policy = height_policy, r = x$r_pt
  )
  vbox_outer <- bl_make_vbox(
    list(rect_box), width_pt = width_pt,
    hjust = x$hjust, vjust = x$vjust, width_policy = width_policy
  )
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
      x$vbox_inner, width_pt, height_pt, x$margin_pt, x$padding_pt, x$box_gp,
      content_hjust = x$halign, content_vjust = x$valign,
      width_policy = width_policy, height_policy = "fixed", r = x$r_pt
    )
    vbox_outer <- bl_make_vbox(list(rect_box), width_pt = width_pt, hjust = x$hjust, vjust = x$vjust, width_policy = width_policy)
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

  vp <- viewport(x$x, x$y, just = c(x$hjust, x$vjust), angle = x$angle)
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
  x_pt <- convertX(unit(x$hjust, "npc"), "pt", valueOnly = TRUE)
  y_pt <- convertY(unit(x$vjust, "npc"), "pt", valueOnly = TRUE)

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
