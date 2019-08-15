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
#' @param hjust,vjust Numerical values specifying the text
#'   justification.
#' @param box_hjust,box_vjust Numerical values specifying the justification
#'   of the text boxes relative to `x` and `y`. If not specified, these
#'   default to `hjust` and `vjust`.
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
#' @param align_widths,align_heights Should the widths and heights of all
#'   the text boxes be aligned? Default is no.
#' @param name Name of the grob.
#' @param gp Other graphical parameters for drawing.
#' @param box_gp Graphical parameters for the enclosing box around each text label.
#' @param vp Viewport.
#' @param use_markdown Should the `text` input be treated as markdown? Default
#'   is yes.
#' @examples
#' library(grid)
#'
#' text <- c(
#'   "Some text **in bold.**", "Linebreaks<br>Linebreaks<br>Linebreaks",
#'   "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
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
#' g <- richtext_grob(
#'   text, x, y, hjust = hjust, vjust = vjust, rot = rot,
#'   padding = unit(c(6, 6, 4, 6), "pt"),
#'   r = unit(c(0, 2, 4, 8), "pt"),
#'   gp = gp, box_gp = box_gp
#' )
#' grid.draw(g)
#' grid.points(x, y, default.units = "npc", pch = 19, size = unit(5, "pt"))
#' @export
richtext_grob <- function(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                          hjust = 0.5, vjust = 0.5, box_hjust = hjust, box_vjust = vjust,
                          rot = 0, default.units = "npc",
                          margin = unit(c(0, 0, 0, 0), "pt"), padding = unit(c(0, 0, 0, 0), "pt"),
                          r = unit(0, "pt"), align_widths = FALSE, align_heights = FALSE,
                          name = NULL, gp = gpar(), box_gp = gpar(col = NA), vp = NULL,
                          use_markdown = TRUE) {
  # make sure x and y are units
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  # make sure we can handle input text even if provided as factor
  text <- as.character(text)

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
  # need to convert x and y to lists so mapply can handle them properly
  x_list <- unit_to_list(x)
  y_list <- unit_to_list(y)

  inner_boxes <- mapply(
    make_inner_box,
    text,
    hjust,
    vjust,
    use_markdown,
    gp_list,
    SIMPLIFY = FALSE
  )

  # do we have to align the contents box sizes?
  if (isTRUE(align_widths) || isTRUE(align_heights)) {
    # yes, obtain max width and/or height as needed
    lapply(inner_boxes, bl_calc_layout)
    width <- vapply(inner_boxes, bl_box_width, numeric(1))
    height <-  vapply(inner_boxes, bl_box_height, numeric(1))
  }

  if (isTRUE(align_widths)) {
    width <- max(width)
  } else {
    width <- list(NULL)
  }
  if (isTRUE(align_heights)) {
    height <- max(height)
  } else {
    height <- list(NULL)
  }

  grobs <- mapply(
    make_outer_box,
    inner_boxes,
    width,
    height,
    x_list,
    y_list,
    hjust,
    vjust,
    box_hjust,
    box_vjust,
    rot,
    list(margin_pt),
    list(padding_pt),
    r_pt,
    box_gp_list,
    SIMPLIFY = FALSE
  )

  children <- do.call(gList, grobs)

  gTree(
    gp = gp,
    vp = vp,
    name = name,
    children = children,
    cl = "richtext_grob"
  )
}


make_inner_box <- function(text, hjust, vjust, use_markdown, gp) {
  if (use_markdown) {
    text <- markdown::markdownToHTML(text = text, options = c("use_xhtml", "fragment_only"))
  }
  doctree <- read_html(text)

  drawing_context <- setup_context(gp = gp, hjust = hjust)
  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox_inner <- bl_make_vbox(boxlist, vjust = 0, width_policy = "native")

  vbox_inner
}

make_outer_box <- function(vbox_inner, width, height, x, y, hjust, vjust,
                           box_hjust, box_vjust, rot,
                           margin_pt, padding_pt, r_pt, box_gp) {
  if (is.null(width)) {
    width <- 0
    width_policy <- "native"
  } else {
    width <- width + margin_pt[2] + margin_pt[4] + padding_pt[2] + padding_pt[4] # make space for margin and padding
    width_policy <- "fixed"
  }

  if (is.null(height)) {
    height <- 0
    height_policy <- "native"
  } else {
    height <- height + margin_pt[1] + margin_pt[3] + padding_pt[1] + padding_pt[3] # make space for margin and padding
    height_policy <- "fixed"
  }

  rect_box <- bl_make_rect_box(
    vbox_inner, width, height, margin_pt, padding_pt, box_gp,
    content_hjust = hjust, content_vjust = vjust,
    width_policy = width_policy, height_policy = height_policy, r = r_pt
  )
  vbox_outer <- bl_make_vbox(list(rect_box), hjust = box_hjust, vjust = box_vjust, width_policy = "native")

  bl_calc_layout(vbox_outer)
  grobs <- bl_render(vbox_outer)

  # calculate corner points
  # (We exclude x, y and keep everything in pt, to avoid unit calculations at this stage)
  # (lower left, lower right, upper left, upper right before rotation)
  theta <- rot*2*pi/360
  width <- bl_box_width(vbox_outer)
  height <- bl_box_height(vbox_outer)
  # lower left
  xll <- box_hjust*cos(theta)*width + box_vjust*sin(theta)*height
  yll <- box_hjust*sin(theta)*width - box_vjust*cos(theta)*height
  # lower right
  xlr <- xll + width*cos(theta)
  ylr <- yll + width*sin(theta)
  # upper left
  xul <- xll - height*sin(theta)
  yul <- yll + height*cos(theta)
  # upper right
  xur <- xul + width*cos(theta)
  yur <- yul + width*sin(theta)

  xext <- c(xll, xlr, xul, xur)
  yext <- c(yll, ylr, yul, yur)

  gTree(
    x = x,
    y = y,
    xext = xext,
    yext = yext,
    children = grobs,
    vp = viewport(x = x, y = y, just = c(0, 0), angle = rot)
  )
}



#' @export
heightDetails.richtext_grob <- function(x) {
  grobs <- x$children
  if (length(grobs) == 1) {
    # shortcut for grobs with just one child; unit calcs not needed
    unit(max(grobs[[1]]$yext) - min(grobs[[1]]$yext), "pt")
  } else {
    # get ymax and ymin values for each child grob
    ymax <- lapply(grobs, function(x) {x$y + unit(max(x$yext), "pt")})
    ymin <- lapply(grobs, function(x) {x$y + unit(min(x$yext), "pt")})
    # now return the difference between the overal max and the overall min
    do.call(max, ymax) - do.call(min, ymin)
  }
}

#' @export
widthDetails.richtext_grob <- function(x) {
  grobs <- x$children
  if (length(grobs) == 1) {
    # shortcut for grobs with just one child; unit calcs not needed
    unit(max(grobs[[1]]$xext) - min(grobs[[1]]$xext), "pt")
  } else {
    # get xmax and xmin values for each child grob
    xmax <- lapply(grobs, function(x) {x$x + unit(max(x$xext), "pt")})
    xmin <- lapply(grobs, function(x) {x$x + unit(min(x$xext), "pt")})
    # now return the difference between the overal max and the overall min
    do.call(max, xmax) - do.call(min, xmin)
  }
}

#' @export
ascentDetails.richtext_grob <- function(x) {
  heightDetails(x)
}

#' @export
descentDetails.richtext_grob <- function(x) {
  unit(0, "pt")
}


