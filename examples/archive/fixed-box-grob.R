#' Draw a fixed-sized box with content
#'
#' This grob draws a box around another grob specifying content. All sizes need
#' to be specified in absolute units ahead of time; this enables much faster
#' unit calculations.
#' @param content_grob A grob that represents the contents inside the box.
#' @param width_pt,height_pt Width and height of the box grob, in
#'   points ("pt").
#' @param x,y Unit objects specifying the location of the reference point.
#' @param hjust,vjust Numerical values specifying the location of the box
#'   relative to the reference point.
#' @param padding_pt Numeric vector of 4 values specifying the padding
#'   in points, in the order top, right, bottom, left.
#' @param margin_pt Numeric vector of 4 values specifying the margin
#'   in points, in the order top, right, bottom, left.
#' @param angle Rotation angle (in degrees)
#' @param fill Fill color of the box.
#' @param color Outline color of the box.
#' @param debug Bool indicating whether debug info should be drawn or not.
#' @examples
#' library(grid)
#'
#' g1 <- textGrob("abcd")
#' g2 <- fixed_box_grob(
#'   g1,
#'   width_pt = convertWidth(grobWidth(g1), "pt", valueOnly = TRUE) + 60,
#'   height_pt = convertHeight(grobHeight(g1), "pt", valueOnly = TRUE) + 60,
#'   angle = 40,
#'   padding_pt = c(10, 20, 10, 20),
#'   margin_pt = c(20, 10, 20, 10),
#'   hjust = 0,
#'   vjust = 1,
#'   debug = TRUE
#' )
#' grid.newpage()
#' grid.draw(g2)
#' @export
fixed_box_grob <- function(content_grob, width_pt = 100, height_pt = 100,
                     x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                     hjust = 0.5, vjust = 0.5, padding_pt = c(0, 0, 0, 0),
                     margin_pt = c(0, 0, 0, 0), angle = 0, fill = NA, color = NA,
                     debug = FALSE) {
  content_width_pt <- width_pt - margin_pt[2] - margin_pt[4] - padding_pt[2] - padding_pt[4]
  content_height_pt <- height_pt - margin_pt[1] - margin_pt[3] - padding_pt[1] - padding_pt[3]

  widths <- unit(c(margin_pt[4], padding_pt[4], content_width_pt, padding_pt[2], margin_pt[2]), "pt")
  heights <- unit(c(margin_pt[1], padding_pt[1], content_height_pt, padding_pt[3], margin_pt[3]), "pt")

  vp <- viewport(
    x = x, y = y, just = c(hjust, vjust),
    width = unit(width_pt, "pt"),
    height = unit(height_pt, "pt"),
    angle = angle,
    layout = grid.layout(nrow = 5, ncol = 5, widths = widths, heights = heights)
  )

  content_grob <- editGrob(
    content_grob,
    vp = viewport(layout.pos.row = 3, layout.pos.col = 3)
  )

  if (isTRUE(debug)) {
    if (is.na(color)) color <- "black"
    if (is.na(fill)) fill <- "azure1"
  }

  pad_grob <- rectGrob(
    gp = gpar(fill = fill, col = color),
    vp = viewport(layout.pos.row = c(2, 4), layout.pos.col = c(2, 4))
  )

  if (isTRUE(debug)) {
    marg_grob <- rectGrob(
      gp = gpar(fill = "azure2", col = NA),
      vp = viewport(layout.pos.row = c(1, 5), layout.pos.col = c(1, 5))
    )

    point_grob <- pointsGrob(
      unit(hjust, "npc"), unit(vjust, "npc"), pch = 20, gp = gpar(col = "azure4"),
      vp = viewport(layout.pos.row = c(1, 5), layout.pos.col = c(1, 5))
    )

    children <- gList(
      marg_grob, pad_grob, point_grob, content_grob
    )
  } else {
    children <- gList(pad_grob, content_grob)
  }

  # calculate corner points (but setting (x, y) = (0, 0) for speed)
  # (lower left, lower right, upper left, upper right before rotation)
  theta <- angle*2*pi/360
  # lower left
  xll <- - hjust*width_pt*cos(theta) + vjust*height_pt*sin(theta)
  yll <- - hjust*width_pt*sin(theta) - vjust*height_pt*cos(theta)
  # lower right
  xlr <- xll + width_pt*cos(theta)
  ylr <- yll + width_pt*sin(theta)
  # upper left
  xul <- xll - height_pt*sin(theta)
  yul <- yll + height_pt*cos(theta)
  # upper right
  xur <- xul + width_pt*cos(theta)
  yur <- yul + width_pt*sin(theta)

  xext_pt <- c(xll, xlr, xul, xur)
  yext_pt <- c(yll, ylr, yul, yur)

  gTree(
    x = x,
    y = y,
    xext_pt = xext_pt,
    xext = unit(xext_pt, "pt") + x,
    yext_pt = yext_pt,
    yext = unit(yext_pt, "pt") + y,
    width_pt = width_pt,
    height_pt = height_pt,
    children = children,
    vp = vp,
    cl = c("fixed_box_grob", "box_grob")
  )
}

#' @export
heightDetails.fixed_box_grob <- function(x) {
  unit(max(x$yext_pt) - min(x$yext_pt), "pt")
}

#' @export
widthDetails.fixed_box_grob <- function(x) {
  unit(max(x$xext_pt) - min(x$xext_pt), "pt")
}

#' @export
ascentDetails.fixed_box_grob <- function(x) {
  heightDetails(x)
}

#' @export
descentDetails.fixed_box_grob <- function(x) {
  unit(0, "pt")
}

