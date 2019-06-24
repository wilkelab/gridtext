#' Draw a box with content
#'
#' This grob draws a box around another grob specifying content.
#' @param content_grob A grob that represents the contents inside the box.
#' @param width,height Width and height of the box grob. If either is set
#'   to `NULL` (the default), it is calculated from the content grob.
#' @param x,y Unit objects specifying the location of the reference point.
#' @param hjust,vjust Numerical values specifying the location of the box
#'   relative to the reference point.
#' @param padding Unit vector of 4 specifying the padding, in the order
#'   top, right, bottom, left.
#' @param margin Unit vector of 4 specifying the margin, in the order
#'   top, right, bottom, left.
#' @param angle Rotation angle (in degrees)
#' @param fill Fill color of the box.
#' @param color Outline color of the box.
#' @param debug Bool indicating whether debug info should be drawn or not.
#' @examples
#' library(grid)
#'
#' g1 <- textGrob("abcd")
#' g2 <- box_grob(
#'   g1,
#'   angle = 40,
#'   padding = mar(10, 20, 10, 20),
#'   margin = mar(20, 10, 20, 10),
#'   hjust = 0,
#'   vjust = 1,
#'   debug = TRUE
#' )
#' grid.newpage()
#' grid.draw(g2)
#' @export
box_grob <- function(content_grob, width = NULL, height = NULL,
                     x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                     hjust = 0.5, vjust = 0.5, padding = mar(0, 0, 0, 0),
                     margin = mar(0, 0, 0, 0), angle = 0, fill = NA, color = NA,
                     ..., debug = FALSE) {
  if (!is.unit(x)) x <- unit(x, "npc")
  if (!is.unit(y)) y <- unit(y, "npc")

  if (is.null(width)) {
    content_width <- grobWidth(content_grob)
    width <- content_width + margin[2] + margin[4] + padding[2] + padding[4]
  } else {
    content_width <- width - margin[2] - margin[4] - padding[2] - padding[4]
  }
  if (is.null(height)) {
    content_height <- grobHeight(content_grob)
    height <- content_height + margin[1] + margin[3] + padding[1] + padding[3]
  } else {
    content_height <- height - margin[1] - margin[3] - padding[1] - padding[3]
  }

  widths <- unit.c(margin[4], padding[4], content_width, padding[2], margin[2])
  heights <- unit.c(margin[1], padding[1], content_height, padding[3], margin[3])

  vp <- viewport(
    x = x, y = y, just = c(hjust, vjust),
    width = width,
    height = height,
    angle = angle,
    layout = grid.layout(nrow = 5, ncol = 5, widths = widths, heights = heights)
  )

  content_grob <- grobTree(
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

  # calculate corner points
  # (lower left, lower right, upper left, upper right before rotation)
  theta <- angle*2*pi/360
  # lower left
  xll <- x - hjust*cos(theta)*width + vjust*sin(theta)*height
  yll <- y - hjust*sin(theta)*width - vjust*cos(theta)*height
  # lower right
  xlr <- xll + width*cos(theta)
  ylr <- yll + width*sin(theta)
  # upper left
  xul <- xll - height*sin(theta)
  yul <- yll + height*cos(theta)
  # upper right
  xur <- xul + width*cos(theta)
  yur <- yul + width*sin(theta)

  xext <- unit.c(xll, xlr, xul, xur)
  yext <- unit.c(yll, ylr, yul, yur)

  gTree(
    xext = xext,
    yext = yext,
    width = width,
    height = height,
    children = children,
    vp = vp,
    cl = "box_grob"
  )
}

#' @export
heightDetails.box_grob <- function(x) {
  max(x$yext) - min(x$yext)
}

#' @export
widthDetails.box_grob <- function(x) {
  max(x$xext) - min(x$xext)
}

#' @export
ascentDetails.box_grob <- function(x) {
  heightDetails(x)
}

#' @export
descentDetails.box_grob <- function(x) {
  unit(0, "pt")
}

