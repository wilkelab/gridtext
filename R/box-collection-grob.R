#' Draw a collection of box grobs
#'
#' Draw a collection of box grobs.
#' @param ... One or more box grobs.
#' @param width,height Width and height of the box collection grob. If either is set
#'   to `NULL` (the default), it is calculated from the box grobs.
#' @param vp Grid viewport.
#' @param debug Bool indicating whether debug info should be drawn or not.
#' @examples
#' # no examples yet
#' @export
box_collection_grob <- function(..., width = NULL, height = NULL,
                                vp = NULL, debug = FALSE)
{
  children <- list(...)

  # are all provided grobs of class box_grob?
  if (!all(vapply(children, function(x) inherits(x, "box_grob"), logical(1)))) {
    stop("All grobs must be box grobs.", call. = FALSE)
  }

  # calculate bounding box
  xexts <- do.call(unit.c, lapply(children, function(x) x$xext))
  yexts <- do.call(unit.c, lapply(children, function(x) x$yext))
  xmin <- min(xexts)
  xmax <- max(xexts)
  ymin <- min(yexts)
  ymax <- max(yexts)

  if (is.null(width)) {
    width <- xmax - xmin
  }

  if (is.null(height)) {
    height <- ymax - ymin
  }

  if (isTRUE(debug)) {
    outline_grob <- rectGrob(
      x = 0.5*(xmax + xmin), y = 0.5*(ymax + ymin), width = width, height = height,
      hjust = 0.5, vjust = 0.5,
      gp = gpar(fill = NA, col = "black")
    )

    children <- c(children, list(outline_grob))
  }

  gTree(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    width = width,
    height = height,
    children = do.call(gList, children),
    vp = vp,
    cl = "box_collection_grob"
  )
}

heightDetails.box_collection_grob <- function(x) {
  x$height
}
widthDetails.box_collection_grob <- function(x) {
  x$width
}
ascentDetails.box_collection_grob <- function(x) {
  heightDetails(x)
}
descentDetails.box_collection_grob <- function(x) {
  unit(0, "pt")
}

