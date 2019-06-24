render_line <- function(line, linex_pt = 0, liney_pt = 0) {
  x_pt <- cumsum(line$width_pt) - line$width_pt + linex_pt
  y_pt <- liney_pt + line$yoff_pt
  tbl <- tibble(grob = line$grob, x_pt = x_pt, y_pt = y_pt)

  pmap(
    tbl,
    function(grob, x_pt, y_pt) {
      editGrob(grob, vp = viewport(x = x_pt, y = y_pt, default.units = "pt", just = c(0, 0)))
    }
  )
}

render_lines <- function(lines, hjust = 0, x_pt = 0, y_pt = 0, vp = NULL) {
  linewidths_pt <- vapply(lines, function(x) sum(x$width_pt), numeric(1))
  lineheights_pt <- vapply(lines, function(x) max(x$height_pt), numeric(1))
  linedesc_pt <- vapply(lines, function(x) max(x$descent_pt), numeric(1))
  linespacing_pt <- vapply(lines, function(x) max(x$linespacing_pt), numeric(1))

  maxwidth_pt <- max(linewidths_pt)
  linex_pt <- (maxwidth_pt - linewidths_pt) * hjust

  # get the last non-zero descent
  lastdesc_pt <- linedesc_pt[linedesc_pt != 0]
  lastdesc_pt <- lastdesc_pt[length(lastdesc_pt)]
  liney_pt <- lastdesc_pt + sum(linespacing_pt) + y_pt - # base reference point
    cumsum(linespacing_pt) # line position

  tbl <- tibble(line = lines, linex_pt = linex_pt, liney_pt = liney_pt)

  grobs <- unlist(pmap(tbl, render_line), recursive = FALSE)
  children <- do.call(gList, grobs)
  # highlight reference point for debugging
  #children <- gList(children, pointsGrob(x = unit(0, "npc"), y = unit(0, "npc")))

  gTree(
    width_pt = maxwidth_pt,
    height_pt = sum(linespacing_pt),
    children = children,
    vp = vp,
    cl = "rich_text_grob"
  )
}

#' @export
heightDetails.rich_text_grob <- function(x) {
  unit(x$height_pt, "pt")
}

#' @export
widthDetails.rich_text_grob <- function(x) {
  unit(x$width_pt, "pt")
}

#' @export
ascentDetails.rich_text_grob <- function(x) {
  heightDetails(x)
}

#' @export
descentDetails.rich_text_grob <- function(x) {
  unit(0, "pt")
}


