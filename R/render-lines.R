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

render_line_old <- function(line, linex_pt = 0, liney_pt = 0) {
  line$x_pt <- cumsum(line$width_pt) - line$width_pt + linex_pt
  grobs <- map2(
    line$grob, line$x_pt,
    ~editGrob(.x, vp = viewport(x = .y, y = liney_pt, default.units = "pt", just = c(0, 0)))
  )
}

render_lines <- function(lines, hjust = 0, x_pt = 0, y_pt = 0) {
  linewidths_pt <- vapply(lines, function(x) sum(x$width_pt), numeric(1))
  lineheights_pt <- vapply(lines, function(x) max(x$height_pt), numeric(1))
  linedesc_pt <- vapply(lines, function(x) max(x$descent_pt), numeric(1))
  linespacing_pt <- vapply(lines, function(x) max(x$linespacing_pt), numeric(1))

  maxwidth_pt <- max(linewidths_pt)

  linex_pt <- (maxwidth_pt - linewidths_pt) * hjust + x_pt
  liney_pt <- y_pt - cumsum(linespacing_pt) + linespacing_pt

  tbl <- tibble(line = lines, linex_pt = linex_pt, liney_pt = liney_pt)

  grobs <- unlist(pmap(tbl, render_line), recursive = FALSE)
}
