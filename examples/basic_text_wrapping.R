library(grid)
library(gridtext)

wrap_grob <- function(text, x = unit(0.2, "npc"), y = unit(0.8, "npc"), width = unit(0.5, "npc"),
                      gp = gpar(), linespacing_pt = 14, render_cpp = TRUE) {
  # currently ignores newlines
  tokens <- stringr::str_split(text, "[[:space:]]+")[[1]]

  grobs <- lapply(tokens, gridtext:::text_grob, gp = gp)
  widths_pt <- vapply(grobs, grob_width_pt, numeric(1))
  sp_grob <- gridtext:::text_grob(" ", gp = gp)
  space_width_pt <- grob_width_pt(sp_grob)

  if (isTRUE(render_cpp)) {
    nodes <- mapply(gridtext:::bl_make_grob_box, grobs, widths_pt)
    bl_nodes <- gridtext:::bl_make_node_list(nodes)
    hbox <- gridtext:::bl_make_hbox(bl_nodes, linespacing_pt, space_width_pt)
  } else {
    hbox <- NULL
  }

  gTree(
    x = x,
    y = y,
    width = width,
    grobs = do.call(gList, grobs),
    widths_pt = widths_pt,
    space_width_pt = space_width_pt,
    linespacing_pt = linespacing_pt,
    hbox = hbox,
    render_cpp = render_cpp,
    gp = gp,
    cl = "wrap_grob"
  )
}

makeContent.wrap_grob <- function(x) {
  # get absolute coordinates of the grob
  x_pt <- convertX(x$x, "pt", valueOnly = TRUE)
  y_pt <- convertY(x$y, "pt", valueOnly = TRUE)
  width_pt <- convertWidth(x$width, "pt", valueOnly = TRUE)

  if (isTRUE(x$render_cpp)) {
    print("rendering via C++")
    gridtext:::bl_calc_layout(x$hbox, width_pt)
    children <- gridtext:::bl_render(x$hbox, x_pt, y_pt)
  } else {
    print("rendering via R")
    # x and y offsets as we draw
    x_off <- 0
    y_off <- 0
    children <- x$grobs
    for (i in seq_along(children)) {
      children[[i]]$x = gridtext:::unit_pt(x_pt + x_off)
      children[[i]]$y = gridtext:::unit_pt(y_pt + y_off)
      x_off <- x_off + x$widths_pt[i]
      if (x_off > width_pt) { # start new line
        x_off <- 0
        y_off <- y_off - x$linespacing_pt
      } else { # add space
        x_off <- x_off + x$space_width_pt
      }
    }
  }

  setChildren(x, children)
}

text <- "The quick brown fox jumps over the lazy dog. Lorem ipsum dolor sit amet,
  consectetur adipiscing elit. Aenean rhoncus felis nunc, sed finibus erat mollis
  a. Donec sapien lorem, ornare non dictum a, dictum eu risus. Vivamus eleifend
  ante ut purus rhoncus, sit amet laoreet sapien sodales. Suspendisse porta egestas
  enim. Ut odio ex, bibendum vehicula rhoncus pulvinar, rutrum sed orci. Maecenas
  lobortis sapien in vehicula accumsan. Aenean mauris lacus, finibus et justo ut,
  volutpat placerat augue. Mauris nec ultrices nulla. Vivamus ut dolor accumsan,
  fermentum odio id, facilisis velit."


g1 <- wrap_grob(text, gp = gpar(fontsize = 14), linespacing_pt = 17, render_cpp = FALSE)
g2 <- wrap_grob(
  text,
  x = unit(0.201, "npc"), y = unit(0.801, "npc"),
  gp = gpar(fontsize = 14, col = "red"), linespacing_pt = 17, render_cpp = TRUE
)
grid.newpage()
grid.draw(g1)
grid.draw(g2)
