library(grid)
library(gridtext)

wrap_grob <- function(text, x = unit(0.2, "npc"), y = unit(0.8, "npc"), width = unit(0.5, "npc"),
                      gp = gpar(), render_cpp = TRUE) {
  # currently ignores newlines
  tokens <- stringr::str_split(text, "[[:space:]]+")[[1]]

  grobs <- lapply(tokens, gridtext:::text_grob, gp = gp)
  widths_pt <- vapply(grobs, grob_width_pt, numeric(1))
  sp_grob <- gridtext:::text_grob(" ", gp = gp)
  td <- text_details("a", gp = gp)

  gTree(
    x = x,
    y = y,
    width = width,
    tokens = tokens,
    grobs = do.call(gList, grobs),
    widths_pt = widths_pt,
    space_width_pt = td$space_pt,
    linespacing_pt = 1.2*(td$ascent_pt + td$descent_pt),
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
    children <- gridtext:::test_hbox(tokens, width_pt, x_pt, y_pt, x$gp)
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


g1 <- wrap_grob(text, gp = gpar(fontsize = 14), render_cpp = FALSE)
g2 <- wrap_grob(
  text,
  x = unit(0.2015, "npc"), y = unit(0.8015, "npc"),
  gp = gpar(fontsize = 14, col = "red", fill = "cornsilk"), render_cpp = TRUE
)
grid.newpage()
grid.draw(g1)
grid.draw(g2)

# benchmark shows layouting via C++ is about 10 times faster
# than just grob generation in regular R

tokens <- stringr::str_split(text, "[[:space:]]+")[[1]]
gp <- gpar(fontfamily = "Times", fontsize = 10, col = "blue")
f_cpp <- function(tokens, gp) {
  gridtext:::test_hbox(tokens, 200, 20, 400, gp)
}

f_R <- function(tokens, gp) {
  grobs <- lapply(tokens, textGrob, gp = gp)
  widths_pt <- vapply(grobs, grob_width_pt, numeric(1))
}

microbenchmark::microbenchmark(f_cpp(tokens, gp), f_R(tokens, gp))

