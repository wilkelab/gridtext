library(grid)
library(gridtext)

wrap_grob <- function(text, x = unit(0.2, "npc"), y = unit(0.8, "npc"),
                      width = unit(0.5, "npc"), gp = gpar()) {
  # currently ignores newlines
  tokens <- stringr::str_split(text, "[[:space:]]+")[[1]]
  td <- text_details("a", gp = gp)

  gTree(
    x = x,
    y = y,
    width = width,
    tokens = tokens,
    space_width_pt = td$space_pt,
    linespacing_pt = 1.2*(td$ascent_pt + td$descent_pt),
    gp = gp,
    cl = "wrap_grob"
  )
}

makeContent.wrap_grob <- function(x) {
  # get absolute coordinates of the grob
  x_pt <- convertX(x$x, "pt", valueOnly = TRUE)
  y_pt <- convertY(x$y, "pt", valueOnly = TRUE)
  width_pt <- convertWidth(x$width, "pt", valueOnly = TRUE)
  children <- gridtext:::test_par_box(tokens, width_pt, x_pt, y_pt, x$gp)
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


g <- wrap_grob(
  text,
  x = unit(0.2, "npc"), y = unit(0.2, "npc"),
  gp = gpar(fontsize = 14, col = "blue", fill = "cornsilk")
)
grid.newpage()
grid.draw(g)

# benchmark shows layouting via C++ is about 10 times faster
# than just grob generation in regular R

tokens <- stringr::str_split(text, "[[:space:]]+")[[1]]
gp <- gpar(fontfamily = "Times", fontsize = 10, col = "blue")
f_cpp <- function(tokens, gp) {
  gridtext:::test_par_box(tokens, 200, 20, 400, gp)
}

f_R <- function(tokens, gp) {
  grobs <- lapply(tokens, textGrob, gp = gp)
  widths_pt <- vapply(grobs, grob_width_pt, numeric(1))
}

microbenchmark::microbenchmark(f_cpp(tokens, gp), f_R(tokens, gp))

