library(grid)
library(microbenchmark)

microbenchmark(unit(10, "pt"), gridtext:::unit_pt(10))

tg <- function(label, x_pt, y_pt, gp = gpar()) {
  textGrob(label, x = unit(x_pt, "pt"), y = unit(y_pt, "pt"), hjust = 0, vjust = 0, gp = gp)
}

f <- gridtext:::text_grob

microbenchmark::microbenchmark(
  f("test", 10, 10),
  gridtext:::text_grob("test", 10, 10),
  tg("test", 10, 10)
)


text <- "The quick brown fox jumps over the lazy dog. Lorem ipsum dolor sit amet,
  consectetur adipiscing elit. Aenean rhoncus felis nunc, sed finibus erat mollis
  a. Donec sapien lorem, ornare non dictum a, dictum eu risus. Vivamus eleifend
  ante ut purus rhoncus, sit amet laoreet sapien sodales. Suspendisse porta egestas
  enim. Ut odio ex, bibendum vehicula rhoncus pulvinar, rutrum sed orci. Maecenas
  lobortis sapien in vehicula accumsan. Aenean mauris lacus, finibus et justo ut,
  volutpat placerat augue. Mauris nec ultrices nulla. Vivamus ut dolor accumsan,
  fermentum odio id, facilisis velit."

f1 <- function(text) {
  tokens <- stringr::str_split(text, "[[:space:]]+")[[1]]
  grobs <- lapply(tokens, gridtext:::text_grob, gp = gpar())
  grobs
}

f2 <- function(text) {
  tokens <- stringr::str_split(text, "[[:space:]]+")[[1]]
  grobs <- lapply(tokens, grid::textGrob, gp = gpar())
  grobs
}

microbenchmark::microbenchmark(f1(text), f2(text))
