library(grid)
library(microbenchmark)

microbenchmark(unit(10, "pt"), gridtext:::unit_pt(10))

tg <- function(label, x_pt, y_pt, gp = gpar()) {
  textGrob(label, x = unit(x_pt, "pt"), y = unit(y_pt, "pt"), hjust = 0, vjust = 0, gp = gp, name = "")
}

f <- gridtext:::text_grob

microbenchmark::microbenchmark(
  f("test", 10, 10),
  gridtext:::text_grob("test", 10, 10),
  tg("test", 10, 10)
)
