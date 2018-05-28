margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  structure(unit(c(t, r, b, l), unit), class = c("margin", "unit"))
}


label_grob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       hjust = 0.5, vjust = 0.5, padding = margin(0, 0, 0, 0),
                       margin = margin(0, 0, 0, 0), gp = gpar(), angle = 0, debug = TRUE) {
  text_grob <- textGrob(label, x = hjust, y = vjust, hjust = hjust, vjust = vjust)
  width <- grobWidth(text_grob)
  height <- grobHeight(text_grob)

  # calculate descent based on fixed label string
  temp <- editGrob(text_grob, label = "gjpqyQ")
  # hack to work around grid's limitation of not considering descent correctly
  # for different font sizes
  fontsize <- gp$fontsize %||% 12
  descent <- descentDetails(temp) * (fontsize / 12)

  widths <- unit.c(margin[4], padding[4], width, padding[2], margin[2])
  heights <- unit.c(margin[1], padding[1], height, descent, padding[3], margin[3])

  vp <- viewport(
    x = x, y = y, just = c(hjust, vjust),
    width = sum(widths),
    height = sum(heights),
    angle = angle,
    layout = grid.layout(6, 5, heights = heights, widths = widths),
    gp = gp
  )

  text_grob <- editGrob(
    text_grob,
    vp = viewport(layout.pos.row = 3, layout.pos.col = 3)
  )

  pad_grob <- rectGrob(
    gp = gpar(fill = "azure1", col = "black"),
    vp = viewport(layout.pos.row = c(2, 5), layout.pos.col = c(2, 4))
  )

  marg_grob <- rectGrob(
    gp = gpar(fill = "azure2", col = NA),
    vp = viewport(layout.pos.row = c(1, 6), layout.pos.col = c(1, 5))
  )

  point_grob <- pointsGrob(
    unit(hjust, "npc"), unit(vjust, "npc"), pch = 20, gp = gpar(col = "azure4"),
    vp = viewport(layout.pos.row = c(1, 6), layout.pos.col = c(1, 5))
  )

  if (isTRUE(debug)) {
    children <- gList(
      marg_grob, pad_grob, point_grob, text_grob
    )
  } else {
    children <- gList(text_grob)
  }

  grobTree(
    children,
    vp = vp
  )
}


library(grid)
library(rlang)

grid.newpage()
grid.draw(
  label_grob(
    'testg\ntest2abcqd',
    x = unit(.5, "npc"),
    y = unit(.5, "npc"),
    hjust = 0,
    vjust = 1,
    angle = 0,
    padding = margin(2, 0, 2, 0),
    margin = margin(5, 5, 5, 5),
    gp = gpar()#fontfamily = "Comic Sans MS", fontsize = 20)
  )
)
