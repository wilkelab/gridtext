test_that("vertical stacking works", {
  nb <- bl_make_null_box()
  rb1 <- bl_make_rect_box(nb, 100, 100, rep(0, 4), rep(0, 4), gp = gpar())
  rb2 <- bl_make_rect_box(nb, 50, 50, rep(10, 4), rep(0, 4), gp = gpar())
  rb3 <- bl_make_rect_box(nb, 50, 10, rep(0, 4), rep(0, 4), gp = gpar(), width_policy = "expand")

  vb <- bl_make_vbox(list(rb1, rb2, rb3), width = 200, hjust = 0, vjust = 0, width_policy = "fixed")

  bl_calc_layout(vb, 0, 0)
  bl_place(vb, 0, 0)

  expect_identical(bl_box_width(vb), 200)
  expect_identical(bl_box_height(vb), 160)
  expect_identical(bl_box_voff(vb), 0)

  g <- bl_render(vb, 200, 100)

  out1 <- g[[1]]
  expect_identical(out1$x, unit(200, "pt"))
  expect_identical(out1$y, unit(100 + 10 + 50, "pt"))

  out2 <- g[[2]]
  expect_identical(out2$x, unit(210, "pt"))
  expect_identical(out2$y, unit(100 + 10 + 10, "pt"))

  out3 <- g[[3]]
  expect_identical(out3$x, unit(200, "pt"))
  expect_identical(out3$y, unit(100, "pt"))
  expect_identical(out3$width, unit(200, "pt"))

  # alternatve hjust, vjust, x, y
  vb <- bl_make_vbox(list(rb1, rb2, rb3), width = 200, hjust = 1, vjust = 1, width_policy = "fixed")

  bl_calc_layout(vb, 0, 0)
  bl_place(vb, 15, 27)

  expect_identical(bl_box_width(vb), 200)
  expect_identical(bl_box_height(vb), 160)

  g <- bl_render(vb, 200, 100)

  out1 <- g[[1]]
  expect_identical(out1$x, unit(15 + 0, "pt"))
  expect_identical(out1$y, unit(27 - 60 + 10 + 50, "pt"))

  out2 <- g[[2]]
  expect_identical(out2$x, unit(15 + 10, "pt"))
  expect_identical(out2$y, unit(27 - 60 + 10 + 10, "pt"))

  out3 <- g[[3]]
  expect_identical(out3$x, unit(15 + 0, "pt"))
  expect_identical(out3$y, unit(27 - 60, "pt"))
  expect_identical(out3$width, unit(200, "pt"))
})


test_that("size policies", {
  nb <- bl_make_null_box()
  rb1 <- bl_make_rect_box(nb, 100, 100, rep(0, 4), rep(0, 4), gp = gpar())
  rb2 <- bl_make_rect_box(nb, 50, 50, rep(10, 4), rep(0, 4), gp = gpar())
  vb <- bl_make_vbox(list(rb1, rb2), width = 200, hjust = 0.5, vjust = 0.5, width_policy = "native")

  bl_calc_layout(vb, 0, 0)
  expect_identical(bl_box_width(vb), 100)
  expect_identical(bl_box_height(vb), 150)

  vb <- bl_make_vbox(list(rb1, rb2), width = 200, hjust = 0.5, vjust = 0.5, width_policy = "relative")

  bl_calc_layout(vb, 70, 0)
  expect_identical(bl_box_width(vb), 140)
  expect_identical(bl_box_height(vb), 150)

  vb <- bl_make_vbox(list(rb1, rb2), width = 200, hjust = 0.5, vjust = 0.5, width_policy = "expand")

  bl_calc_layout(vb, 300, 0)
  expect_identical(bl_box_width(vb), 300)
  expect_identical(bl_box_height(vb), 150)
})

test_that("vertical offset is ignored in vertical stacking", {
  tb1 <- bl_make_text_box("string1", gp = gpar(fontsize = 10))
  tb2 <- bl_make_text_box("string2", gp = gpar(fontsize = 20), voff = -10)
  tb3 <- bl_make_text_box("string2", gp = gpar(fontsize = 20), voff = 0)
  tb4 <- bl_make_text_box("string3", gp = gpar(fontsize = 15))
  vb1 <- bl_make_vbox(list(tb1, tb2, tb4), hjust = 0, vjust = 0)
  bl_calc_layout(vb1, 100, 100)
  bl_place(vb1, 17, 24)
  vb2 <- bl_make_vbox(list(tb1, tb3, tb4), hjust = 0, vjust = 0)
  bl_calc_layout(vb2, 100, 100)
  bl_place(vb2, 17, 24)
  g1 <- bl_render(vb1, 0, 0)
  g2 <- bl_render(vb2, 0, 0)

  extract <- function(x, name) {x[[name]]}

  expect_identical(
    lapply(g1, extract, name = "x"),
    lapply(g2, extract, name = "x")
  )

  expect_identical(
    lapply(g1, extract, name = "y"),
    lapply(g2, extract, name = "y")
  )

  expect_identical(
    lapply(g1, extract, name = "label"),
    lapply(g2, extract, name = "label")
  )
})
