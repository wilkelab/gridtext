test_that("alignment of content", {
  nb <- bl_make_null_box()
  cb <- bl_make_rect_box(nb, 20, 10, c(0, 0, 0, 0), c(0, 0, 0, 0), gp = gpar())

  rb <- bl_make_rect_box(cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar())
  bl_calc_layout(rb, 0, 0)
  g <- bl_render(rb, 100, 200)

  # placement of outer box depends on margins
  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(400 - 2 - 8, "pt"))
  expect_identical(outer$height, unit(600 - 1 - 4, "pt"))

  # placement of inner box depends on margins, padding, justification, and inner size
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 600 - 1 - 16 - 10, "pt"))

  rb <- bl_make_rect_box(cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
                                    content_hjust = 1, content_vjust = 0)
  bl_calc_layout(rb, 0, 0)
  g <- bl_render(rb, 100, 200)
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 400 - 2 - 32 - 20, "pt"))
  expect_identical(inner$y, unit(200 + 4 + 64, "pt"))
})


test_that("size policies", {
  nb <- bl_make_null_box()
  cb <- bl_make_rect_box(nb, 20, 10, c(0, 0, 0, 0), c(0, 0, 0, 0), gp = gpar())

  rb <- bl_make_rect_box(
    cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "expand", height_policy = "relative"
  )
  bl_calc_layout(rb, 100, 50)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(100 - 2 - 8, "pt"))
  expect_identical(outer$height, unit(300 - 1 - 4, "pt"))
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 300 - 1 - 16 - 10, "pt"))

  rb <- bl_make_rect_box(
    cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "relative", height_policy = "expand"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(200 - 2 - 8, "pt"))
  expect_identical(outer$height, unit(300 - 1 - 4, "pt"))
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 300 - 1 - 16 - 10, "pt"))

  rb <- bl_make_rect_box(
    cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "native", height_policy = "native"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  # native width/height now depends on padding, not on margin
  expect_identical(outer$width, unit(20 + 32 + 128, "pt"))
  expect_identical(outer$height, unit(10 + 16 + 64, "pt"))
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 4 + 64, "pt"))

  rb <- bl_make_rect_box(
    cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "native", height_policy = "relative"
  )
  bl_calc_layout(rb, 50, 50)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(20 + 32 + 128, "pt"))
  expect_identical(outer$height, unit(300 - 1 - 4, "pt"))
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 300 - 1 - 16 - 10, "pt"))

  rb <- bl_make_rect_box(
    cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "native", height_policy = "expand"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(20 + 32 + 128, "pt"))
  expect_identical(outer$height, unit(300 - 1 - 4, "pt"))
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 300 - 1 - 16 - 10, "pt"))

  rb <- bl_make_rect_box(
    cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "native", height_policy = "fixed"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(20 + 32 + 128, "pt"))
  expect_identical(outer$height, unit(600 - 1 - 4, "pt"))
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 600 - 1 - 16 - 10, "pt"))

  rb <- bl_make_rect_box(
    cb, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "fixed", height_policy = "native"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(400 - 2 - 8, "pt"))
  expect_identical(outer$height, unit(10 + 16 + 64, "pt"))
  inner <- g[[2]]
  expect_identical(inner$x, unit(100 + 8 + 128, "pt"))
  expect_identical(inner$y, unit(200 + 4 + 64, "pt"))

  # native size policies with no content
  rb <- bl_make_rect_box(
    NULL, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "native", height_policy = "native"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  # native width/height now depends only on padding, since content size is 0
  expect_identical(outer$width, unit(32 + 128, "pt"))
  expect_identical(outer$height, unit(16 + 64, "pt"))

  rb <- bl_make_rect_box(
    NULL, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "native", height_policy = "fixed"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(32 + 128, "pt"))
  expect_identical(outer$height, unit(600 - 1 - 4, "pt"))

  rb <- bl_make_rect_box(
    NULL, 400, 600, c(1, 2, 4, 8), c(16, 32, 64, 128), gp = gpar(),
    width_policy = "fixed", height_policy = "native"
  )
  bl_calc_layout(rb, 50, 300)
  g <- bl_render(rb, 100, 200)

  outer <- g[[1]]
  expect_identical(outer$x, unit(100 + 8, "pt"))
  expect_identical(outer$y, unit(200 + 4, "pt"))
  expect_identical(outer$width, unit(400 - 2 - 8, "pt"))
  expect_identical(outer$height, unit(16 + 64, "pt"))

})
