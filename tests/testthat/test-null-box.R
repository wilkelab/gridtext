test_that("basic features", {
  # null box with no extent
  nb <- bl_make_null_box()
  expect_identical(bl_box_width(nb), 0)
  expect_identical(bl_box_height(nb), 0)
  expect_identical(bl_box_ascent(nb), 0)
  expect_identical(bl_box_descent(nb), 0)
  expect_identical(bl_box_voff(nb), 0)

  g <- bl_render(nb, 100, 200)
  expect_identical(length(g), 0L)

  # null box with defined extent
  nb <- bl_make_null_box(100, 200)
  expect_identical(bl_box_width(nb), 100)
  expect_identical(bl_box_height(nb), 200)
  expect_identical(bl_box_ascent(nb), 200)
  expect_identical(bl_box_descent(nb), 0)
  expect_identical(bl_box_voff(nb), 0)

  g <- bl_render(nb, 100, 200)
  expect_identical(length(g), 0L)

  # null box transmits its extent to enclosing rect box
  rb <- bl_make_rect_box(
    nb, 0, 0, margin = rep(0, 4), padding = rep(0, 4),
    gp = gpar(), width_policy = "native", height_policy = "native"
  )
  bl_calc_layout(rb, 0, 0)
  g <- bl_render(rb, 100, 200)
  outer <- g[[1]]
  expect_identical(outer$x, unit(100, "pt"))
  expect_identical(outer$y, unit(200, "pt"))
  expect_identical(outer$width, unit(100, "pt"))
  expect_identical(outer$height, unit(200, "pt"))
})
