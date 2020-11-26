test_that("image dimensions are used", {
  logo_file <- system.file("extdata", "Rlogo.png", package = "gridtext")
  logo <- png::readPNG(logo_file, native = FALSE)

  # default size policy is native for both height and width
  # dpi = 72.27 turns lengths in pixels to lengths in pt
  rb <- bl_make_raster_box(logo, dpi = 72.27)
  bl_calc_layout(rb, 100, 100)
  bl_place(rb, 30, 5)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(40, "pt"))
  expect_identical(img$y, unit(25, "pt"))
  expect_equal(img$width, unit(ncol(logo), "pt"))
  expect_equal(img$height, unit(nrow(logo), "pt"))

  # test now with raster object
  logo2 <- as.raster(logo)
  rb <- bl_make_raster_box(logo2, dpi = 72.27)
  bl_calc_layout(rb, 100, 100)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_equal(img$width, unit(ncol(logo), "pt"))
  expect_equal(img$height, unit(nrow(logo), "pt"))

  # test now with nativeRaster object
  logo3 <- png::readPNG(logo_file, native = TRUE)
  rb <- bl_make_raster_box(logo3, dpi = 72.27)
  bl_calc_layout(rb, 100, 100)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_equal(img$width, unit(ncol(logo), "pt"))
  expect_equal(img$height, unit(nrow(logo), "pt"))

  # dimensions are reported correctly
  expect_equal(bl_box_width(rb), ncol(logo))
  expect_equal(bl_box_height(rb), nrow(logo))
  expect_equal(bl_box_ascent(rb), nrow(logo))
  expect_identical(bl_box_descent(rb), 0)
  expect_identical(bl_box_voff(rb), 0)

  m <- 1:10
  dim(m) <- 10

  expect_error(
    bl_make_raster_box(m),
    "Cannot extract image dimensions."
  )
})


test_that("size policies, respect_aspect = FALSE", {
  logo_file <- system.file("extdata", "Rlogo.png", package = "gridtext")
  logo <- png::readPNG(logo_file, native = TRUE)

  rb <- bl_make_raster_box(logo, width = 50, height = 80,
                                      width_policy = "fixed", height_policy = "fixed",
                                      respect_aspect = FALSE)
  bl_calc_layout(rb, 200, 100)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_identical(img$width, unit(50, "pt"))
  expect_identical(img$height, unit(80, "pt"))

  rb <- bl_make_raster_box(logo, width = 50, height = 80,
                           width_policy = "relative", height_policy = "expand",
                           respect_aspect = FALSE)
  bl_calc_layout(rb, 200, 100)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_identical(img$width, unit(100, "pt"))
  expect_identical(img$height, unit(100, "pt"))

  rb <- bl_make_raster_box(logo, width = 50, height = 80,
                           width_policy = "expand", height_policy = "relative",
                           respect_aspect = FALSE)
  bl_calc_layout(rb, 200, 100)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_identical(img$width, unit(200, "pt"))
  expect_identical(img$height, unit(80, "pt"))

  rb <- bl_make_raster_box(logo, width = 50, height = 80,
                           width_policy = "fixed", height_policy = "native",
                           respect_aspect = FALSE)
  bl_calc_layout(rb, 200, 100)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_identical(img$width, unit(50, "pt"))
  expect_equal(img$height, unit(50*nrow(logo)/ncol(logo), "pt"))

  rb <- bl_make_raster_box(logo, width = 50, height = 80,
                           width_policy = "native", height_policy = "fixed",
                           respect_aspect = FALSE)
  bl_calc_layout(rb, 200, 100)
  g <- bl_render(rb, 10, 20)

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_equal(img$width, unit(80*ncol(logo)/nrow(logo), "pt"))
  expect_identical(img$height, unit(80, "pt"))
})


test_that("size policies, respect_aspect = TRUE", {
  logo_file <- system.file("extdata", "Rlogo.png", package = "gridtext")
  logo <- png::readPNG(logo_file, native = TRUE)

  rb <- bl_make_raster_box(logo, width = 50, height = 80,
                           width_policy = "fixed", height_policy = "fixed")
  bl_calc_layout(rb, 200, 100)
  g <- bl_render(rb, 10, 20)

  nr <- nrow(logo)
  nc <- ncol(logo)
  img_height <- 50*nr/nc
  yoff <- (80 - img_height)/2

  img <- g[[1]]
  expect_identical(img$x, unit(10, "pt"))
  expect_equal(img$y, unit(20 + yoff, "pt"))
  expect_identical(img$width, unit(50, "pt"))
  expect_equal(img$height, unit(img_height, "pt"))

  rb <- bl_make_raster_box(logo, width = 80, height = 50,
                           width_policy = "fixed", height_policy = "fixed")
  bl_calc_layout(rb, 200, 100)
  g <- bl_render(rb, 10, 20)

  nr <- nrow(logo)
  nc <- ncol(logo)
  img_width <- 50*nc/nr
  xoff <- (80 - img_width)/2

  img <- g[[1]]
  expect_equal(img$x, unit(10 + xoff, "pt"))
  expect_identical(img$y, unit(20, "pt"))
  expect_equal(img$width, unit(img_width, "pt"))
  expect_identical(img$height, unit(50, "pt"))
})
