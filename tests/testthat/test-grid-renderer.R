context("grid-renderer")

test_that("basic functioning", {
  r <- grid_renderer()
  g <- grid_renderer_collect_grobs(r)

  # without any grobs rendered, we get an empty list of class gList
  expect_equal(length(g), 0)
  expect_true(inherits(g, "gList"))

  # grobs get added in order
  grid_renderer_text(r, "abcd", 100, 100, gpar())
  grid_renderer_rect(r, 100, 100, 200, 200, gpar())
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 2)
  expect_true(inherits(g, "gList"))
  expect_true(inherits(g[[1]], "text"))
  expect_true(inherits(g[[2]], "rect"))

  # internal state gets reset after calling collect_grobs()
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)
  expect_true(inherits(g, "gList"))
})

test_that("smart rendering of rects", {
  r <- grid_renderer()
  # add normal rect
  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar())
  # add rect with rounded corners
  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(), r = 5)
  # add rect that is invisible, gets removed automatically
  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(col = NA))
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 2)
  expect_true(inherits(g, "gList"))
  expect_true(inherits(g[[1]], "rect"))
  expect_true(inherits(g[[2]], "roundrect"))

  # more extensive testing variations for dropping unneeded rects
  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(lty = 0))
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)

  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(col = NA, lty = 1))
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)

  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(col = "black", lty = 0))
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)

  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(fill = NA, lty = 0))
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)

  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(fill = NA, col = "black", lty = 0))
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)

  grid_renderer_rect(r, 100, 100, 200, 200, gp = gpar(fill = NA, col = NA, lty = 1))
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)
})

test_that("visual tests", {
  draw_grob <- function(g) {
    function() {
      grid.newpage()
      grid.draw(g)
      invisible()
    }
  }

  r <- grid_renderer()
  grid_renderer_text(r, "blue", 10, 400, gp = gpar(col = "blue", fontsize = 12))
  grid_renderer_text(r, "red bold", 20, 380, gp = gpar(col = "red", fontsize = 12, fontface = "bold"))
  grid_renderer_text(r, "roman", 30, 360, gp = gpar(fontsize = 12, fontfamily = "Times"))
  g <- grid_renderer_collect_grobs(r)
  expect_doppelganger("Text in different stylings", draw_grob(g))

  grid_renderer_rect(r, 100, 400, 200, 20, gp = gpar(col = "blue"))
  grid_renderer_rect(r, 100, 200, 300, 30, gp = gpar(fill = "cornsilk"), r = 8)
  grid_renderer_text(r, "text 1, square box blue", 100, 400, gp = gpar(fontsize = 20))
  grid_renderer_text(r, "text 2, rounded box filled", 100, 200, gp = gpar(fontsize = 20))
  g <- grid_renderer_collect_grobs(r)
  expect_doppelganger("Mixing text and boxes", draw_grob(g))

  logo_file <- system.file("extdata", "Rlogo.png", package = "gridtext")
  logo <- png::readPNG(logo_file, native = TRUE)
  width <- ncol(logo)
  height <- nrow(logo)
  grid_renderer_raster(r, logo, 10, 10, width, height)
  g <- grid_renderer_collect_grobs(r)
  expect_doppelganger("Rendering raster data", draw_grob(g))
})


test_that("text details are calculated correctly", {
  gp = gpar(fontsize = 20)
  td <- text_details("abcd", gp)

  td2 <- grid_renderer_text_details("abcd", gp)
  expect_identical(td, td2)
})
